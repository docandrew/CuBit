"""Run a built output-retirement fixture inside Nix under build.lock.

Uses a Desktop image override and private headless script. Never stages binaries.
The scaled observer gets a separate QMP channel for deterministic real input.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import socket
import subprocess
import sys
import threading
import time
import output_retirement_fixture as oracle
import async_lease_fixture as lease_oracle

ROOT = Path(__file__).resolve().parents[2]


class Monitor:
    def __init__(self, address):
        self.socket = socket.socket(socket.AF_UNIX)
        self.socket.settimeout(3)
        self.socket.connect(address)
        self.stream = self.socket.makefile('rwb', buffering=0)
        if 'QMP' not in json.loads(self.stream.readline()):
            raise RuntimeError('missing QMP greeting')
        self.command('qmp_capabilities')

    def command(self, name, arguments=None):
        request = {'execute': name}
        if arguments is not None:
            request['arguments'] = arguments
        self.stream.write((json.dumps(request) + '\n').encode())
        while True:
            reply = json.loads(self.stream.readline())
            if 'error' in reply:
                raise RuntimeError(reply)
            if 'return' in reply:
                return reply['return']

    def close(self):
        self.stream.close()
        self.socket.close()


def observe(fixture, mode, serial, address, timeout, async_lease=False):
    check = lease_oracle.check if async_lease else oracle.check
    log = Path(serial)
    deadline = time.monotonic() + float(timeout)
    while not Path(address).exists():
        if time.monotonic() >= deadline:
            raise RuntimeError('QMP creation deadline')
        time.sleep(.05)
    if mode == 'shutdown':
        monitor = Monitor(address)
        try:
            while True:
                text = log.read_text(errors='replace') if log.exists() else ''
                if re.search(r'OUTPUT-(?:DRAIN|SHUTDOWN): FAIL', text):
                    raise RuntimeError('native shutdown assertion failed')
                try:
                    check(text, mode)
                    break
                except ValueError:
                    if time.monotonic() >= deadline:
                        raise RuntimeError('shutdown acceptance deadline')
                time.sleep(.05)
            print('PASS native shutdown: delayed presentation and grants, zero storage, loop and process exit', flush=True)
            monitor.command('quit')
        finally:
            monitor.close()
        return

    stop = threading.Event()
    injection = {}
    def inject():
        try:
            while not stop.wait(.02):
                if log.exists() and ('LEASE-NATIVE: both replies held' if async_lease else 'OUTPUT-DRAIN: starting with a real queued frame') in log.read_text(errors='replace'):
                    break
            else:
                return
            monitor = Monitor(address + '.injection')
            try:
                for down in (True, False):
                    monitor.command('input-send-event', {'events': [{'type': 'key', 'data': {
                        'down': down, 'key': {'type': 'qcode', 'data': 'shift_r'}}}]})
                injection['sent'] = True
            finally:
                monitor.close()
        except BaseException as error:
            injection['error'] = repr(error)
        finally:
            (fixture / 'injection.json').write_text(json.dumps(injection, indent=2) + '\n')
    worker = threading.Thread(target=inject, daemon=True) if mode == 'scaled' else None
    if worker:
        worker.start()
    try:
        subprocess.run([sys.executable, ROOT / 'tests/headless/check-dual-desktop.py', serial, address, timeout], check=True)
        check(log.read_text(errors='replace'), mode)
        if mode == 'scaled' and not injection.get('sent'):
            raise RuntimeError(('input injection failed', injection))
        monitor = Monitor(address)
        try:
            monitor.command('quit')
        finally:
            monitor.close()
    finally:
        stop.set()
        if worker:
            worker.join(timeout=4)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('fixture', type=Path)
    parser.add_argument('--observe', nargs=3, metavar=('SERIAL', 'SOCKET', 'TIMEOUT'))
    args = parser.parse_args()
    fixture = args.fixture.resolve()
    built = json.loads((fixture / 'result.json').read_text())
    async_lease = built.get('mode', '').startswith('lease-')
    mode = built.get('mode', '').removeprefix('lease-' if async_lease else 'output-')
    check = lease_oracle.check if async_lease else oracle.check
    if built.get('status') != 'BUILT' or mode not in ('scaled', 'shutdown', 'partial'):
        raise RuntimeError('unsupported or unbuilt fixture')
    binary = fixture / 'desktop.svc'
    digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    if digest(binary) != built['binary_sha256']:
        raise RuntimeError('fixture binary hash mismatch')
    if args.observe:
        observe(fixture, mode, *args.observe, async_lease=async_lease)
        return
    if any((fixture / name).exists() for name in ('serial.log', 'boot.log', 'headless.sh', 'boot-result.json')):
        raise RuntimeError('refusing to overwrite native evidence')
    runner = ROOT / 'tests/headless/run.sh'
    script = runner.read_text()
    def replace(old, new):
        nonlocal script
        if script.count(old) != 1:
            raise RuntimeError('runner anchor changed: ' + old)
        script = script.replace(old, new)
    replace('ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"', 'ROOT_DIR=' + shlex.quote(str(ROOT)))
    replace('python3 "$ROOT_DIR/tests/headless/$observer"',
            'python3 ' + shlex.quote(str(Path(__file__).resolve())) + ' ' + shlex.quote(str(fixture)) + ' --observe')
    if mode == 'scaled':
        replace('    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")\n    if [ "$TEST_NAME" = "desktop-dual-output" ]; then',
                '    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off" -qmp "unix:$QMP_SOCKET.injection,server=on,wait=off")\n    if [ "$TEST_NAME" = "desktop-dual-output" ]; then')
    if mode != 'scaled':
        old = 'desktop: active outputs= 2 primary= 0\ndesktop: internal shell active\ndesktop: asynchronous frame released\nccl-workbench: native window ready'
        new = ('desktop: active outputs= 2 primary= 0\ndesktop: internal shell active\nOUTPUT-DRAIN: PASS drained\nOUTPUT-SHUTDOWN: PASS drained loop exited' if mode == 'shutdown' else
               'desktop: active outputs= 2 primary= 0\nOUTPUT-PARTIAL: PASS recovered internal session\ndesktop: asynchronous frame released\nccl-workbench: native window ready')
        replace(old, new)
    (fixture / 'headless.sh').write_text(script)
    before = {str(path): digest(path) for path in (binary, ROOT / 'kernel/isodir/boot/desktop.svc', ROOT / 'kernel/isodir/boot/display.svc')}
    result = {'status': 'INCOMPLETE', 'mode': mode, 'async_lease': async_lease, 'inputs': before, 'runner_sha256': digest(runner)}
    env = {**os.environ, 'CUBIT_DESKTOP_IMAGE': str(binary), 'CUBIT_TEST_MIXED_OUTPUTS': '1',
           'CUBIT_TEST_PRIMARY': '1' if mode == 'scaled' else '0',
           'CUBIT_TEST_SCALING': '1' if mode == 'scaled' else '0',
           'CUBIT_TEST_ARRANGEMENT': '1' if mode == 'scaled' else '0', 'CUBIT_TEST_SETTLE_SECONDS': '2'}
    try:
        with (fixture / 'boot.log').open('w') as output:
            run = subprocess.run(['bash', str(fixture / 'headless.sh'), '--test', 'desktop-dual-output',
                                  '--accel', 'tcg,thread=multi', '--cpus', '4', '--timeout',
                                  '300' if mode == 'scaled' else '120', '--serial', str(fixture / 'serial.log')],
                                 cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT)
        result['headless_status'] = run.returncode
        if run.returncode:
            raise RuntimeError('headless fixture failed')
        check((fixture / 'serial.log').read_text(errors='replace'), mode)
        groups = ('primary', 'scaling', 'arrangement', 'Desktop') if mode == 'scaled' else ('shutdown',) if mode == 'shutdown' else ('Desktop',)
        log = (fixture / 'boot.log').read_text()
        if not all('PASS native ' + group + ':' in log for group in groups):
            raise RuntimeError('missing interaction acceptance')
        result['status'] = 'PASS'
    except BaseException as error:
        result.update(status='FAIL', error=repr(error))
        raise
    finally:
        result['staging_unchanged'] = all(digest(Path(path)) == value for path, value in before.items())
        (fixture / 'boot-result.json').write_text(json.dumps(result, indent=2) + '\n')
        if not result['staging_unchanged']:
            raise RuntimeError('staging or fixture changed')
    print('PASS native output retirement:', mode, fixture)


if __name__ == '__main__':
    main()
