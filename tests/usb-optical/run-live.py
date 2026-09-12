#!/usr/bin/env python3
"""Boot the real USB-only live image; no ATA/NVMe or second initrd fallback.

Retains serial, monitor, screenshots and command line in a fresh /tmp directory.
Run under nix develop. The UI test uses native PS/2 keyboard navigation and a
USB mouse while the optical drive shares its xHCI controller.
"""
import argparse
import json
import pathlib
import re
import socket
import sys
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser()
parser.add_argument('--cpus', type=int, default=1)
parser.add_argument('--timeout', type=int, default=180)
parser.add_argument('--disk-first', action='store_true', help='non-optical LUN 0, CD LUN 1')
parser.add_argument('--mouse-first', action='store_true')
parser.add_argument('--eject', action='store_true', help='test fail-closed media removal after app launch')
parser.add_argument('--early-text', action='store_true', help='check the early text diagnostic boot entry')
parser.add_argument('--sameboy', action='store_true', help='also exercise the native Game Boy frontend')
parser.add_argument('--sameboy-local-rom', action='store_true',
                    help='also launch explicitly staged ROM 01; never supplies or downloads a cartridge')
args = parser.parse_args()
if args.sameboy_local_rom and not args.sameboy:
    parser.error('--sameboy-local-rom requires --sameboy')
root = pathlib.Path(__file__).resolve().parents[2]
image = root / 'kernel/cubit_laptop_usb.iso'
run = pathlib.Path(tempfile.mkdtemp(prefix='cubit-usb-live.', dir='/tmp'))
serial = run / 'serial.log'
monitor = run / 'monitor.sock'
mouse_port, cd_port = (1, 2) if args.mouse_first else (2, 1)
lun = 1 if args.disk_first else 0
command = ['qemu-system-x86_64', '-enable-kvm', '-machine', 'q35', '-cpu', 'host',
           '-smp', str(args.cpus), '-m', '4G', '-nodefaults', '-device', 'VGA',
           '-device', 'qemu-xhci,id=xhci',
           '-drive', f'file={image},if=none,id=cd,media=cdrom,format=raw,readonly=on',
           '-device', f'usb-bot,id=usbcd,bus=xhci.0,port={cd_port}',
           '-device', f'scsi-cd,bus=usbcd.0,lun={lun},drive=cd,bootindex=1',
           '-device', f'usb-mouse,bus=xhci.0,port={mouse_port}',
           '-audiodev', 'none,id=sound', '-device', 'ich9-intel-hda',
           '-device', 'hda-output,audiodev=sound',
           '-serial', f'file:{serial}', '-qmp', f'unix:{monitor},server,nowait',
           '-display', 'none', '-no-reboot']
if args.disk_first:
    disk = run / 'ignored-disk.img'
    with disk.open('wb') as fixture:
        fixture.truncate(32 * 1024 * 1024)
    command += ['-drive', f'file={disk},if=none,id=disk,format=raw,readonly=on',
                '-device', 'scsi-hd,bus=usbcd.0,lun=0,drive=disk']
(run / 'command.json').write_text(json.dumps(command, indent=2))
print(f'USB live test logs: {run}', flush=True)
with (run / 'qemu.log').open('w') as log:
    process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT)
    connection = None
    try:
        deadline = time.monotonic() + args.timeout
        while not monitor.exists() and process.poll() is None:
            if time.monotonic() >= deadline:
                raise RuntimeError('monitor not created')
            time.sleep(0.05)
        connection = socket.socket(socket.AF_UNIX)
        connection.connect(str(monitor))
        connection.settimeout(2)
        stream = connection.makefile('rb')
        json.loads(stream.readline())
        sequence = 0

        def qmp(operation, arguments=None):
            global sequence
            sequence += 1
            request = {'execute': operation, 'id': sequence}
            if arguments is not None:
                request['arguments'] = arguments
            connection.sendall((json.dumps(request) + '\n').encode())
            while True:
                response = json.loads(stream.readline())
                if response.get('id') == sequence:
                    if 'error' in response:
                        raise RuntimeError(response)
                    return response['return']

        qmp('qmp_capabilities')

        def hmp(text):
            return qmp('human-monitor-command', {'command-line': text})

        def wait_for(marker):
            while time.monotonic() < deadline and process.poll() is None:
                text = serial.read_text(errors='replace') if serial.exists() else ''
                if marker in text:
                    return
                if 'EXCEPTION' in text or 'optical transport quarantined' in text:
                    raise RuntimeError('native fault; see serial.log')
                time.sleep(0.2)
            raise RuntimeError(f'timeout waiting for {marker}')

        def key(name):
            hmp(f'sendkey {name}')
            time.sleep(0.2)

        if args.early_text:
            wait_for('GNU GRUB')
            key('down'); key('down'); key('ret')
            wait_for('EARLY: memory initialization complete')
            hmp(f'screendump {run}/early-text.ppm')
            print('EARLY TEXT PASS: kernel checkpoints precede framebuffer allocation.', flush=True)
            sys.exit(0)

        wait_for('desktop: display info ready')
        time.sleep(2)
        hmp(f'screendump {run}/desktop.ppm')
        if args.sameboy:
            key('meta_l')
            for _ in range(6):
                key('down')
            key('ret')
            wait_for('sameboy: loaded ROM 00')
            wait_for('sameboy: 120 emulated frames')
            time.sleep(4)
            key('p')
            hmp(f'screendump {run}/sameboy-before.ppm')
            key('p')
            hmp('sendkey right 500')
            time.sleep(1)
            key('p')
            hmp(f'screendump {run}/sameboy-after.ppm')
            if (run / 'sameboy-before.ppm').read_bytes() == (run / 'sameboy-after.ppm').read_bytes():
                raise RuntimeError('SameBoy framebuffer did not respond to Right input')
            if args.sameboy_local_rom:
                key('f2')
                wait_for('sameboy: loaded ROM 01')
                time.sleep(12)
                hmp(f'screendump {run}/sameboy-local-intro.ppm')
                key('ret')
                time.sleep(2)
                hmp(f'screendump {run}/sameboy-local-start.ppm')
                key('ret')
                time.sleep(2)
                for _ in range(6):
                    key('x')
                    time.sleep(0.5)
                hmp(f'screendump {run}/sameboy-local-game.ppm')
                print('LOCAL CARTRIDGE: loaded ROM 01; inspect intro/start/game screenshots.', flush=True)
            key('esc')
            wait_for('sameboy: clean exit')
            print('SAMEBOY PASS: cartridge read from USB CD, frames and keyboard response, clean exit.', flush=True)
        # Apps menu starts on Console, followed by Workbench and DOOM.
        key('meta_l'); key('down'); key('down'); key('ret')
        wait_for('doom.elf')
        for _ in range(100):
            hmp('mouse_move 2 1')
            hmp('mouse_move -2 -1')
            time.sleep(0.02)
        time.sleep(5)
        hmp(f'screendump {run}/doom-title.ppm')
        key('ret'); key('ret'); key('ret')
        time.sleep(5)
        hmp(f'screendump {run}/doom-game.ppm')
        wait_for('I_InitGraphics: DOOM screen size:')
        key('meta_l'); key('down'); key('ret')
        wait_for('ccl-workbench: native window ready')
        time.sleep(2)
        hmp(f'screendump {run}/workbench.ppm')
        key('meta_l')
        for _ in range(5):
            key('down')
        key('ret')
        wait_for('files: native window ready')
        time.sleep(2)
        hmp(f'screendump {run}/files.ppm')
        if args.eject:
            qmp('eject', {'device': 'cd', 'force': True})
            key('meta_l')
            for _ in range(3):
                key('down')
            key('ret')
            # A fresh Devices launch must need directory/image reads. The
            # storage session is invalidated, but the mouse must remain live.
            removal_deadline = time.monotonic() + 10
            while time.monotonic() < removal_deadline:
                if 'optical transport quarantined' in serial.read_text(errors='replace'):
                    break
                time.sleep(0.1)
            else:
                raise RuntimeError('removed media did not invalidate storage session')
            for index in range(320):
                hmp('mouse_move 2 1' if index % 2 else 'mouse_move -2 -1')
                if index % 32 == 0:
                    hmp('mouse_button 1')
                if index % 32 == 16:
                    hmp('mouse_button 0')
                time.sleep(0.02)
            hmp('mouse_button 0')
            time.sleep(2)
            hmp(f'screendump {run}/media-removed.ppm')
        text = serial.read_text(errors='replace')
        required = ['xhci: optical LUN=', 'FS: native USB ISO9660 apps mounted',
                    'xhci: pre-reset sleep resumed',
                    'devmgr: loaded from filesystem: procmgr.svc',
                    'desktop: display info ready']
        if not all(marker in text for marker in required):
            raise RuntimeError('missing native optical boot evidence')
        if not any(marker in text for marker in
                   ['xhci: no firmware ownership capability',
                    'xhci: firmware ownership acquired=']):
            raise RuntimeError('missing firmware handoff evidence')
        if any(marker in text for marker in ['EXCEPTION', 'PANIC']):
            raise RuntimeError('native fault; see serial.log')
        if not args.eject and 'optical transport quarantined' in text:
            raise RuntimeError('unexpected storage failure')
        if args.eject:
            after = text.split('optical transport quarantined', 1)[1]
            if 'FS Server: ATA driver not registered' in after or 'FS: file not found' in after:
                raise RuntimeError('failed CD lookup incorrectly fell through to writable backends')
            if 'procmgr: OP_OPEN failed' not in after or 'Loaded module devices.app' in after:
                raise RuntimeError('Devices launch did not fail closed after removal')
            counters = re.findall(r'xhci: stats events=(\d+) reports=(\d+) motion=(\d+) buttons=(\d+) errors=(\d+)', after)
            if not counters or int(counters[-1][1]) < 330 or int(counters[-1][3]) < 8 or int(counters[-1][4]) != 0:
                raise RuntimeError('mouse did not remain healthy after optical removal')
            print('USB MEDIA REMOVAL PASS: storage fails closed, HID remains live.', flush=True)
        print('USB LIVE BOOT PASS; inspect DOOM screenshots for rendering/gameplay.', flush=True)
    finally:
        if connection is not None:
            connection.close()
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
