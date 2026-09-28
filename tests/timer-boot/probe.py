"""Read-only ISO boot probe. Run under Nix and the shared build lock.

Reuse a known-good live-test command, but always allocate private outputs.
The missing PIT case models failure, not the N95 chipset.
"""
import argparse
import json
import pathlib
import re
import socket
import subprocess
import tempfile
import time

p = argparse.ArgumentParser()
p.add_argument('command', type=pathlib.Path, nargs='?', help='optional saved live-test command')
p.add_argument('--bios', action='store_true', help='default command uses BIOS live image')
p.add_argument('--without-pit', action='store_true')
p.add_argument('--mask-pic', action='store_true', help='mask IRQ0 at calibration entry using GDB')
p.add_argument('--slow-pit', action='store_true', help='program divisor 65536 instead of 1193')
p.add_argument('--drop-eoi', action='store_true', help='suppress the seventh PIC timer acknowledgement')
p.add_argument('--hpet-replacement', action='store_true', help='inherit a slow HPET IRQ0 replacement')
p.add_argument('--skip-hpet-takeover', action='store_true', help='with replacement injection, bypass its repair')
p.add_argument('--settle', type=float, default=3, help='seconds after PIT setup before capture')
p.add_argument('--expect', choices=['missing', 'masked', 'slow', 'stopped', 'restored'], help='assert the diagnostic outcome')
a = p.parse_args()
if sum([a.without_pit, a.mask_pic, a.slow_pit, a.drop_eoi, a.hpet_replacement]) > 1:
    p.error('select only one injected fault')
if a.skip_hpet_takeover and not a.hpet_replacement:
    p.error('skipping takeover requires HPET replacement injection')
run = pathlib.Path(tempfile.mkdtemp(prefix='timer-boot-'))
root = pathlib.Path(__file__).resolve().parents[2]
if a.command:
    command = json.loads(a.command.read_text())
else:
    image = root / 'kernel' / ('cubit_laptop_usb.img' if a.bios else 'cubit_live_uefi.img')
    command = ['qemu-system-x86_64', '-enable-kvm', '-machine', 'q35',
               '-cpu', 'host', '-smp', '1', '-m', '4G', '-nodefaults',
               '-device', 'VGA', '-device', 'qemu-xhci,id=xhci',
               '-drive', f'file={image},if=none,id=cd,media=cdrom,format=raw,readonly=on',
               '-device', 'usb-bot,id=usbcd,bus=xhci.0,port=1',
               '-device', 'scsi-cd,bus=usbcd.0,lun=0,drive=cd,bootindex=1',
               '-device', 'usb-mouse,bus=xhci.0,port=2',
               '-serial', 'none', '-qmp', 'placeholder', '-display', 'none', '-no-reboot',
               '-audiodev', 'none,id=sound', '-device', 'ich9-intel-hda',
               '-device', 'hda-output,audiodev=sound']
    if not a.bios:
        command += ['-drive', 'if=pflash,format=raw,unit=0,readonly=on,file=/usr/share/OVMF/OVMF_CODE_4M.fd']
command[command.index('-serial') + 1] = f'file:{run}/serial.log'
command[command.index('-qmp') + 1] = f'unix:{run}/qmp.sock,server,nowait'
if a.without_pit:
    command[command.index('-machine') + 1] += ',pit=off'
if a.mask_pic or a.slow_pit or a.drop_eoi or a.hpet_replacement:
    command += ['-S', '-gdb', f'unix:{run}/gdb.sock,server=on,wait=off']
(run / 'command.json').write_text(json.dumps(command, indent=2))
print(run, flush=True)
with (run / 'qemu.log').open('w') as log:
    proc = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT)
    sock = socket.socket(socket.AF_UNIX)
    try:
        deadline = time.monotonic() + 60
        while not (run / 'qmp.sock').exists():
            if proc.poll() is not None or time.monotonic() > deadline:
                raise RuntimeError('QEMU did not start')
            time.sleep(.05)
        while True:
            try:
                sock.connect(str(run / 'qmp.sock'))
                break
            except ConnectionRefusedError:
                if proc.poll() is not None or time.monotonic() > deadline:
                    raise
                time.sleep(.05)
        sock.settimeout(5)
        stream = sock.makefile('rb')
        json.loads(stream.readline())
        seq = 0

        def qmp(op, arguments=None):
            global seq
            seq += 1
            req = dict(execute=op, id=seq)
            if arguments is not None:
                req['arguments'] = arguments
            sock.sendall((json.dumps(req) + '\n').encode())
            while True:
                res = json.loads(stream.readline())
                if res.get('id') == seq:
                    if 'error' in res:
                        raise RuntimeError(res)
                    return res['return']

        qmp('qmp_capabilities')
        debugger = None
        if a.mask_pic or a.slow_pit or a.drop_eoi or a.hpet_replacement:
            # HMP port writes do not reliably alter KVM's in-kernel PIC.
            # Change the argument of CuBit's actual guest OUT instructions.
            if a.mask_pic:
                breakpoint = 'hbreak *x86__out8 if $rdi == 33 && ($rsi & 255) == 250'
                injection = ['set $rsi = 255']
            elif a.slow_pit:
                breakpoint = 'hbreak *x86__out8 if $rdi == 64'
                # A zero low/high reload word means 65536 PIT clocks.
                injection = ['set $rsi = 0', 'continue', 'set $rsi = 0']
            elif a.drop_eoi:
                breakpoint = ('hbreak *x86__out8 if $rdi == 32 && ($rsi & 255) == 32 '
                              '&& *(unsigned long long *)&time__msticks == 6')
                # Replace EOI with OCW3 IRR selection: do not acknowledge.
                injection = ['set $rsi = 10']
            else:
                breakpoint = 'hbreak *boot_timer_setup__quiesce_hpet'
                # The guest has mapped HPET, but has not yet read or changed it.
                # Device-register writes, not forged IRQs or software tick counts.
                injection = [
                    'set $h = $rdi',
                    'set $period = *(unsigned int *)($h + 4)',
                    'set $interval = (unsigned long long)314000000000000 / $period',
                    'set *(unsigned int *)($h + 0x10) = 0',
                    'set *(unsigned long long *)($h + 0xf0) = 0',
                    'set *(unsigned int *)($h + 0x100) = 0x4c',
                    'set *(unsigned long long *)($h + 0x108) = $interval',
                    'set *(unsigned long long *)($h + 0x108) = $interval',
                    'set *(unsigned int *)($h + 0x10) = 3',
                    'printf "HPET injected config=%x period=%u interval=%llu\\n", '
                    '*(unsigned int *)($h+0x10), $period, $interval',
                ]
                if a.skip_hpet_takeover:
                    injection += ['return']
            injection_args = [arg for step in injection for arg in ['-ex', step]]
            with (run / 'gdb.log').open('w') as gdb_log:
                debugger = subprocess.Popen([
                    'gdb', '-q', '-batch', str(root / 'kernel/cubit_kernel'),
                    '-ex', f'target remote {run}/gdb.sock',
                    '-ex', 'set language c', '-ex', breakpoint, '-ex', 'continue',
                    *injection_args, '-ex', 'monitor info lapic', '-ex', 'detach'],
                    stdout=gdb_log, stderr=subprocess.STDOUT)
        observed = None
        while time.monotonic() < deadline and proc.poll() is None:
            serial = run / 'serial.log'
            text = serial.read_text(errors='replace') if serial.exists() else ''
            if 'Setting up PIT' in text and observed is None:
                observed = time.monotonic()
            if observed is not None and time.monotonic() - observed > a.settle:
                break
            time.sleep(.05)
        qmp('stop')
        for name, cmd in [('registers', 'info registers'), ('irq', 'info irq'),
                          ('pic', 'info pic'), ('lapic', 'info lapic')]:
            result = qmp('human-monitor-command', {'command-line': cmd})
            (run / (name + '.txt')).write_text(result)
        qmp('human-monitor-command', {'command-line': f'screendump {run}/boot.ppm'})
        print('Captured stopped guest state; inspect serial and registers.', flush=True)
        if observed is None:
            raise RuntimeError('Guest never reached PIT setup')
        if a.expect:
            text = (run / 'serial.log').read_text(errors='replace')
            if a.hpet_replacement:
                gdb_text = (run / 'gdb.log').read_text(errors='replace')
                if 'HPET injected config=3 ' not in gdb_text:
                    raise RuntimeError('HPET device injection did not take effect')
            if a.expect == 'restored':
                if ('HPET=00000003>00000000' not in text or
                    'APIC timer calibration:' not in text or 'EXCEPTION:' in text):
                    raise RuntimeError('Timer takeover did not restore boot calibration')
                print('PASS: inherited HPET replacement disabled; calibration completed', flush=True)
                # Cleanup still runs through the finally block.
                raise SystemExit(0)
            message = {'missing': 'no counter movement or IRQ0 delivery',
                       'masked': 'IRQ0 masked',
                       'slow': 'incomplete tick delivery',
                       'stopped': 'incomplete tick delivery'}[a.expect]
            if 'EXCEPTION: PIT calibration stalled: ' + message not in text:
                raise RuntimeError('Expected diagnostic was not produced')
            if a.expect != 'missing' and 'PIT moved=Y' not in text:
                raise RuntimeError('Expected a running PIT counter')
            source = re.search(
                r'entries=([0-9A-F]+) returned=([0-9A-F]+) pic0=([0-9A-F]+) isr-or=([0-9A-F]+)',
                text)
            timings = re.search(
                r'gap-min=([0-9A-F]+) gap-max=([0-9A-F]+) handler-max=([0-9A-F]+)',
                text)
            if not source or not timings:
                raise RuntimeError('Missing handler/source evidence')
            entries, completed, pic0, isr_bits = (int(v, 16) for v in source.groups())
            minimum, maximum, handler = (int(v, 16) for v in timings.groups())
            compact = re.search(
                r'IRQ2 n=([0-9A-F]+) ret=([0-9A-F]+) pic=([0-9A-F]+) h=([0-9A-F]+) gap=([0-9A-F]+)',
                text)
            if not compact or tuple(int(v, 16) for v in compact.groups()) != (
                    entries, completed, pic0, handler, maximum):
                raise RuntimeError('Missing or inconsistent top-of-panel IRQ2 summary')
            if len(compact[0]) > 96:
                raise RuntimeError('IRQ2 summary exceeds panel width')
            if entries != completed:
                raise RuntimeError('Boot IRQ probe lost a handler return')
            if a.expect in ('missing', 'masked'):
                if entries or pic0 or isr_bits or minimum or maximum or handler:
                    raise RuntimeError('Unexpected IRQ activity without IRQ0 delivery')
            else:
                if not (entries > 1 and entries == pic0 and isr_bits == 1 and
                        0 < minimum <= maximum and handler > 0):
                    raise RuntimeError('Expected genuine PIC IRQ0 and measured handler intervals')
            print(f'IRQ probe: entries={entries}, pic0={pic0}, gap={minimum}..{maximum}, '
                  f'handler-max={handler} TSC cycles', flush=True)
            if a.hpet_replacement and a.skip_hpet_takeover:
                counts = re.search(r'min=([0-9A-F]+) max=([0-9A-F]+)', text)
                if (not counts or not 0 < int(counts[2], 16) <= 1193 or
                    'mask=0xFA irr=0x00 isr=0x00 IF=1' not in text):
                    raise RuntimeError('Expected normal PIT count range and clean PIC state')
            if a.expect in ('slow', 'stopped'):
                match = re.search(r'span=([0-9A-F]+) first=([0-9A-F]+) last=([0-9A-F]+)', text)
                if not match:
                    raise RuntimeError('Missing timing evidence')
                span, first, last = (int(v, 16) for v in match.groups())
                if not 0 < first <= last <= span:
                    raise RuntimeError('Inconsistent tick timestamps')
                if a.expect == 'slow' and last * 4 < span * 3:
                    raise RuntimeError('Expected ticks throughout the wait')
                if a.expect == 'stopped' and (last * 2 >= span or 'isr=0x01' not in text):
                    raise RuntimeError('Expected an early burst and an unacknowledged PIC IRQ')
            print('PASS: ' + message, flush=True)
    finally:
        sock.close()
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()
        if 'debugger' in globals() and debugger is not None:
            debugger.wait(timeout=5)
