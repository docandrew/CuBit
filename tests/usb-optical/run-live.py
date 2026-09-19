#!/usr/bin/env python3
"""Boot the real USB-only live image; no ATA/NVMe or second initrd fallback.

Retains serial, monitor, screenshots and command line in a fresh /tmp directory.
Run under nix develop. The UI test uses native PS/2 keyboard navigation and a
USB mouse while the optical drive shares its xHCI controller.
"""
import argparse
import array
import json
import math
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
parser.add_argument('--without-audio', action='store_true',
                    help='omit HDA hardware and verify optional audio cannot block boot')
parser.add_argument('--sameboy', action='store_true', help='also exercise the native Game Boy frontend')
parser.add_argument('--settings', action='store_true', help='exercise Settings and live shared-toolkit theme changes')
parser.add_argument('--ccl-ui-hooks', action='store_true', help='exercise native REPL clock, label hooks and retained button callback')
parser.add_argument('--ccl-samples', action='store_true', help='open an ISO-seeded workspace sample and invoke its button')
parser.add_argument('--sameboy-audio', action='store_true',
                    help='capture the original test ROM tone and verify volume, mute and pause')
parser.add_argument('--taskbar', action='store_true',
                    help='exercise native master volume and wallpaper restoration (requires SameBoy audio)')
parser.add_argument('--sameboy-local-rom', action='store_true',
                    help='also launch explicitly staged ROM 01; never supplies or downloads a cartridge')
parser.add_argument('--sameboy-second-local-rom', action='store_true',
                    help='also exercise explicitly staged ROM 02')
args = parser.parse_args()
if args.taskbar and not args.sameboy_audio:
    parser.error('--taskbar requires --sameboy-audio')
if args.sameboy_audio and (not args.sameboy or args.without_audio):
    parser.error('--sameboy-audio requires --sameboy and HDA hardware')
if args.sameboy_local_rom and not args.sameboy:
    parser.error('--sameboy-local-rom requires --sameboy')
if args.sameboy_second_local_rom and not args.sameboy_local_rom:
    parser.error('--sameboy-second-local-rom requires --sameboy-local-rom')
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
           '-serial', f'file:{serial}', '-qmp', f'unix:{monitor},server,nowait',
           '-display', 'none', '-no-reboot']
if args.taskbar:
    command += ['-rtc', 'base=2026-07-01T12:34:00,clock=vm']
if not args.without_audio:
    backend = (f'wav,id=sound,path={run}/audio.wav,out.frequency=48000'
               if args.sameboy_audio else 'none,id=sound')
    command += ['-audiodev', backend, '-device', 'ich9-intel-hda',
                '-device', 'hda-output,audiodev=sound']
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
            if process.poll() is None:
                for name, command in [('registers', 'info registers'),
                                      ('interrupts', 'info irq'),
                                      ('lapic', 'info lapic')]:
                    (run / f'{name}.txt').write_text(hmp(command))
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
        if args.taskbar or args.ccl_ui_hooks or args.ccl_samples:
            from PIL import Image, ImageChops
            width, height = Image.open(run / 'desktop.ppm').size

            def move_relative(dx, dy):
                while dx or dy:
                    x = max(-80, min(80, dx))
                    y = max(-80, min(80, dy))
                    hmp(f'mouse_move {x} {y}')
                    dx -= x; dy -= y
                    time.sleep(0.04)

            def move_to(x, y):
                move_relative(-width * 2, -height * 2)
                move_relative(x, y)
                time.sleep(0.15)

            def click():
                hmp('mouse_button 1'); time.sleep(0.15)
                hmp('mouse_button 0'); time.sleep(0.25)

            def screenshot(name):
                time.sleep(0.3)
                path = run / f'{name}.ppm'
                hmp(f'screendump {path}')
                return Image.open(path).convert('RGB')

        if args.taskbar:
            wait_for('clock: RTC-derived UTC ready')
            move_to(0, 0)
            original = screenshot('wallpaper-clean')
            move_to(width - 130, height - 18); click()
            screenshot('master-popup')
            key('esc')
            move_to(0, 0)
            restored = screenshot('wallpaper-restored')
            # The clock may tick during a slow run; compare desktop artwork,
            # excluding only the taskbar. Menu/cursor damage must be exact.
            diff = ImageChops.difference(original, restored).crop((0, 0, width, height - 36))
            if diff.getbbox() is not None:
                diff.save(run / 'wallpaper-damage.png')
                raise RuntimeError('popup/cursor left wallpaper damage')
            print('WALLPAPER DAMAGE PASS: popup and pointer restore exact background.', flush=True)
        if args.sameboy:
            key('meta_l')
            for _ in range(6):
                key('down')
            key('ret')
            wait_for('sameboy: loaded ROM 00')
            if args.without_audio:
                wait_for('sameboy: mixer unavailable; continuing silently')
            wait_for('sameboy: 120 emulated frames')
            if args.sameboy_audio:
                wait_for('sameboy: native audio started')
                measurements = {}

                def measure(name):
                    # File offsets measure actual captured PCM, not guest/host
                    # clock synchronization. Trim transitions and backend buffering.
                    time.sleep(0.5)
                    path = run / 'audio.wav'
                    begin = path.stat().st_size
                    time.sleep(2)
                    end = path.stat().st_size
                    payload = path.read_bytes()[begin:end]
                    if name == 'paused' and not payload:
                        # Stopping the final stream stops HDA and QEMU's WAV
                        # producer, rather than writing endless silent frames.
                        measurements[name] = 0.0
                        return
                    samples = array.array('h', payload[:len(payload)//4*4])
                    if sys.byteorder != 'little':
                        samples.byteswap()
                    samples = samples[::2][4800:-4800]
                    if len(samples) < 48000:
                        raise RuntimeError(f'{name}: missing captured audio frames')
                    measurements[name] = math.sqrt(sum(x*x for x in samples)/len(samples))

                measure('volume_70')
                if args.taskbar:
                    # Ten system-volume key presses must halve final PCM,
                    # without changing the application's own 70% stream gain.
                    for _ in range(10): key('volumedown')
                    measure('master_50')
                    key('audiomute'); measure('master_muted')
                    key('audiomute')
                    for _ in range(10): key('volumeup')
                    measure('master_restored')
                    base = measurements['volume_70']
                    if not 0.45 < measurements['master_50'] / base < 0.55:
                        raise RuntimeError('master-volume keys did not halve PCM')
                    if measurements['master_muted'] > 1:
                        raise RuntimeError('master mute did not silence PCM')
                    if not 0.9 < measurements['master_restored'] / base < 1.1:
                        raise RuntimeError('master volume did not recover')
                    # Also exercise the actual taskbar slider, not just keys.
                    move_to(width - 130, height - 18); click()
                    move_to(width - 128, height - 102); click()
                    measure('master_slider_50')
                    if not 0.43 < measurements['master_slider_50'] / base < 0.57:
                        raise RuntimeError('taskbar slider did not change mixer gain')
                    screenshot('master-slider-50')
                    key('esc')
                    for _ in range(11): key('volumeup')
                    print('MASTER AUDIO PASS: media keys, mute and native popup slider.', flush=True)
                for _ in range(7):
                    key('f9')
                wait_for('sameboy: volume=35 mute=0')
                measure('volume_35')
                key('f8')
                wait_for('sameboy: volume=35 mute=1')
                measure('muted')
                key('f8')
                measure('unmuted_35')
                for _ in range(7):
                    key('f10')
                measure('restored_70')
                key('p')
                measure('paused')
                key('p')
                measure('resumed_70')
                (run / 'sameboy-audio.json').write_text(json.dumps(measurements, indent=2))
                baseline = measurements['volume_70']
                if baseline < 100:
                    raise RuntimeError('SameBoy test tone is silent')
                for name in ('volume_35', 'unmuted_35'):
                    if not 0.45 < measurements[name]/baseline < 0.55:
                        raise RuntimeError(f'{name}: volume scaling failed')
                for name in ('muted', 'paused'):
                    if measurements[name] > 1:
                        raise RuntimeError(f'{name}: expected silence')
                for name in ('restored_70', 'resumed_70'):
                    if not 0.9 < measurements[name]/baseline < 1.1:
                        raise RuntimeError(f'{name}: playback did not recover')
                if 'audio stalled/overflowed' in serial.read_text(errors='replace'):
                    raise RuntimeError('SameBoy audio fell back to silence')
                print('SAMEBOY AUDIO PASS: tone, volume ratio, mute, pause and resume.', flush=True)
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
                if args.sameboy_second_local_rom:
                    key('f2')
                    wait_for('sameboy: loaded ROM 02')
                    time.sleep(5)
                    hmp(f'screendump {run}/sameboy-second-local.ppm')
                    key('ret')
                    time.sleep(3)
                    hmp(f'screendump {run}/sameboy-second-local-start.ppm')
                    print('LOCAL CARTRIDGE: loaded ROM 02; inspect screenshots.', flush=True)
            if args.sameboy_audio and not args.sameboy_local_rom:
                key('p')  # Exit from running audio, not an already closed pause.
                time.sleep(1)
            key('esc')
            wait_for('sameboy: clean exit')
            if args.sameboy_audio:
                time.sleep(0.5)
                stopped_size = (run / 'audio.wav').stat().st_size
                time.sleep(1)
                if (run / 'audio.wav').stat().st_size != stopped_size:
                    raise RuntimeError('SameBoy exit left the audio hardware running')
                print('SAMEBOY AUDIO CLOSE PASS: final stream stopped hardware.', flush=True)
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
        if args.ccl_samples:
            key('ctrl-o')
            screenshot('ccl-sample-picker')
            # The demo workspace places button-clock first. No source typing
            # or mock file reads: native directory enumeration and FS IPC.
            key('ret')
            wait_for('ccl-workbench: workspace opened button-clock.ccl')
            screenshot('ccl-sample-loaded')
            key('f5')
            registered = screenshot('ccl-sample-registered')
            move_to(540, 162); click(); move_to(0, 0)
            clicked = screenshot('ccl-sample-clicked')
            label_bounds = (610, 149, 1000, 176)
            if ImageChops.difference(registered.crop(label_bounds),
                                     clicked.crop(label_bounds)).getbbox() is None:
                raise RuntimeError('loaded clock sample did not update its label on click')
            print('CCL SAMPLE PASS: file-picker load from live workspace, registration and clock callback.', flush=True)
            # Close registration before other optional callback tests.
            move_to(322, 162); click(); move_to(0, 0)
        if args.ccl_ui_hooks:
            key('f6')
            def repl_command(source):
                for ch in source:
                    key({'(': 'shift-9', ')': 'shift-0', '.': 'dot',
                         '-': 'minus', ' ': 'spc', '"': 'shift-apostrophe'}.get(
                             ch, 'shift-' + ch.lower() if ch.isupper() else ch))
                key('ret')
                time.sleep(1)
            repl_command('(ui.label-value 42)')
            hmp(f'screendump {run}/ccl-label.ppm')
            repl_command('(ui.label-visible false)')
            repl_command('(clock.monotonic-ms)')
            hmp(f'screendump {run}/ccl-clock-repl.ppm')
            repl_command('(define (greet (name String)) String (concat "hello " name)) '
                         '(ui.label-text (greet "cubit"))')
            hmp(f'screendump {run}/ccl-text-label.ppm')
            if serial.read_text(errors='replace').count('ccl-workbench: REPL completed') < 4:
                raise RuntimeError('native REPL did not complete all four submissions')
            repl_command('(define (clicked) Boolean (ui.label-text "Clicked!")) '
                         '(ui.button-on-click (handler clicked))')
            registered = screenshot('ccl-button-registered')
            # Workbench's native client origin is (120, 124); the shared
            # button is at client (366, 25); Desktop owns native title chrome.
            move_to(540, 162)
            click()
            move_to(0, 0)
            clicked = screenshot('ccl-button-clicked')
            label_bounds = (610, 149, 1000, 176)
            if ImageChops.difference(registered.crop(label_bounds),
                                     clicked.crop(label_bounds)).getbbox() is None:
                raise RuntimeError('native CCL button click did not change its label')
            print('CCL BUTTON PASS: native input dispatched retained CCL and repainted its label.', flush=True)
        key('meta_l')
        for _ in range(5):
            key('down')
        key('ret')
        wait_for('files: native window ready')
        time.sleep(2)
        hmp(f'screendump {run}/files.ppm')
        if args.settings:
            key('meta_l'); key('up'); key('ret')
            time.sleep(1)
            hmp(f'screendump {run}/settings-light.ppm')
            key('tab'); key('ret')  # Select Alloy Dark, still only a preview.
            for _ in range(8): key('tab')
            key('ret')
            wait_for('desktop: appearance applied')
            time.sleep(2)
            hmp(f'screendump {run}/settings-dark.ppm')
            from PIL import Image
            light = Image.open(run / 'settings-light.ppm').convert('RGB')
            dark = Image.open(run / 'settings-dark.ppm').convert('RGB')
            # Shared Alloy taskbar face changes, not just the Settings preview.
            point = (light.width // 2, light.height - 2)
            if light.getpixel(point) == dark.getpixel(point):
                raise RuntimeError('Settings Apply did not change desktop theme')
            if dark.getpixel(point) != (48, 58, 66):
                raise RuntimeError(f'wrong dark taskbar color: {dark.getpixel(point)}')
            if dark.getpixel((800, 600)) != (35, 44, 51):
                raise RuntimeError('existing Files client did not repaint with the dark palette')
            if serial.read_text(errors='replace').count('desktop: CCL theme loaded') < 4:
                raise RuntimeError('missing startup and Apply CCL theme-loading evidence')
            # Apply currently has focus. Select Cubie, then apply it live.
            for _ in range(6): key('shift-tab')
            key('ret')
            for _ in range(6): key('tab')
            key('ret')
            time.sleep(2)
            hmp(f'screendump {run}/settings-cubie.ppm')
            cubie = Image.open(run / 'settings-cubie.ppm').convert('RGB')
            background = (dark.width - 10, 10)
            if cubie.getpixel(background) == dark.getpixel(background):
                raise RuntimeError('Cubie Apply did not change the wallpaper')
            if cubie.getpixel(point) != dark.getpixel(point):
                raise RuntimeError('wallpaper change unexpectedly changed the theme')
            # Reopen an already-running Workbench to inspect its repainted UI.
            key('meta_l'); key('down'); key('ret')
            time.sleep(2)
            hmp(f'screendump {run}/workbench-dark.ppm')
            print('SETTINGS PASS: keyboard selection, Apply, live palette and Cubie wallpaper.', flush=True)
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
        if args.without_audio:
            # Without a device capability, sysinfo may itself deny the
            # physical-address query; mapping rejection is also a safe exit.
            if not any(marker in text for marker in (
                    'hda: missing controller/DMA allocation',
                    'hda: controller mapping failed')):
                raise RuntimeError('missing HDA absence diagnostic')
            if 'hda: no usable output, exiting' not in text:
                raise RuntimeError('missing HDA failure handshake')
            if 'hda: registered, entering service loop' in text:
                raise RuntimeError('absent HDA was incorrectly registered')
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
