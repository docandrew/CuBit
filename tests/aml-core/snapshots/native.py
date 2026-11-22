#!/usr/bin/env python3
"""Inspect an unmodified kernel's sealed ACPI cache in private QEMU guests.

This exercises boot discovery/capture, not service launch or grant transport.
Requires Nix QEMU/GRUB/GDB; no disk image or physical machine is attached.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser()
parser.add_argument('--kernel', type=Path, required=True)
parser.add_argument('--fault', choices=('none', 'table-limit', 'late-table', 'allocation'), default='none')
parser.add_argument('--large-inventory', action='store_true')
parser.add_argument('--accel', choices=('tcg', 'kvm'), default='tcg')
parser.add_argument('--protocol', choices=('both', 'multiboot', 'multiboot2'), default='both')
args = parser.parse_args()
kernel = args.kernel.resolve(strict=True)
kernel_hash = hashlib.sha256(kernel.read_bytes()).hexdigest()
run = Path(tempfile.mkdtemp(prefix='cubit-acpi-inspect-', dir='/tmp'))
print(f'ACPI boot artifacts: {run}', flush=True)
extra = []
if args.large_inventory:
    raw = bytearray(40000)
    raw[:4] = b'SSDT'
    raw[4:8] = len(raw).to_bytes(4, 'little')
    raw[8] = 2
    raw[10:16] = b'CUBIT '
    raw[16:24] = b'SIZETEST'
    raw[9] = -sum(raw) & 255
    table = run / 'large-ssdt.bin'
    table.write_bytes(raw)
    extra = ['-acpitable', f'file={table}'] * 33
protocols = ('multiboot', 'multiboot2') if args.protocol == 'both' else (args.protocol,)
for protocol in protocols:
    case = run / protocol
    stage = case / 'iso'
    (stage / 'boot/grub').mkdir(parents=True)
    shutil.copyfile(kernel, stage / 'boot/cubit_kernel')
    assert hashlib.sha256((stage / 'boot/cubit_kernel').read_bytes()).hexdigest() == kernel_hash
    (stage / 'boot/grub/grub.cfg').write_text(
        'set timeout=0\nset default=0\nterminal_output console\n'
        f'menuentry "ACPI snapshot" {{\n {protocol} /boot/cubit_kernel\n'
        ' set gfxpayload=text\n boot\n}\n')
    with (case / 'iso.log').open('w') as log:
        subprocess.run(['grub-mkrescue', '-o', str(case / 'boot.iso'), str(stage)],
                       stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    debug = case / 'gdb.sock'
    serial = case / 'serial.log'
    report = case / 'report.json'
    script = case / 'inspect.gdb'
    script.write_text('set pagination off\nset confirm off\nset language c\n'
        'set max-value-size unlimited\n'
        f'file {kernel}\ntarget remote {debug}\nhbreak acpi__setup\ncontinue\nfinish\n'
        'python\n'
        'import gdb, json, hashlib\n'
        'v = gdb.parse_and_eval("acpi__boot_snapshot").dereference()\n'
        'inferior = gdb.selected_inferior()\n'
        'assert int(gdb.parse_and_eval("$rax")) & 255 == 1, "ACPI setup failed"\n'
        'assert int(v["mode"]) == 2, "snapshot not Ready"\n'
        'count = int(v["used"])\n'
        'assert 1 <= count <= 256 and count == int(v["required"])\n'
        'tables = v["tables"]\n'
        'lo, hi = tables.type.range()\n'
        'assert lo == 1 and hi == count\n'
        'backing = int(v["data"].address)\n'
        'results = []\n'
        'total = 0\n'
        'for i in range(1, count + 1):\n'
        '    t = tables[i]\n'
        '    d = t["description"]\n'
        '    n = int(d["extent"])\n'
        '    assert 36 <= n <= int(v["table_byte_limit"])\n'
        '    offset = int(t["offset"])\n'
        '    assert offset == total, "payload is not packed"\n'
        '    data = bytes(inferior.read_memory(backing + offset, n))\n'
        '    name = bytes(inferior.read_memory(int(d["name"].address), 4))\n'
        '    assert data[:4] == name and name != b"FACS"\n'
        '    assert (name == b"DSDT") == (i == 1)\n'
        '    assert int.from_bytes(data[4:8], "little") == n\n'
        '    assert data[8] == int(d["revision"])\n'
        '    assert sum(data[:n]) % 256 == 0, "checksum"\n'
        '    original = int(d["physical"]) + 0xffff800000000000\n'
        '    assert backing + offset != original, "source alias"\n'
        '    assert bytes(inferior.read_memory(original, n)) == data[:n], "copy differs"\n'
        '    total += n\n'
        '    results.append(dict(index=i, signature=name.decode("ascii"), length=n, sha256=hashlib.sha256(data[:n]).hexdigest()))\n'
        'assert total == int(v["total"]) and total == int(v["byte_capacity"])\n'
        'assert max(t["length"] for t in results) == int(v["table_byte_limit"])\n'
        'assert int(v["table_capacity"]) == count\n'
        + ('assert total > 1024 * 1024 and sum(t["length"] == 40000 for t in results) == 33 and count > 32, "large inventory missing"\n' if args.large_inventory else '')
        +
        'pool = gdb.parse_and_eval("acpi__snapshot_pool")\n'
        'padding = int(pool["base"]) + int(pool["capacity"]) - backing - total\n'
        'assert padding >= 0\n'
        'if padding: assert not any(bytes(inferior.read_memory(backing + total, padding))), "dirty allocator padding"\n'
        f'with open({str(report)!r}, "w") as out:\n'
        '    json.dump(dict(status="PASS", published=True, count=count, bytes=total, tables=results), out, indent=2)\n'
        'print("ACPI-SNAPSHOT-BOOT: PASS", count, total)\n'
        'end\ndetach\nquit\n')
    if args.fault != 'none':
        hook = ('firmware_tables__snapshots__begin_snapshot'
                if args.fault == 'table-limit' else 'firmware_tables__snapshots__append')
        mutation = ('set var tables = 257\n' if args.fault == 'table-limit'
                    else 'set var expected.extent = 2147483647\n')
        script.write_text('set pagination off\nset confirm off\nset language c\n'
            'set max-value-size unlimited\n'
            f'file {kernel}\ntarget remote {debug}\nhbreak {hook}\n'
            + ('ignore 1 1\n' if args.fault == 'late-table' else '')
            + 'continue\n' + mutation
            + 'python\nimport gdb\nf = gdb.newest_frame()\n'
            'while f is not None and (f.name() or "").replace("__", ".") != "acpi.setup":\n'
            '    f = f.older()\n'
            'assert f is not None, "setup frame missing"\nf.select()\nend\nfinish\n'
            'python\nimport gdb, json\n'
            'v = gdb.parse_and_eval("acpi__boot_snapshot").dereference()\n'
            'assert int(gdb.parse_and_eval("$rax")) & 255 == 1, "essential ACPI failed"\n'
            'assert int(v["mode"]) == 3, "failed inventory became visible"\n'
            f'assert int(v["used"]) == {1 if args.fault == "late-table" else 0}\n'
            f'with open({str(report)!r}, "w") as out:\n'
            f'    json.dump(dict(status="PASS", fault={args.fault!r}, published=False, count=int(v["used"]), bytes=int(v["total"])), out, indent=2)\n'
            'print("ACPI-SNAPSHOT-BOOT: rejection PASS")\nend\ndetach\nquit\n')
    if args.fault == 'allocation':
        # Break at the capture's allocator call, then make that one allocation
        # return the documented null result without consuming a buddy block.
        # This x86-64 GNAT build returns the scalar out parameter in RAX.
        source = kernel.parent / 'src/acpi.adb'
        allocation_line = next(i for i, line in enumerate(source.read_text().splitlines(), 1)
                               if 'BuddyAllocator.alloc (Order, Block);' in line)
        script.write_text('set pagination off\nset confirm off\nset language c\n'
            f'file {kernel}\ntarget remote {debug}\nhbreak {source}:{allocation_line}\ncontinue\n'
            'hbreak buddyallocator__alloc\ncontinue\nreturn\nset $rax = 0\n'
            'python\nimport gdb\nf = gdb.newest_frame()\n'
            'while f is not None and (f.name() or "").replace("__", ".") != "acpi.setup":\n'
            '    f = f.older()\n'
            'assert f is not None\nf.select()\nend\nfinish\n'
            'python\nimport gdb, json\n'
            'assert int(gdb.parse_and_eval("$rax")) & 255 == 1\n'
            'assert int(gdb.parse_and_eval("acpi__boot_snapshot")) == 0\n'
            'assert int(gdb.parse_and_eval("acpi__capture_attempted")) == 1\n'
            'assert not bool(gdb.parse_and_eval("acpi__snapshot_pool")["consumed"])\n'
            f'with open({str(report)!r}, "w") as out:\n'
            '    json.dump(dict(status="PASS", fault="allocation", published=False, count=0, bytes=0), out)\n'
            'end\ndetach\nquit\n')
    # Capture serial while all guest CPUs are stopped. Detach resumes the
    # kernel; failures later in this intentionally module-free boot are outside
    # this test and must not race with the snapshot assertions.
    stopped_serial = case / 'stopped-serial.log'
    checks = ('python\n'
        f'with open({str(serial)!r}, errors="replace") as inp: boot_output = inp.read()\n'
        'assert not any(x in boot_output for x in ("CUBIT KERNEL PANIC", "CPU EXCEPTION", "Illegal memory access"))\n'
        f'assert ("ACPI: validated Multiboot2 root handoff" in boot_output) == {protocol == "multiboot2"!r}\n'
        + ('assert "immutable snapshot unavailable" in boot_output\n' if args.fault != 'none' else '')
        + f'with open({str(stopped_serial)!r}, "w") as out: out.write(boot_output)\n'
        'end\ndetach\nquit\n')
    script.write_text(script.read_text().replace('detach\nquit\n', checks))
    with (case / 'qemu.log').open('w') as log:
        vm = subprocess.Popen([
            'qemu-system-x86_64', '-accel', args.accel, '-machine', 'q35',
            '-cpu', 'host' if args.accel == 'kvm' else 'max', '-smp', '2', '-m', '512',
            '-display', 'none', '-nic', 'none', '-cdrom', str(case / 'boot.iso'),
            '-boot', 'd', '-serial', f'file:{serial}', '-no-reboot', '-S',
            '-gdb', f'unix:{debug},server=on,wait=off'] + extra, stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 10
            while not debug.exists():
                if vm.poll() is not None or time.monotonic() >= deadline:
                    raise RuntimeError(f'QEMU debugger unavailable: {case}')
                time.sleep(0.05)
            with (case / 'gdb.log').open('w') as debug_log:
                subprocess.run(['gdb', '-q', '-nx', '-batch', '-x', str(script)],
                               stdout=debug_log, stderr=subprocess.STDOUT, check=True, timeout=60)
            result = json.loads(report.read_text())
            assert result['status'] == 'PASS'
            result['kernel_sha256'] = kernel_hash
            report.write_text(json.dumps(result, indent=2) + '\n')
            output = stopped_serial.read_text(errors='replace')
            assert 'CUBIT KERNEL PANIC' not in output and 'CPU EXCEPTION' not in output
            if args.fault != 'none':
                assert 'immutable snapshot unavailable' in output
            print(protocol, args.fault, 'PASS', result['count'], 'captured tables',
                  'published' if result['published'] else 'not published', flush=True)
        finally:
            if vm.poll() is None:
                vm.terminate()
                try:
                    vm.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    vm.kill()
                    vm.wait()
assert hashlib.sha256(kernel.read_bytes()).hexdigest() == kernel_hash
print('ACPI-SNAPSHOT-BOOT: all requested protocols PASS', flush=True)
