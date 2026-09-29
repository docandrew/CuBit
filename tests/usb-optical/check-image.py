#!/usr/bin/env python3
"""Independently inspect the ISO primary tree and bootstrap archive membership."""
import hashlib
import pathlib
import struct
import sys

image = pathlib.Path(sys.argv[1])
data = image.read_bytes()

def dual32(blob, offset):
    little = struct.unpack_from('<I', blob, offset)[0]
    assert little == struct.unpack_from('>I', blob, offset + 4)[0]
    return little

def record(blob, offset):
    length = blob[offset]
    assert length >= 34 and offset + length <= len(blob)
    assert offset % 2048 + length <= 2048
    extent = dual32(blob, offset + 2)
    size = dual32(blob, offset + 10)
    assert extent * 2048 + size <= len(data)
    name_bytes = blob[offset + 32]
    assert 33 + name_bytes <= length
    name = blob[offset + 33:offset + 33 + name_bytes].decode('ascii')
    name = name.removesuffix(';1')
    return name, extent, size, length

def entries(extent, size):
    blob = data[extent * 2048:extent * 2048 + size]
    result = {}
    offset = 0
    while offset < len(blob):
        if blob[offset] == 0:
            offset = (offset // 2048 + 1) * 2048
            continue
        name, sector, length, consumed = record(blob, offset)
        assert name not in result
        result[name] = (sector, length)
        offset += consumed
    return result

def contents(entry):
    sector, size = entry
    return data[sector * 2048:sector * 2048 + size]

pvd = next(data[i * 2048:(i + 1) * 2048] for i in range(16, 48)
           if data[i * 2048:i * 2048 + 7] == b'\x01CD001\x01')
_, extent, size, _ = record(pvd, 156)
root = entries(extent, size)
apps = entries(*root['apps'])
boot = entries(*root['boot'])
# Native boot filesystem paths resolve below apps/, not the ISO root.
firmware = entries(*apps['firmware'])
intel = entries(*firmware['intel'])
licenses = entries(*root['licenses'])
assert hashlib.sha256(contents(intel['tgl_guc_70.bin'])).hexdigest() == '2f1f57a1b23d186f2592318d1e07a1365968932841ccb3e7177c516ba006e2f6'
assert hashlib.sha256(contents(licenses['Intel-GPU.txt'])).hexdigest() == '8542aeabf2761935122d693561e16766ce1bcc2b0d003204f9040b7d6d929f2e'
print('INTEL FIRMWARE AUDIT PASS: reachable firmware and original license hashes match')
expected_apps = {'config.svc', 'netmgr.svc', 'netstack.svc', 'virtio-net.drv',
                 'virtio-gpu.drv', 'intel-gpu.drv', 'hda.drv', 'mixer.svc', 'procmgr.svc',
                 'logstore.svc', 'clock.svc', 'display.svc', 'desktop.svc', 'boot-logs.app',
                 'ccl-workbench.app', 'devices.app', 'files.app', 'doom.elf', 'doom1.wad',
                 'sameboy.app', 'config-inspector.app', 'config-storage.svc', 'cubitshell.app', 'mesa-cube.app'}
assert expected_apps <= apps.keys(), expected_apps - apps.keys()
mesa_notices = entries(*licenses['mesa'])
assert {'MESA-SOURCE.tar.gz', 'UPSTREAM-LICENSE.rst', 'SOURCE.nix',
        'CUBIT-PLATFORM.patch', 'ELF-SHA256.txt', 'LINKED-SOURCES.json',
        'LINK-MAP.txt', 'licenses'} <= mesa_notices.keys()
mesa_hash = contents(mesa_notices['ELF-SHA256.txt']).decode().split()[0]
assert hashlib.sha256(contents(apps['mesa-cube.app'])).hexdigest() == mesa_hash
assert contents(mesa_notices['MESA-SOURCE.tar.gz'])[:2] == b'\x1f\x8b'
print('MESA IMAGE AUDIT PASS: app matches recorded hash; source and notice bundle present')
cartridges = entries(*apps['sameboy'])
assert '00.gb' in cartridges
for name, entry in cartridges.items():
    if name not in ('\x00', '\x01'):
        assert name in {f'{i:02d}.gb' for i in range(16)}, name
        assert 0x150 <= entry[1] <= 8 * 1024 * 1024
archive = contents(boot['initrd.img'])
names = set()
position = 0
while True:
    assert archive[position:position + 6] == b'070701'
    size = int(archive[position + 54:position + 62], 16)
    name_size = int(archive[position + 94:position + 102], 16)
    name = archive[position + 110:position + 110 + name_size - 1].decode()
    if name == 'TRAILER!!!':
        break
    assert name not in names
    names.add(name)
    start = (position + 110 + name_size + 3) & ~3
    assert start + size <= len(archive)
    position = (start + size + 3) & ~3
assert names == {'devmgr.svc', 'filesystem.svc', 'ramdisk.drv', 'ps2.drv', 'xhci.drv',
                 'live-rw.ext2', 'init.ccl', 'system.ccl'}, names
assert not names & expected_apps
grub = entries(*boot['grub'])
config = contents(grub['grub.cfg']).decode()
modules = [line.split()[1:] for line in config.splitlines()
           if line.strip().startswith(('module ', 'module2 ', '$cubit_module '))]
assert modules and all(line == ['/boot/initrd.img', 'init.img'] for line in modules)
assert 'set cubit_loader=multiboot2' in config and 'set cubit_module=module2' in config
assert contents(apps['doom1.wad'])[:4] in (b'IWAD', b'PWAD')
print(f'IMAGE AUDIT PASS: {len(names)} bootstrap files; {len(expected_apps)} CD payload files.')
print(f'CARTRIDGE AUDIT PASS: {len(cartridges) - 2} ROMs on CD, none in initrd.')
print(f'ISO sha256: {hashlib.sha256(data).hexdigest()}')
