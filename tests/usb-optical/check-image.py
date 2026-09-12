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
expected_apps = {'config.svc', 'netmgr.svc', 'netstack.svc', 'virtio-net.drv',
                 'virtio-gpu.drv', 'hda.drv', 'mixer.svc', 'procmgr.svc',
                 'logstore.svc', 'clock.svc', 'display.svc', 'desktop.svc',
                 'ccl-workbench.app', 'devices.app', 'files.app', 'doom.elf', 'doom1.wad',
                 'sameboy.app'}
assert expected_apps <= apps.keys(), expected_apps - apps.keys()
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
assert names == {'devmgr.svc', 'filesystem.svc', 'ps2.drv', 'xhci.drv',
                 'live-rw.ext2', 'init.conf', 'system.conf'}, names
assert not names & expected_apps
grub = entries(*boot['grub'])
config = contents(grub['grub.cfg']).decode()
modules = [line.split()[1:] for line in config.splitlines()
           if line.strip().startswith('module ')]
assert modules and all(line == ['/boot/initrd.img', 'init.img'] for line in modules)
assert contents(apps['doom1.wad'])[:4] in (b'IWAD', b'PWAD')
print(f'IMAGE AUDIT PASS: {len(names)} bootstrap files; {len(expected_apps)} CD payload files.')
print(f'CARTRIDGE AUDIT PASS: {len(cartridges) - 2} ROMs on CD, none in initrd.')
print(f'ISO sha256: {hashlib.sha256(data).hexdigest()}')
