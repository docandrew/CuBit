#!/usr/bin/env python3
"""Explicit, local-only cartridge input. Never downloads or modifies originals."""
import argparse
import pathlib
import shutil

parser = argparse.ArgumentParser()
parser.add_argument('destination', type=pathlib.Path)
parser.add_argument('--directory', type=pathlib.Path)
args = parser.parse_args()
root = pathlib.Path(__file__).resolve().parents[2]
roms = []
if args.directory is not None:
    if not args.directory.is_dir():
        parser.error('ROM directory does not exist')
    roms = sorted((p for p in args.directory.iterdir()
                   if p.is_file() and p.suffix.lower() in ('.gb', '.gbc')),
                  key=lambda p: p.name)
    if len(roms) > 15:
        parser.error('first frontend supports at most 15 private cartridges')
    for rom in roms:
        if not 0x150 <= rom.stat().st_size <= 8 * 1024 * 1024:
            parser.error(f'unsupported cartridge size: {rom.name}')
args.destination.mkdir(parents=True, exist_ok=True)
shutil.copyfile(root / 'userspace/c/sameboy_build/test.gb', args.destination / '00.gb')
for index, rom in enumerate(roms, 1):
    shutil.copyfile(rom, args.destination / f'{index:02d}.gb')
    print(f'SameBoy ROM {index:02d}: {rom.name}')
print(f'SameBoy: original test cartridge + {len(roms)} local cartridges')
