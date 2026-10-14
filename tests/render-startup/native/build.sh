#!/usr/bin/env bash
# Run inside Nix while holding coordination/build.lock.
set -euo pipefail
cd "$(dirname "$0")/../../.."
fixture=tests/render-startup/native
mkdir -p "$fixture/build"
python3 - <<'PY'
from pathlib import Path
import struct
out = Path('tests/render-startup/native/build')
# CAP-002: the slot field is reserved zero (no capability is placed at a
# number). 'occupied' keeps an old-style slot request (3, a bootstrap
# slot): the loader must refuse the executable before any render attempt.
for name, slot, flags in [('optional', 0, 1), ('occupied', 3, 1), ('invalid', 0, 2)]:
    # Same canonical .cubit.caps wire ABI as request-render. Optional flags=1.
    # Keep this independent of CCL syntax so the native decoder is exercised.
    (out / (name + '.caps')).write_bytes(
        struct.pack('<IHH', 0x43424954, 1, 1) +
        struct.pack('<BBHIQ', 11, 3, slot, flags, 0))
PY
objcopy -I binary -O elf64-x86-64 -B i386:x86-64 \
  --rename-section .data=.cubit.caps,alloc,load,readonly,data,contents \
  "$fixture/build/optional.caps" "$fixture/build/manifest.o"
(cd kernel && alr exec -- gprbuild -p -P ../"$fixture/fixture.gpr")
cp "$fixture/build/software.app" kernel/isodir/boot/render-software.app
cp "$fixture/build/software.app" kernel/isodir/boot/render-fallback.app
objcopy --update-section .cubit.caps="$fixture/build/occupied.caps" \
  "$fixture/build/software.app" kernel/isodir/boot/render-occupied.app
objcopy --update-section .cubit.caps="$fixture/build/invalid.caps" \
  "$fixture/build/software.app" kernel/isodir/boot/render-invalid.app
