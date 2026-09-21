#!/usr/bin/env python3
"""Independently inspect the two native ELF fixtures, without running Linux code."""
import pathlib
import re
import struct
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
BUILD = ROOT / "userspace/rust/build"


def elf(path):
    data = path.read_bytes()
    assert data[:7] == b"\x7fELF\x02\x01\x01", "not ELF64 little-endian"
    header = struct.unpack_from("<HHIQQQIHHHHHH", data, 16)
    kind, machine, _, entry, phoff, shoff, _, _, phsize, phnum, shsize, shnum, names = header
    assert (kind, machine) == (2, 62), "not a static x86-64 executable"
    segments = [struct.unpack_from("<IIQQQQQQ", data, phoff + i * phsize)
                for i in range(phnum)]
    assert not any(p[0] in (2, 3) for p in segments), "dynamic loader/dependency"
    stacks = [p for p in segments if p[0] == 0x6474e551]
    assert len(stacks) == 1 and stacks[0][6] == 1024 * 1024 and stacks[0][1] == 6
    loads = [p for p in segments if p[0] == 1]
    assert any(p[1] & 1 and p[3] <= entry < p[3] + p[6] for p in loads)
    assert not any(p[1] & 3 == 3 for p in loads), "writable executable segment"
    assert any(p[6] > p[5] for p in loads), "no BSS initialization exercised"
    assert sum(p[6] - p[5] for p in loads) < 1024 * 1024, "heap payload regressed to static BSS"
    sections = [struct.unpack_from("<IIQQQQIIQQ", data, shoff + i * shsize)
                for i in range(shnum)]
    table = sections[names]
    strings = data[table[4]:table[4] + table[5]]
    result = {}
    for s in sections:
        name = strings[s[0]:].split(b"\0", 1)[0].decode()
        if name.startswith(".cubit."):
            assert not s[2] & 2, "manifest unexpectedly loadable"
            result[name] = data[s[4]:s[4] + s[5]]
    undefined = subprocess.check_output(["nm", "-u", path], text=True)
    assert not undefined.strip(), undefined
    return [(p, data[p[2]:p[2] + p[5]]) for p in loads], result


allowed, a = elf(BUILD / "rust-probe.app")
denied, d = elf(BUILD / "rust-probe-denied.app")
assert allowed == denied, "test binaries differ beyond non-loaded manifest/debug sections"
assert a['.cubit.caps'] == (
    struct.pack('<IHH', 0x43424954, 1, 2)
    + struct.pack('<BBHIQ', 2, 3, 24, 18, 0)
    + struct.pack('<BBHIQ', 2, 3, 25, 19, 0))
assert d['.cubit.caps'] == (
    struct.pack('<IHH', 0x43424954, 1, 1)
    + struct.pack('<BBHIQ', 2, 3, 24, 18, 0))

# Fail when the narrow Rust boundary drifts from the authoritative kernel enum.
kernel = (ROOT / 'kernel/src/syscall.ads').read_text()
runtime = (ROOT / 'userspace/rust/cubit/src/lib.rs').read_text()
for rust, ada in [('Exit', 'SYSCALL_EXIT'),
                  ('GrowHeap', 'SYSCALL_SBRK'),
                  ('CallViaEndpointCapability', 'SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY')]:
    expected = re.search(r'\b' + ada + r'\s*=>\s*(\d+)', kernel).group(1)
    actual = re.search(r'\b' + rust + r'\s*=\s*(\d+)', runtime).group(1)
    assert actual == expected, f'{rust} syscall ABI drift'
print('PASS: static ELF, stack/BSS, identical loaded code/data, CCL ceilings, syscall ABI')
