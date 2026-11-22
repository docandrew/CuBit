#!/usr/bin/env python3
"""Put the SPAWN rules check in procmgr's place in a stage-one initrd.

Usage: swap-procmgr.py INITRD CHECK_IMAGE

The initrd is a newc cpio archive (ccl-image's realize.py writes it);
devmgr starts the member named procmgr.svc with procmgr's handles. Only
that member's data changes; every other member keeps its bytes and order.
"""
import pathlib
import sys

MAGIC = b"070701"
HEADER = 110
FIELDS = 13
FILESIZE, NAMESIZE = 6, 11
TARGET = b"procmgr.svc"
BLOCK = 512  # cpio pads the archive to whole blocks


def pad(length):
    return (4 - length % 4) % 4


def members(data):
    offset = 0
    while True:
        if data[offset:offset + 6] != MAGIC:
            raise ValueError(f"not a newc member at {offset}")
        fields = [int(data[offset + 6 + 8 * i:offset + 14 + 8 * i], 16) for i in range(FIELDS)]
        name_at = offset + HEADER
        name = data[name_at:name_at + fields[NAMESIZE] - 1]
        body_at = name_at + fields[NAMESIZE] + pad(HEADER + fields[NAMESIZE])
        body = data[body_at:body_at + fields[FILESIZE]]
        yield fields, name, body
        offset = body_at + fields[FILESIZE] + pad(fields[FILESIZE])
        if name == b"TRAILER!!!":
            return


def encode(fields, name, body):
    fields = list(fields)
    fields[FILESIZE] = len(body)
    fields[NAMESIZE] = len(name) + 1
    header = MAGIC + b"".join(b"%08X" % f for f in fields)
    head = header + name + b"\0"
    return head + b"\0" * pad(len(head)) + body + b"\0" * pad(len(body))


def main():
    initrd, check = map(pathlib.Path, sys.argv[1:3])
    replacement = check.read_bytes()
    out, swapped = bytearray(), 0
    for fields, name, body in members(initrd.read_bytes()):
        if name == TARGET:
            body, swapped = replacement, swapped + 1
        out += encode(fields, name, body)
    out += b"\0" * ((BLOCK - len(out) % BLOCK) % BLOCK)
    if swapped != 1:
        raise SystemExit(f"swap-procmgr: {swapped} members named procmgr.svc, expected 1")
    initrd.write_bytes(bytes(out))
    print(f"spawn-rules: procmgr.svc is now {check.name} ({len(replacement)} bytes)")


if __name__ == "__main__":
    main()
