#!/usr/bin/env python3
"""Writes a test install payload: ESP_DIR (8.3 names, boot files) and
ROOT_DIR (a closure-like tree: nested directories, an empty file, a file
needing double indirect blocks, a directory spanning several blocks).
Contents are deterministic pseudo-random bytes.

usage: make-payload.py ESP_DIR ROOT_DIR
"""
import hashlib
import os
import sys

KIB = 1024
MIB = 1024 * KIB


def content(name: str, size: int) -> bytes:
    out = bytearray()
    counter = 0
    while len(out) < size:
        out += hashlib.sha256(f"{name}:{counter}".encode()).digest()
        counter += 1
    return bytes(out[:size])


def put(root: str, relative: str, size: int) -> None:
    path = os.path.join(root, relative)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(content(relative, size))


def main() -> None:
    esp, root = sys.argv[1], sys.argv[2]
    put(esp, "EFI/BOOT/BOOTX64.EFI", 300 * KIB + 17)
    put(esp, "BOOT/KERNEL", 2 * MIB + 5)
    put(esp, "BOOT/INITRD.IMG", 700 * KIB)
    put(root, "apps/netstack.svc", 1 * MIB + 3)
    put(root, "apps/big.bin", 6 * MIB + 4097)          # double indirect
    put(root, "apps/firmware/intel/tgl_guc_70.bin", 330 * KIB)
    put(root, "apps/Assets/cubit-icons/4/applications.qoi", 90 * KIB)
    put(root, "empty.txt", 0)
    put(root, "licenses/a-file-with-a-rather-long-name-to-pack-records.txt", 4096)
    for i in range(300):                               # a multi-block directory
        put(root, f"many/file-{i:04d}.dat", (i * 37) % 9000)


if __name__ == "__main__":
    main()
