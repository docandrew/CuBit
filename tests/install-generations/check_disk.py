#!/usr/bin/env python3
"""Checks an installed disk image on the host (tests/install-generations).

  check_disk.py DISK BLOCK_BYTES PAYLOAD_DIR
      The GPT (sgdisk -v), the ESP (fsck.fat, a FAT boot sector), the root
      (e2fsck -fn); every component INDEX names (sha256=) is in the root's
      storehouse under its own SHA-256 with exactly those bytes; generation 1
      is a System_Generation record with no pins and current names it.
  check_disk.py --not-bootable DISK BLOCK_BYTES
      SYS-002: the ESP partition holds no FAT boot sector (no 55 AA
      signature, no FAT32 type), so no firmware reads a loader from it.
  check_disk.py --remove-from-root DISK BLOCK_BYTES NAME
      Removes NAME from the root filesystem (the missing-profile case).
"""
import hashlib
import pathlib
import subprocess
import sys
import tempfile

TOOL_SECONDS = 300
SECTOR = 512
GPT_HEADER_LBA = 1
ENTRY_LBA = 2
ENTRY_BYTES = 128
BOOT_SIGNATURE = b"\x55\xaa"
FAT32_TYPE = b"FAT32   "
FAT32_TYPE_OFFSET = 82
COPY_CHUNK = 1 << 20


def partitions(disk, block):
    """(first, last) LBA of the GPT's first two entries: the ESP and the root."""
    with open(disk, "rb") as f:
        f.seek(GPT_HEADER_LBA * block)
        header = f.read(block)
        if header[:8] != b"EFI PART":
            raise SystemExit("check-disk: no primary GPT header")
        f.seek(ENTRY_LBA * block)
        table = f.read(2 * ENTRY_BYTES)
    result = []
    for index in range(2):
        entry = table[index * ENTRY_BYTES:(index + 1) * ENTRY_BYTES]
        result.append((int.from_bytes(entry[32:40], "little"), int.from_bytes(entry[40:48], "little")))
    return result


def extract(disk, block, span, target):
    first, last = span
    with open(disk, "rb") as source, open(target, "wb") as out:
        source.seek(first * block)
        left = (last - first + 1) * block
        while left:
            chunk = source.read(min(left, COPY_CHUNK))
            if not chunk:
                break
            out.write(chunk)
            left -= len(chunk)


def put_back(disk, block, span, image):
    first, _ = span
    with open(image, "rb") as source, open(disk, "r+b") as out:
        out.seek(first * block)
        while True:
            chunk = source.read(COPY_CHUNK)
            if not chunk:
                break
            out.write(chunk)


def run(command):
    return subprocess.run(command, capture_output=True, timeout=TOOL_SECONDS)


def esp_boots(disk, block, span):
    with open(disk, "rb") as f:
        f.seek(span[0] * block)
        sector = f.read(SECTOR)
    return sector[510:512] == BOOT_SIGNATURE and sector[FAT32_TYPE_OFFSET:FAT32_TYPE_OFFSET + 8] == FAT32_TYPE


def debugfs(image, request, write=False):
    return run(["debugfs"] + (["-w"] if write else []) + ["-R", request, str(image)])


def check(disk, block, payload):
    failures = 0

    def verdict(ok, what):
        nonlocal failures
        print(("PASS: " if ok else "FAIL: ") + what, flush=True)
        failures += 0 if ok else 1

    esp, root = partitions(disk, block)
    verdict(run(["sgdisk", "-v", str(disk)]).returncode == 0, "sgdisk -v: the GPT verifies")
    verdict(esp_boots(disk, block, esp), "the ESP has its FAT32 boot sector (written last)")
    with tempfile.TemporaryDirectory(prefix="check-disk.") as work:
        esp_image, root_image = pathlib.Path(work, "esp.img"), pathlib.Path(work, "root.img")
        extract(disk, block, esp, esp_image)
        extract(disk, block, root, root_image)
        verdict(run(["fsck.fat", "-n", str(esp_image)]).returncode == 0, "fsck.fat -n: the ESP is clean")
        verdict(b"BOOTX64.EFI" in run(["mdir", "-b", "-i", str(esp_image), "::/EFI/BOOT"]).stdout,
                "the ESP holds \\EFI\\BOOT\\BOOTX64.EFI")
        verdict(run(["e2fsck", "-fn", str(root_image)]).returncode == 0, "e2fsck -fn: the root is clean")
        components = {}
        for line in (payload / "INDEX").read_text().splitlines():
            fields = line.split("\t")
            for field in fields[1:]:
                if field.startswith("sha256="):
                    components[fields[0]] = field[len("sha256="):]
        missing = 0
        for name, digest in sorted(components.items()):
            kind = "executables" if name.startswith("root/") else "files"
            data = debugfs(root_image, f"cat /storehouse/{kind}/{digest}").stdout
            if hashlib.sha256(data).hexdigest() != digest:
                missing += 1
                print(f"  storehouse/{kind}/{digest} ({name}) is missing or differs", flush=True)
        verdict(missing == 0 and len(components) > 0,
                f"every component ({len(components)}) is in the storehouse under its SHA-256")
        record = debugfs(root_image, "cat /storehouse/profiles/system/1").stdout.decode(errors="replace")
        current = debugfs(root_image, "cat /storehouse/profiles/system/current").stdout.decode(errors="replace")
        verdict(record.strip() == "(System_Generation number => 1 previous => 0 pins => (list-of Service_Pin))",
                "generation 1: a System_Generation record with no pins (%r)" % record.strip())
        verdict(current == "1\n", "current names generation 1")
        init = debugfs(root_image, "cat /init.ccl").stdout
        verdict(init == (payload / "root" / "init.ccl").read_bytes(), "the startup plan is the payload's")
    return failures


def main():
    args = sys.argv[1:]
    if args[0] == "--not-bootable":
        disk, block = pathlib.Path(args[1]), int(args[2])
        esp, _ = partitions(disk, block)
        bootable = esp_boots(disk, block, esp)
        print(("FAIL: the ESP still has a FAT boot sector" if bootable
               else "PASS: the ESP has no FAT boot sector"), flush=True)
        return 1 if bootable else 0
    if args[0] == "--remove-from-root":
        disk, block, name = pathlib.Path(args[1]), int(args[2]), args[3]
        _, root = partitions(disk, block)
        with tempfile.TemporaryDirectory(prefix="check-disk.") as work:
            root_image = pathlib.Path(work, "root.img")
            extract(disk, block, root, root_image)
            if debugfs(root_image, f"rm /{name}", write=True).returncode != 0:
                return 1
            put_back(disk, block, root, root_image)
        return 0
    disk, block, payload = pathlib.Path(args[0]), int(args[1]), pathlib.Path(args[2])
    return 1 if check(disk, block, payload) else 0


if __name__ == "__main__":
    sys.exit(main())
