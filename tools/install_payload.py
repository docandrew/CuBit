#!/usr/bin/env python3
"""The install payload installer.svc reads (SYS-001): esp/ (the UEFI
fallback loader EFI/BOOT/BOOTX64.EFI, the kernel BOOT/KERNEL and the
installed system's bootstrap archive BOOT/INITRD.IMG), root/ (the image
closure) and INDEX, one "esp/<path>" or "root/<path>" line per file. A
line may instead name, after a tab, where the file already is below the
medium's root: the live image's closure is not stored twice.

ESP names are 8.3 (FAT32 without long names). The realizer adds a payload
derived from an image's checked plan (realize.py --install-efi); this
command line builds one from explicit files (the headless test).

usage: install_payload.py OUT_DIR --efi PATH --kernel PATH --initrd PATH
                          [--root NAME=PATH ...] [--on-medium NAME=MEDIUM_PATH ...]
"""
import argparse
import pathlib
import sys

ESP_LOADER = "EFI/BOOT/BOOTX64.EFI"
ESP_KERNEL = "BOOT/KERNEL"
ESP_INITRD = "BOOT/INITRD.IMG"


def safe(name: str) -> str:
    if not name or name.startswith("/") or ".." in name.split("/") or "\t" in name or "\n" in name:
        raise ValueError(f"unsafe payload path: {name!r}")
    return name


def entries(efi: bytes, kernel: bytes, initrd: bytes, root: dict, on_medium: dict = None) -> dict:
    """The payload's files by path relative to its directory, INDEX last.
    root maps installed names to bytes stored in the payload; on_medium
    maps installed names to paths below the medium's root."""
    on_medium = on_medium or {}
    result = {f"esp/{ESP_LOADER}": efi, f"esp/{ESP_KERNEL}": kernel, f"esp/{ESP_INITRD}": initrd}
    for name in sorted(root):
        result[f"root/{safe(name)}"] = root[name]
    lines = sorted(result)
    for name in sorted(on_medium):
        if name in root:
            raise ValueError(f"root path given twice: {name}")
        lines.append(f"root/{safe(name)}\t{safe(on_medium[name])}")
    result["INDEX"] = ("\n".join(lines) + "\n").encode("ascii")
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("out", type=pathlib.Path)
    parser.add_argument("--efi", type=pathlib.Path, required=True)
    parser.add_argument("--kernel", type=pathlib.Path, required=True)
    parser.add_argument("--initrd", type=pathlib.Path, required=True)
    parser.add_argument("--root", action="append", default=[], metavar="NAME=PATH")
    parser.add_argument("--on-medium", action="append", default=[], metavar="NAME=MEDIUM_PATH")
    args = parser.parse_args()
    on_medium = dict(binding.split("=", 1) for binding in args.on_medium)
    root = {}
    for binding in args.root:
        name, path = binding.split("=", 1)
        root[name] = pathlib.Path(path).read_bytes()
    for name, data in entries(args.efi.read_bytes(), args.kernel.read_bytes(),
                              args.initrd.read_bytes(), root, on_medium).items():
        target = args.out / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
    return 0


if __name__ == "__main__":
    sys.exit(main())
