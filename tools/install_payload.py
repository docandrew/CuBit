#!/usr/bin/env python3
"""The install payload installer.svc reads (SYS-001): esp/ (the UEFI
fallback loader EFI/BOOT/BOOTX64.EFI, the kernel BOOT/KERNEL and the
installed system's bootstrap archive BOOT/INITRD.IMG), root/ (the image
closure) and INDEX, one "esp/<path>" or "root/<path>" line per file. A
line may instead name, after a tab, where the file already is below the
medium's root: the live image's closure is not stored twice; or a whole
CuBit name ("@mem:0/..."): the device-build live image's RAM root. After
another tab, "executable" marks a file that runs (mode 0755), and
"sha256=<hex>" a component: a program (an ELF file) or a boot file, which
the installer also stores by content in the installed root's storehouse
(storehouse/executables/<sha256>; boot files storehouse/files/<sha256>) and
checks against this digest as it copies it (SYS-003). A file stored in the
payload with fields has an empty medium field: "<path>" TAB TAB "<field>".
A line "root/<path>/" is a directory (for empty ones).

ESP names are 8.3 (FAT32 without long names). The realizer adds a payload
derived from an image's checked plan (realize.py --install-efi); this
command line builds one from explicit files (the headless test).

usage: install_payload.py OUT_DIR --efi PATH --kernel PATH --initrd PATH
                          [--root NAME=PATH ...] [--on-medium NAME=MEDIUM_PATH ...]
"""
import argparse
import hashlib
import pathlib
import sys

ESP_LOADER = "EFI/BOOT/BOOTX64.EFI"
ESP_KERNEL = "BOOT/KERNEL"
ESP_INITRD = "BOOT/INITRD.IMG"
EXECUTABLE = "executable"
DIGEST = "sha256="
ELF_MAGIC = b"\x7fELF"


def is_component(data: bytes) -> bool:
    """A program the installer stores by content: an ELF file."""
    return data[:len(ELF_MAGIC)] == ELF_MAGIC


def digest_field(data: bytes) -> str:
    return "\t" + DIGEST + hashlib.sha256(data).hexdigest()


def safe(name: str) -> str:
    if not name or name.startswith("/") or ".." in name.split("/") or "\t" in name or "\n" in name:
        raise ValueError(f"unsafe payload path: {name!r}")
    return name


def entries(efi: bytes, kernel: bytes, initrd: bytes, root: dict, on_medium: dict = None,
            executable: frozenset = frozenset(), directories: frozenset = frozenset(),
            digests: dict = None) -> dict:
    """The payload's files by path relative to its directory, INDEX last.
    root maps installed names to bytes stored in the payload; on_medium
    maps installed names to paths below the medium's root (or whole CuBit
    names); executable names those of them that run; directories names
    directories to make even when empty; digests maps on_medium names that
    are components to their bytes' SHA-256 (hex). Stored files that are
    programs, and the boot files, carry their digests too."""
    on_medium = on_medium or {}
    digests = digests or {}
    if not executable <= on_medium.keys():
        raise ValueError("only files named where they are may be marked executable")
    if not digests.keys() <= on_medium.keys():
        raise ValueError("digests name files named where they are")
    result = {f"esp/{ESP_LOADER}": efi, f"esp/{ESP_KERNEL}": kernel, f"esp/{ESP_INITRD}": initrd}
    for name in sorted(root):
        result[f"root/{safe(name)}"] = root[name]
    lines = []
    for path in sorted(result):
        data = result[path]
        component = path.startswith("esp/") or is_component(data)
        lines.append(f"{path}\t{digest_field(data)}" if component else path)
    for name in sorted(on_medium):
        if name in root:
            raise ValueError(f"root path given twice: {name}")
        mark = f"\t{EXECUTABLE}" if name in executable else ""
        digest = f"\t{DIGEST}{digests[name]}" if name in digests else ""
        lines.append(f"root/{safe(name)}\t{safe(on_medium[name])}{mark}{digest}")
    for name in sorted(directories):
        lines.append(f"root/{safe(name)}/")
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
