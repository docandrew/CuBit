#!/usr/bin/env python3
"""Overlay staged desktop files on a disposable, verified copy of an ext2 disk.

Never opens the base writable and never mounts either image. The previous output
survives failed staging, including debugfs's notorious success-on-ENOSPC exit.
This is a developer image tool, not a parser for untrusted downloaded disks.
Callers must hold coordination/build.lock for shared staging and must not use
an image that a VM currently has open. --replace is for generated scratch disks,
not an installed system or the persistent Config playground disk.
"""
import argparse
import hashlib
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import tempfile


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").digest()


def checked(command, **kwargs):
    return subprocess.run(command, check=True, capture_output=True, text=True, **kwargs)


def prepare(base, output, files, *, replace=False):
    base, output = Path(base), Path(output)
    if not base.is_file() or base.is_symlink():
        raise ValueError("base must be a regular development disk image")
    if output.is_symlink() or output.resolve() == base.resolve():
        raise ValueError("output must not alias the base image")
    if output.exists():
        if os.path.samefile(base, output) or not output.is_file():
            raise ValueError("output must be a separate regular file")
        if not replace:
            raise FileExistsError("output exists; --replace is only for a disposable desktop disk")
    payloads = {}
    for destination, source in files:
        path = PurePosixPath(destination)
        if (not path.parts or path.is_absolute() or ".." in path.parts
                or str(path) != destination or destination in payloads
                or any(not (char.isascii() and (char.isalnum() or char in "/._-"))
                       for char in destination)):
            raise ValueError("invalid/duplicate image path: " + destination)
        source = Path(source)
        if not source.is_file():
            raise ValueError("missing staged payload: " + str(source))
        payloads[destination] = source
    # Detect file/directory collisions before doing any filesystem work.
    for destination in payloads:
        if any(str(parent) in payloads for parent in PurePosixPath(destination).parents):
            raise ValueError("payload file also used as a directory: " + destination)
    checked(["e2fsck", "-fn", str(base)])
    with tempfile.TemporaryDirectory(prefix=".cubit-desktop-", dir=output.parent) as directory:
        root = Path(directory)
        candidate = root / "disk.img"
        shutil.copyfile(base, candidate)
        # Reserve enough space even for an old, nearly full base image. Growing
        # a disk file does not reserve guest RAM. Always start from the base,
        # so repeated launcher invocations do not grow the image indefinitely.
        quantum = 64 * 1024 * 1024
        extra = sum(path.stat().st_size for path in payloads.values()) * 2 + quantum
        size = ((base.stat().st_size + extra + quantum - 1) // quantum) * quantum
        with candidate.open("r+b") as stream:
            stream.truncate(size)
        checked(["resize2fs", "disk.img"], cwd=root)

        def debug(command, write=False):
            return checked(["debugfs", *(["-w"] if write else []),
                            "-R", command, "disk.img"], cwd=root)

        directories = {"work"}
        for destination in payloads:
            directories.update(str(parent) for parent in PurePosixPath(destination).parents
                               if str(parent) != ".")
        for directory in sorted(directories, key=lambda path: (path.count("/"), path)):
            debug("mkdir /" + directory, write=True)
        for index, (destination, source) in enumerate(payloads.items()):
            # Generated local names keep host paths out of the debugfs command
            # language, including paths with spaces, quotes or backslashes.
            staged = root / f"payload-{index}"
            shutil.copyfile(source, staged)
            expected = digest(staged)
            debug("rm /" + destination, write=True)
            debug(f"write {staged.name} /{destination}", write=True)
            dumped = root / f"verified-{index}"
            reply = debug(f"dump /{destination} {dumped.name}")
            if not dumped.is_file() or digest(dumped) != expected:
                raise RuntimeError("staged payload mismatch: " + destination + "\n" + reply.stderr)
        checked(["e2fsck", "-fn", "disk.img"], cwd=root)
        os.replace(candidate, output)
    print(f"Desktop scratch disk: {len(payloads)} payloads verified; base unchanged")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("base", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--replace", action="store_true")
    parser.add_argument("--boot", nargs="*", type=Path, default=[])
    parser.add_argument("--file", action="append", default=[], metavar="DEST=SOURCE")
    args = parser.parse_args()
    files = [(path.name, path) for path in args.boot]
    files.extend((name, Path(source)) for name, source in (item.split("=", 1) for item in args.file))
    prepare(args.base, args.output, files, replace=args.replace)


if __name__ == "__main__":
    main()
