#!/usr/bin/env python3
"""Separate debug information from the executables staged into an image.

Image staging (userspace/ccl/tools/ccl-image/realize.py and
tools/build_development_disk.py) calls Splitter.split for every payload.
For a static ELF executable it:

1. keeps the debug sections and symbol table in a separate file,
   `objcopy --only-keep-debug`;
2. stages a copy without them, `objcopy --strip-all` (removes .debug_*,
   .symtab and .strtab; keeps every allocated section, so .eh_frame,
   .gcc_except_table and .note.gnu.build-id stay, and the .cubit.* manifest
   sections, which the loaders find by type and magic, not by symbol);
3. links the two, `objcopy --add-gnu-debuglink`.

Nothing in CuBit reads DWARF or .symtab at run time (procmgr and the kernel
loader read program headers and the .cubit.* sections only), so the staged
binary behaves exactly as the full one.

The debug file is stored on the host under the GNU build-id, in the layout
gdb, addr2line, eu-addr2line and debuginfod use:

    <debug dir>/.build-id/<first two hex digits>/<rest>.debug

and each image gets a small manifest, `<debug dir>/images/<image>.build-ids.json`
(name, build-id, input hash, sizes). This mirrors the Storehouse outputs of
BLD-001 ("Multiple outputs per entry"): the staged binary is a program's
`out`, `<debug dir>` holds what the `debug` outputs' `lib/debug/` trees
will hold, and the manifest is the entries' build-id metadata. See
docs/image-debug-info.md.
"""
import hashlib
import json
import os
from pathlib import Path
import struct
import subprocess
import tempfile

ELF_MAGIC = b"\x7fELF"
ELFCLASS64 = 2
ELFDATA2LSB = 1
ET_EXEC = 2
SHT_NOTE = 7
NT_GNU_BUILD_ID = 3
GNU_OWNER = b"GNU\x00"
DEBUG_PREFIX = ".debug_"
STRIPPED_SECTIONS = (".symtab", ".strtab")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def sections(data):
    """(name, type, offset, size) of each section of a little-endian ELF64."""
    if len(data) < 64:
        return []
    shoff, = struct.unpack_from("<Q", data, 40)
    shentsize, shnum, shstrndx = struct.unpack_from("<HHH", data, 58)
    if shoff == 0 or shentsize < 64 or shoff + shnum * shentsize > len(data) or shstrndx >= shnum:
        return []
    headers = [struct.unpack_from("<IIQQQQ", data, shoff + i * shentsize) for i in range(shnum)]
    names_at, names_size = headers[shstrndx][4], headers[shstrndx][5]
    if names_at + names_size > len(data):
        return []
    result = []
    for name, kind, _, _, offset, size in headers:
        end = data.find(b"\x00", names_at + name, names_at + names_size)
        label = data[names_at + name:end].decode("ascii", "replace") if end >= 0 else ""
        result.append((label, kind, offset, size))
    return result


def is_executable(data):
    return (data[:4] == ELF_MAGIC and len(data) >= 64 and data[4] == ELFCLASS64
            and data[5] == ELFDATA2LSB and struct.unpack_from("<H", data, 16)[0] == ET_EXEC)


def build_id(data):
    """The NT_GNU_BUILD_ID of an ELF64 executable, as hex, or None."""
    for _, kind, offset, size in sections(data):
        if kind != SHT_NOTE or offset + size > len(data):
            continue
        at, end = offset, offset + size
        while at + 12 <= end:
            namesz, descsz, note = struct.unpack_from("<III", data, at)
            name_at = at + 12
            desc_at = name_at + ((namesz + 3) & ~3)
            if desc_at + descsz > end:
                break
            if note == NT_GNU_BUILD_ID and data[name_at:name_at + namesz] == GNU_OWNER and descsz:
                return data[desc_at:desc_at + descsz].hex()
            at = desc_at + ((descsz + 3) & ~3)
    return None


def debug_bytes(data):
    """Bytes the staged copy drops: .debug_* plus the symbol tables."""
    return sum(size for name, _, _, size in sections(data)
               if name.startswith(DEBUG_PREFIX) or name in STRIPPED_SECTIONS)


def run(command):
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode:
        raise subprocess.CalledProcessError(result.returncode, command, result.stdout, result.stderr)


SPARSE_BLOCK = 64 * 1024


def write_sparse(path, data):
    """A debug file keeps the executable's offsets: its empty allocated
    sections leave long zero runs, written as holes."""
    zero = bytes(SPARSE_BLOCK)
    with path.open("wb") as stream:
        for at in range(0, len(data), SPARSE_BLOCK):
            block = data[at:at + SPARSE_BLOCK]
            if block == zero[:len(block)]:
                stream.seek(len(block), os.SEEK_CUR)
            else:
                stream.write(block)
        stream.truncate(len(data))


class Splitter:
    """Collects one image's debug files and writes its build-id manifest."""

    def __init__(self, debug_dir, image_name):
        self.debug_dir = Path(debug_dir)
        self.image_name = image_name
        self.records = []

    def debug_path(self, identity):
        return self.debug_dir / ".build-id" / identity[:2] / f"{identity[2:]}.debug"

    def split(self, destination, data):
        """The bytes to stage for destination: data itself unless it is an
        executable with debug information to separate."""
        if not is_executable(data) or debug_bytes(data) == 0:
            return data
        identity = build_id(data)
        if identity is None:
            # Without a build-id the debug file could not be found again:
            # stage the full binary and say so (every CuBit link passes
            # --build-id; see docs/image-debug-info.md).
            self.records.append({"destination": destination, "build_id": None,
                                 "input_sha256": digest(data), "size": len(data),
                                 "note": "no GNU build-id: staged unstripped"})
            return data
        with tempfile.TemporaryDirectory(prefix="cubit-debug-split.") as directory:
            full = Path(directory) / Path(destination).name
            debug = full.with_name(full.name + ".debug")
            full.write_bytes(data)
            # The debug file keeps the allocated build-id note, so objcopy
            # warns that it cannot place it in its (now empty) segment; that
            # is expected, and only a failure is reported.
            run(["objcopy", "--only-keep-debug", "--compress-debug-sections=zlib", full, debug])
            run(["objcopy", "--strip-all", full])
            run(["objcopy", f"--add-gnu-debuglink={debug}", full])
            stripped = full.read_bytes()
            separate = debug.read_bytes()
        if build_id(stripped) != identity:
            raise ValueError(f"build-id lost while stripping {destination}")
        target = self.debug_path(identity)
        if not target.exists() or target.read_bytes() != separate:
            target.parent.mkdir(parents=True, exist_ok=True)
            pending = target.with_name(target.name + f".{os.getpid()}.tmp")
            write_sparse(pending, separate)
            pending.chmod(0o444)
            os.replace(pending, target)
        self.records.append({
            "destination": destination, "build_id": identity,
            "input_sha256": digest(data), "staged_sha256": digest(stripped),
            "input_size": len(data), "staged_size": len(stripped),
            "debug_file": target.relative_to(self.debug_dir).as_posix(),
            "debug_size": len(separate)})
        return stripped

    def summary(self):
        split = [r for r in self.records if r["build_id"]]
        return {"binaries": len(split),
                "input_bytes": sum(r["input_size"] for r in split),
                "staged_bytes": sum(r["staged_size"] for r in split),
                "unstripped_without_build_id": sorted(
                    r["destination"] for r in self.records if not r["build_id"])}

    def write_manifest(self, image_sha256=None):
        path = self.debug_dir / "images" / f"{self.image_name}.build-ids.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        document = {"format": "cubit-image-build-ids-1", "image": self.image_name,
                    "image_sha256": image_sha256, "debug_dir": str(self.debug_dir),
                    "summary": self.summary(),
                    "binaries": sorted(self.records, key=lambda r: r["destination"])}
        pending = path.with_name(path.name + f".{os.getpid()}.tmp")
        pending.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n")
        os.replace(pending, path)
        return path
