#!/usr/bin/env python3
"""Linux image realization boundary; CCL produces data, never command strings.

All inputs are snapshotted and checked before archive/ISO tools run. This is
not a hermetic builder, signature verifier, or source build-graph executor.
"""
import argparse
from dataclasses import dataclass
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import struct
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[4]
CCL = ROOT / "userspace/ccl/build/image/ccl-image"
CONFIG = ROOT / "userspace/ccl/build/config/ccl-config"
MAX_FILE = 128 * 1024 * 1024
MAX_TOTAL = 512 * 1024 * 1024
MAX_FILES = 512


@dataclass(frozen=True)
class File:
    region: str
    role: str
    artifact: str
    destination: str
    data: bytes
    private: bool = False


def digest(data):
    return hashlib.sha256(data).hexdigest()


def read_regular(path, limit=MAX_FILE):
    # Reject devices/FIFOs before opening; these are artifact inputs, not I/O.
    if not stat.S_ISREG(path.stat().st_mode):
        raise ValueError(f"not a regular artifact: {path}")
    with path.open("rb") as source:
        data = source.read(limit + 1)
    if len(data) > limit:
        raise ValueError(f"artifact exceeds {limit}-byte bound: {path}")
    return data


def safe_destination(path):
    return (bool(path) and len(path) <= 192 and not path.startswith("/")
            and all(part and not part.endswith(".") for part in path.split("/"))
            and all(c.isascii() and (c.isalnum() or c in "._-/") for c in path))


def conflict(left, right):
    # Native optical name lookup must not encounter case aliases either.
    a, b = left.casefold(), right.casefold()
    return a == b or a.startswith(b + "/") or b.startswith(a + "/")


def compile_plan(catalog, profile):
    # Compile exact snapshots, so a later source edit cannot change the
    # meaning of a realization recorded under an earlier source hash.
    documents = [read_regular(p, 8192) for p in (catalog, profile)]
    with tempfile.TemporaryDirectory(prefix="ccl-image-source.") as directory:
        paths = [Path(directory) / name for name in ("catalog.ccl", "profile.ccl")]
        for path, data in zip(paths, documents):
            path.write_bytes(data)
        result = subprocess.run([CCL, *paths], capture_output=True, text=True,
                                timeout=30)
    if result.returncode:
        raise ValueError(result.stderr.strip())
    lines = result.stdout.splitlines()
    header = lines[0].split("\t") if lines else []
    if len(header) != 4 or header[0] != "CCL-IMAGE-1":
        raise ValueError("unsupported CCL plan encoding")
    if header[1] not in ("BOOTSTRAP_ONLY", "OPTICAL_IMAGE"):
        raise ValueError("unsupported layout")
    rows = [line.split("\t") for line in lines[1:]]
    if not 1 <= len(rows) <= 64 or any(len(row) != 6 for row in rows):
        raise ValueError("invalid placement list")
    return header, rows, [digest(data) for data in documents]


def prepare(catalog, profile, inputs, private_roms=None, release=False, root=ROOT):
    header, rows, sources = compile_plan(catalog, profile)
    if release and private_roms is not None:
        raise ValueError("release images cannot include private cartridges")
    files, used, total = [], set(), 0
    root = root.resolve()

    def add(region, role, artifact, destination, data, private=False):
        nonlocal total
        if region not in ("BOOTSTRAP", "OPTICAL") or not safe_destination(destination):
            raise ValueError("invalid resolved placement")
        if any(old.region == region and conflict(old.destination, destination) for old in files):
            raise ValueError(f"overlapping destination: {region}/{destination}")
        if len(files) == MAX_FILES or total + len(data) > MAX_TOTAL:
            raise ValueError("resolved image input bound exceeded")
        total += len(data)
        files.append(File(region, role, artifact, destination, data, private))

    # Reserve whole tree destinations, including presently empty directories.
    for index, row in enumerate(rows):
        region, role, artifact, kind, source, destination = row
        if role not in ("CONTENT", "STARTUP", "SETTINGS", "KERNEL", "BOOT_MENU"):
            raise ValueError("unknown placement role")
        if not safe_destination(destination):
            raise ValueError("invalid destination")
        if any(other[0] == region and conflict(destination, other[5]) for other in rows[:index]):
            raise ValueError("overlapping placement roots")
        if kind == "REPOSITORY_FILE":
            path = (root / source).resolve(strict=True)
            if not path.is_relative_to(root):
                raise ValueError("repository artifact escapes repository")
        elif kind in ("SUPPLIED_FILE", "SUPPLIED_TREE"):
            if source not in inputs:
                raise ValueError(f"missing explicit input: {source}")
            used.add(source)
            path = inputs[source].resolve(strict=True)
        else:
            raise ValueError("unknown artifact source kind")
        if kind == "SUPPLIED_TREE":
            if region != "OPTICAL" or role != "CONTENT" or not path.is_dir():
                raise ValueError("invalid tree input")
            children = []
            for child in path.rglob("*"):
                if len(children) == MAX_FILES:
                    raise ValueError("tree input bound exceeded")
                children.append(child)
            children.sort()
            count = 0
            for child in children:
                if child.is_symlink():
                    raise ValueError("symlinks are not supported in supplied trees")
                if child.is_dir():
                    continue
                add(region, role, artifact,
                    destination + "/" + child.relative_to(path).as_posix(),
                    read_regular(child))
                count += 1
            if not count:
                raise ValueError("empty tree input")
        else:
            add(region, role, artifact, destination, read_regular(path))
    if set(inputs) != used:
        raise ValueError("unused supplied inputs: " + ", ".join(sorted(set(inputs) - used)))

    if private_roms is not None:
        if header[1] != "OPTICAL_IMAGE" or not any(
                f.region == "OPTICAL" and f.destination == "apps/sameboy/00.gb" for f in files):
            raise ValueError("private cartridges require the optical SameBoy profile")
        if not private_roms.is_dir():
            raise ValueError("private cartridge directory does not exist")
        cartridges = []
        for candidate in private_roms.iterdir():
            if candidate.suffix.lower() in (".gb", ".gbc"):
                if len(cartridges) == 15:
                    raise ValueError("at most 15 private cartridges are supported")
                cartridges.append(candidate)
        cartridges.sort(key=lambda p: p.name)
        for index, cartridge in enumerate(cartridges, 1):
            data = read_regular(cartridge, 8 * 1024 * 1024)
            if len(data) < 0x150:
                raise ValueError("private cartridge is too small")
            add("OPTICAL", "CONTENT", f"private-cartridge-{index:02d}",
                f"apps/sameboy/{index:02d}.gb", data, True)

    # Validate native CCL inputs with the same frontend used during boot.
    with tempfile.TemporaryDirectory(prefix="ccl-image-config.") as directory:
        for index, item in enumerate(files):
            if item.role not in ("STARTUP", "SETTINGS"):
                continue
            path = Path(directory) / f"{index}.ccl"
            path.write_bytes(item.data)
            mode = "--dump-startup" if item.role == "STARTUP" else "--dump-system"
            result = subprocess.run([CONFIG, path, mode], text=True, capture_output=True, timeout=30)
            if result.returncode:
                raise ValueError(f"invalid {item.role.lower()} input: {result.stderr.strip()}")
            if item.role == "STARTUP":
                for line in result.stdout.splitlines():
                    executable = line.split()[0]
                    if not any((f.region == "BOOTSTRAP" and f.destination == executable)
                               or (f.region == "OPTICAL" and f.destination == "apps/" + executable)
                               for f in files):
                        raise ValueError(f"startup executable absent from image: {executable}")

    report = {
        "format": "ccl-image-realization-1", "layout": header[1],
        "catalog": header[2], "provider": header[3],
        "catalog_sha256": sources[0], "profile_sha256": sources[1],
        "private_inputs": any(f.private for f in files),
        "files": [{"region": f.region, "role": f.role, "artifact": f.artifact,
                   "destination": f.destination, "size": len(f.data),
                   "sha256": digest(f.data), "private": f.private}
                  for f in sorted(files, key=lambda f: (f.region, f.destination))],
    }
    return files, report


def stage(files, directory):
    bootstrap, optical = directory / "bootstrap", directory / "iso"
    bootstrap.mkdir()
    optical.mkdir()
    for item in files:
        base = bootstrap if item.region == "BOOTSTRAP" else optical
        path = base / item.destination
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(item.data)
        path.chmod(0o644)
        os.utime(path, (0, 0))
    for path in sorted(bootstrap.rglob("*"), reverse=True):
        if path.is_dir():
            os.utime(path, (0, 0))
    return bootstrap, optical


def check_cpio(path, expected):
    """Independently check archive bytes, not just the staging directory."""
    blob = path.read_bytes()
    offset, actual = 0, {}
    while True:
        if blob[offset:offset + 6] != b"070701":
            raise ValueError("invalid generated CPIO")
        size = int(blob[offset + 54:offset + 62], 16)
        length = int(blob[offset + 94:offset + 102], 16)
        mode = int(blob[offset + 14:offset + 22], 16)
        name = blob[offset + 110:offset + 110 + length - 1].decode("ascii")
        if name == "TRAILER!!!":
            break
        start = (offset + 110 + length + 3) & ~3
        if not safe_destination(name) or start + size > len(blob):
            raise ValueError("invalid CPIO member")
        if stat.S_ISREG(mode):
            if name in actual:
                raise ValueError("duplicate CPIO file")
            actual[name] = blob[start:start + size]
        elif not stat.S_ISDIR(mode):
            raise ValueError("unsupported CPIO member type")
        offset = (start + size + 3) & ~3
    if actual != {f.destination: f.data for f in expected if f.region == "BOOTSTRAP"}:
        raise ValueError("generated CPIO disagrees with checked plan")


def publish(source, output):
    # Never truncate an existing working image before validation completes.
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(prefix=output.name + ".", dir=output.parent,
                                     delete=False) as temporary:
        pending = Path(temporary.name)
        try:
            with source.open("rb") as data:
                shutil.copyfileobj(data, temporary)
            temporary.flush()
            os.fsync(temporary.fileno())
        except BaseException:
            pending.unlink()
            raise
    try:
        os.replace(pending, output)
    finally:
        pending.unlink(missing_ok=True)


def check_iso(path, expected):
    """Check declared bytes in the primary ISO9660 tree CuBit actually reads."""
    blob = path.read_bytes()
    visited, actual = set(), {}

    def dual32(offset):
        if offset + 8 > len(blob):
            raise ValueError("truncated ISO field")
        little = struct.unpack_from("<I", blob, offset)[0]
        if little != struct.unpack_from(">I", blob, offset + 4)[0]:
            raise ValueError("inconsistent ISO byte orders")
        return little

    def record(offset, end):
        size = blob[offset]
        if size < 34 or offset + size > end or offset % 2048 + size > 2048:
            raise ValueError("invalid ISO directory record")
        length = blob[offset + 32]
        if 33 + length > size:
            raise ValueError("truncated ISO name")
        name = blob[offset + 33:offset + 33 + length].decode("ascii").removesuffix(";1")
        # Match CuBit's ISO_Records.Matches: no-extension ISO identifiers
        # may carry a terminal dot (e.g. cubit_kernel.;1).
        name = name.removesuffix(".")
        sector, size_bytes = dual32(offset + 2), dual32(offset + 10)
        if sector * 2048 + size_bytes > len(blob) or blob[offset + 25] & 0x80:
            raise ValueError("invalid or multi-extent ISO file")
        return name, sector, size_bytes, bool(blob[offset + 25] & 2), size

    def directory(sector, size, prefix="", depth=0):
        if sector in visited or depth > 16 or len(visited) > 1024:
            raise ValueError("cyclic or oversized ISO directory tree")
        visited.add(sector)
        offset, end = sector * 2048, sector * 2048 + size
        seen = set()
        while offset < end:
            if blob[offset] == 0:
                offset = (offset // 2048 + 1) * 2048
                continue
            name, child, length, is_directory, consumed = record(offset, end)
            offset += consumed
            if name in ("\x00", "\x01"):
                continue
            # GRUB's generated locale filenames include '@'; they need not
            # use the smaller alphabet accepted for CCL-declared paths.
            valid_name = (bool(name) and name not in (".", "..") and "/" not in name
                          and all(" " < c <= "~" and c != "\\" for c in name))
            if not valid_name or name.casefold() in seen:
                raise ValueError(f"ambiguous ISO name: {prefix + name!r}")
            seen.add(name.casefold())
            full = prefix + name
            if is_directory:
                directory(child, length, full + "/", depth + 1)
            else:
                actual[full] = blob[child * 2048:child * 2048 + length]

    primary = next((i * 2048 for i in range(16, min(48, len(blob) // 2048))
                    if blob[i * 2048:i * 2048 + 7] == b"\x01CD001\x01"), None)
    if primary is None:
        raise ValueError("ISO primary volume descriptor absent")
    _, sector, size, is_directory, _ = record(primary + 156, primary + 2048)
    if not is_directory:
        raise ValueError("ISO root is not a directory")
    directory(sector, size)
    for destination, data in expected.items():
        if actual.get(destination) != data:
            observed = actual.get(destination)
            raise ValueError(f"ISO bytes disagree with plan: {destination} "
                             f"(expected {len(data)}, found "
                             f"{len(observed) if observed is not None else 'missing'})")
    # GRUB may generate loader files and its El Torito catalog, not app payload.
    extras = {name for name in actual if name not in expected
              and not name.startswith("boot/grub/") and name != "boot.catalog"}
    if extras:
        raise ValueError("undeclared ISO payload: " + ", ".join(sorted(extras)))


def realize(files, report, output, audit_usb=False):
    optical_layout = report["layout"] == "OPTICAL_IMAGE"
    if output.suffix != (".iso" if optical_layout else ".img"):
        raise ValueError("output extension must match the declared layout")
    # Retain staging and its input hashes for inspection/reproduction.
    directory = Path(tempfile.mkdtemp(prefix="cubit-ccl-image.", dir="/tmp"))
    bootstrap, optical = stage(files, directory)
    archive = directory / "initrd.img"
    names = sorted(p.relative_to(bootstrap).as_posix() for p in bootstrap.rglob("*"))
    with archive.open("wb") as destination:
        subprocess.run(["cpio", "--null", "--reproducible", "--owner=0:0", "-o", "-H", "newc"],
                       input=("\0".join(names) + "\0").encode("ascii"), cwd=bootstrap,
                       stdout=destination, check=True)
    check_cpio(archive, files)
    result = archive
    if optical_layout:
        target = optical / "boot/initrd.img"
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(archive, target)
        result = directory / "image.iso"
        subprocess.run(["grub-mkrescue", "-o", result, optical,
                        "-iso-level", "3", "-full-iso9660-filenames", "-allow-lowercase",
                        "-allow-multidot", "-relaxed-filenames"], check=True)
        expected = {f.destination: f.data for f in files if f.region == "OPTICAL"}
        expected["boot/initrd.img"] = archive.read_bytes()
        check_iso(result, expected)
        if audit_usb:
            subprocess.run(["python3", ROOT / "tests/usb-optical/check-image.py", result], check=True)
    report["output_sha256"] = digest(result.read_bytes())
    report_path = directory / "plan.json"
    report_path.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    publish(result, output)
    publish(report_path, output.with_name(output.name + ".plan.json"))
    print(f"CCL image: {output}\nChecked plan and staging: {directory}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile", type=Path)
    parser.add_argument("--catalog", type=Path, default=ROOT / "images/artifacts.ccl")
    parser.add_argument("--input", action="append", default=[], metavar="NAME=PATH")
    parser.add_argument("--private-rom-dir", type=Path)
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--check-only", action="store_true")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--audit-usb", action="store_true")
    args = parser.parse_args()
    inputs = {}
    try:
        for binding in args.input:
            name, path = binding.split("=", 1)
            if not name or not path or name in inputs:
                raise ValueError("invalid or duplicate input binding")
            inputs[name] = Path(path)
        if not args.check_only and args.output is None:
            raise ValueError("--output is required for realization")
        files, report = prepare(args.catalog, args.profile, inputs,
                                args.private_rom_dir, args.release)
        if args.check_only:
            print(json.dumps(report, indent=2, sort_keys=True))
        else:
            realize(files, report, args.output, args.audit_usb)
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        parser.exit(1, f"ccl-image: {error}\n")


if __name__ == "__main__":
    main()
