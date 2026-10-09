"""Packaging identity guard, not execution, admission, or GPU validation."""
import argparse
import hashlib
import json
from pathlib import Path


def digest(data):
    return hashlib.sha256(data).hexdigest()


def contained(root, name):
    path = Path(name)
    if path.is_absolute() or not path.parts or any(p in (".", "..") for p in path.parts):
        raise ValueError("invalid artifact path")
    current = root
    for part in path.parts:
        current = current / part
        if current.is_symlink():
            raise ValueError("symlink artifact input")
    return current


def verify(directory):
    root = Path(directory).resolve(strict=True)
    record = json.loads(contained(root, "compositor-result.json").read_bytes())
    for key, value in {"status": "LINKED", "backend": "vulkan-runtime-dispatch",
                       "gpu_drawing_enabled": True}.items():
        if type(record.get(key)) is not type(value) or record[key] != value:
            raise ValueError("invalid compositor field: " + key)
    binary = contained(root, record["binary"])
    data = binary.read_bytes()
    if not data.startswith(b"\x7fELF") or digest(data) != record["binary_sha256"]:
        raise ValueError("binary identity mismatch")
    if type(record["binary_bytes"]) is not int or len(data) != record["binary_bytes"]:
        raise ValueError("binary size mismatch")
    manifest = contained(root, record["source_manifest"]).read_bytes()
    if digest(manifest) != record["source_manifest_sha256"]:
        raise ValueError("source manifest identity mismatch")
    sources = json.loads(manifest)
    if not isinstance(sources, dict) or not sources:
        raise ValueError("empty source provenance")
    for name, expected in sources.items():
        if digest(contained(root, name).read_bytes()) != expected:
            raise ValueError("source drift: " + name)
    return binary


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory")
    args = parser.parse_args()
    try:
        print(verify(args.directory))
    except (OSError, ValueError, TypeError, KeyError) as error:
        parser.exit(1, "Compositor artifact rejected: " + str(error) + "\n")
