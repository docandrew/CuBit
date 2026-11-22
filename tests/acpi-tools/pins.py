#!/usr/bin/env python3
"""Refresh declared file pins; changing a hash does not establish a test result."""

import argparse
import copy
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import sys
import tempfile


SHA256 = re.compile(r"[0-9a-f]{64}\Z")
ROOT_NAMES = {"tests", "userspace", "shared", "tools", "docs"}


class PinError(Exception):
    pass


def digest(data):
    return hashlib.sha256(data).hexdigest()


def unlinked(path):
    """Check the supplied spelling, including ancestors, before resolving it."""
    path = Path(os.path.abspath(path))
    for item in [path, *path.parents]:
        if item.is_symlink():
            raise PinError(f"symbolic link refused: {item}")
    return path


def pointer(parts):
    return "".join("/" + str(p).replace("~", "~0").replace("/", "~1") for p in parts)


def split_pointer(value):
    if value == "":
        return []
    if not value.startswith("/") or re.search(r"~(?![01])", value):
        raise PinError(f"invalid JSON pointer: {value}")
    return [p.replace("~1", "/").replace("~0", "~") for p in value[1:].split("/")]


def at(document, parts):
    try:
        for part in parts:
            document = document[int(part)] if isinstance(document, list) else document[part]
    except (KeyError, IndexError, ValueError, TypeError) as error:
        raise PinError(f"missing JSON pointer: {pointer(parts)}") from error
    return document


def assignment(value):
    try:
        if "=" not in value:
            raise PinError("expected JSON_POINTER=PATH")
        key, path = value.split("=", 1)
        if not path:
            raise PinError("empty input path")
        return split_pointer(key), path
    except PinError as error:
        raise argparse.ArgumentTypeError(str(error)) from error


def load_json(data):
    def pairs(items):
        result = {}
        for key, value in items:
            if key in result:
                raise PinError(f"duplicate JSON key: {key}")
            result[key] = value
        return result
    return json.loads(data, object_pairs_hook=pairs)


class Planner:
    def __init__(self, root, manifests, allow_external=False, bases=()):
        self.root = unlinked(root)
        self.allow_external = allow_external
        self.bases = [(tuple(parts), path) for parts, path in bases]
        self.original = {}
        self.documents = {}
        self.reads = {}
        self.planned = {}
        self.active = set()
        self.changes = []
        self.pins = 0
        for name in manifests:
            path = self.locate(name, self.root)
            if not path.is_relative_to(self.root):
                raise PinError("manifests must be inside --root")
            if path.suffix != ".json" or path in self.documents:
                raise PinError(f"expected distinct JSON manifests: {name}")
            raw = self.read(path)
            self.original[path] = raw
            self.documents[path] = load_json(raw)

    def locate(self, name, base):
        if not isinstance(name, str) or not name or "\\" in name:
            raise PinError(f"invalid file path: {name!r}")
        relative = Path(name)
        if ".." in relative.parts:
            raise PinError(f"parent traversal refused: {name}")
        path = unlinked(relative if relative.is_absolute() else base / relative)
        if not path.is_relative_to(self.root) and not self.allow_external:
            raise PinError(f"external input requires --allow-external: {name}")
        return path

    def read(self, path):
        if not path.is_file():
            raise PinError(f"missing regular input: {path}")
        data = path.read_bytes()
        if path in self.reads and self.reads[path] != digest(data):
            raise PinError(f"input changed while planning: {path}")
        self.reads[path] = digest(data)
        return data

    def source(self, manifest, name, parts, named_fixture=False):
        base = self.root
        if named_fixture:
            base = manifest.parent / "fixtures"
        elif parts and parts[0] in {"helpers", "assets", "fixtures"}:
            base = manifest.parent
        if Path(name).parts and Path(name).parts[0] in ROOT_NAMES:
            base = self.root
        applicable = [(prefix, directory) for prefix, directory in self.bases
                      if tuple(parts[:len(prefix)]) == prefix]
        if applicable:
            _, directory = max(applicable, key=lambda item: len(item[0]))
            base = self.locate(directory, self.root)
        return self.locate(name, base)

    def edit(self, additions, removals):
        if (additions or removals) and len(self.documents) != 1:
            raise PinError("--add/--remove require one manifest")
        for document in self.documents.values():
            for parts, name in removals:
                container = at(document, parts)
                if isinstance(container, list):
                    matches = [row for row in container if isinstance(row, dict) and row.get("path") == name]
                    if len(matches) != 1:
                        raise PinError(f"--remove requires exactly one row: {name}")
                    container.remove(matches[0])
                elif isinstance(container, dict) and name in container:
                    del container[name]
                else:
                    raise PinError(f"missing removal: {name}")
            for parts, name in additions:
                container = at(document, parts)
                if isinstance(container, list):
                    if any(isinstance(row, dict) and row.get("path") == name for row in container):
                        raise PinError(f"duplicate addition: {name}")
                    # Additional classification fields can be edited explicitly before refresh.
                    container.append({"path": name, "sha256": "0" * 64})
                elif isinstance(container, dict) and name not in container:
                    container[name] = "0" * 64
                else:
                    raise PinError(f"invalid or duplicate addition: {name}")

    def refresh_pin(self, manifest, container, key, name, parts, named_fixture=False):
        old = container[key]
        if not isinstance(old, str) or not SHA256.fullmatch(old):
            raise PinError(f"invalid SHA-256 at {manifest}:{pointer(parts + [key])}")
        path = self.source(manifest, name, parts, named_fixture)
        data = self.plan(path) if path in self.documents else self.read(path)
        new = digest(data)
        self.pins += 1
        if old != new:
            self.changes.append({"manifest": str(manifest.relative_to(self.root)),
                                 "pointer": pointer(parts + [key]), "path": name,
                                 "before": old, "after": new})
            container[key] = new

    def walk(self, manifest, value, parts):
        if isinstance(value, list):
            for index, item in enumerate(value):
                self.walk(manifest, item, parts + [str(index)])
        elif isinstance(value, dict):
            handled = set()
            if "sha256" in value:
                field = next((key for key in ("repository_path", "path", "name") if key in value), None)
                if field is None:
                    raise PinError(f"sha256 without file reference: {manifest}:{pointer(parts)}")
                self.refresh_pin(manifest, value, "sha256", value[field], parts, field == "name")
                handled.add("sha256")
            for key, item in value.items():
                if key in handled:
                    continue
                if key.endswith("_sha256"):
                    stem = key[:-7]
                    field = next((f for f in (stem, stem + "_path") if isinstance(value.get(f), str)), None)
                    if field:
                        self.refresh_pin(manifest, value, key, value[field], parts)
                elif isinstance(item, str) and SHA256.fullmatch(item) and (
                        "/" in key or Path(key).suffix):
                    self.refresh_pin(manifest, value, key, key, parts)
                elif isinstance(item, (dict, list)):
                    self.walk(manifest, item, parts + [key])

    def plan(self, manifest):
        if manifest in self.planned:
            return self.planned[manifest]
        if manifest in self.active:
            raise PinError(f"cyclic manifest hashes: {manifest}")
        self.active.add(manifest)
        document = self.documents[manifest]
        before = copy.deepcopy(document)
        self.walk(manifest, document, [])
        original = self.original[manifest]
        # Preserve formatting and bytes for a true no-op, including minified catalogs.
        data = original if document == before and document == load_json(original) else (
            json.dumps(document, indent=2, ensure_ascii=False, allow_nan=False) + "\n").encode()
        self.active.remove(manifest)
        self.planned[manifest] = data
        return data

    def verify_reads(self):
        for path, expected in self.reads.items():
            unlinked(path)
            if digest(path.read_bytes()) != expected:
                raise PinError(f"input changed before publication: {path}")

    def publish(self):
        # Every nominated manifest and dependency has been checked before any write.
        self.verify_reads()
        for path in sorted(self.planned):
            data = self.planned[path]
            if data == self.original[path]:
                continue
            name = None
            try:
                with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".pins-", delete=False) as output:
                    name = output.name
                    output.write(data)
                    os.fchmod(output.fileno(), stat.S_IMODE(path.stat().st_mode))
                os.replace(name, path)
            finally:
                if name and os.path.exists(name):
                    os.unlink(name)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifests", nargs="+", help="explicit repository-relative JSON files")
    parser.add_argument("--root", type=Path, default=Path(__file__).absolute().parents[2])
    action = parser.add_mutually_exclusive_group(required=True)
    action.add_argument("--check", action="store_true", help="read only; exit 1 on stale pins")
    action.add_argument("--write", action="store_true", help="refresh declared file pins")
    parser.add_argument("--allow-external", action="store_true", help="also read absolute tool inputs")
    parser.add_argument("--base", action="append", default=[], type=assignment, metavar="POINTER=DIRECTORY")
    parser.add_argument("--add", action="append", default=[], type=assignment, metavar="POINTER=PATH")
    parser.add_argument("--remove", action="append", default=[], type=assignment, metavar="POINTER=PATH")
    args = parser.parse_args()
    try:
        planner = Planner(args.root, args.manifests, args.allow_external, args.base)
        planner.edit(args.add, args.remove)
        for manifest in sorted(planner.documents):
            planner.plan(manifest)
        if planner.pins == 0:
            raise PinError("no declared file pins found")
        planner.verify_reads()
        changed = sum(data != planner.original[path] for path, data in planner.planned.items())
        if args.write:
            planner.publish()
        print(json.dumps({"manifests": len(planner.documents), "pins": planner.pins,
                          "changed_manifests": changed, "written_manifests": changed if args.write else 0,
                          "changes": planner.changes}, sort_keys=True))
        return 1 if args.check and changed else 0
    except (PinError, OSError, ValueError, TypeError) as error:
        print(f"pins: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
