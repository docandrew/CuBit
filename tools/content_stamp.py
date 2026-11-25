#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Write STAMP, a listing of every file under each DIRECTORY by SHA-256,
only when that listing changed (BLD-001).

make compares times, and an image such as the ISO packs a whole directory
that other tools touch without changing it (the headless tests copy the
kernel in and edit grub.cfg and put it back; the sign step rewrites a
program with the same bytes). A rule that depends on STAMP instead is
redone only when a file was added, removed or changed in content.

Files are hashed again only when their size, inode or modification or
change time differ from STAMP's cache (STAMP.cache), so an unchanged tree
costs one stat per file.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path


def stat_key(info):
    return [info.st_size, info.st_ino, info.st_mtime_ns, info.st_ctime_ns]


def listing(directories, cache):
    entries, fresh = [], {}
    for directory in directories:
        for root, subdirectories, files in os.walk(directory):
            subdirectories.sort()
            for name in sorted(files):
                path = os.path.join(root, name)
                info = os.lstat(path)
                key = stat_key(info)
                known = cache.get(path)
                if known is not None and known[0] == key:
                    digest = known[1]
                elif os.path.islink(path):
                    digest = "link:" + os.readlink(path)
                else:
                    with open(path, "rb") as stream:
                        digest = hashlib.file_digest(stream, "sha256").hexdigest()
                fresh[path] = [key, digest]
                entries.append(f"{digest} {oct(info.st_mode & 0o7777)} {path}")
    return "\n".join(entries) + "\n", fresh


def replace(path, text):
    pending = path.with_name(path.name + f".{os.getpid()}.tmp")
    pending.write_text(text)
    os.replace(pending, path)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("stamp", type=Path)
    parser.add_argument("directories", nargs="+", type=Path)
    args = parser.parse_args()
    cache_path = args.stamp.with_name(args.stamp.name + ".cache")
    try:
        cache = json.loads(cache_path.read_text())
    except (OSError, ValueError):
        cache = {}
    text, fresh = listing(args.directories, cache)
    args.stamp.parent.mkdir(parents=True, exist_ok=True)
    try:
        unchanged = args.stamp.read_text() == text
    except OSError:
        unchanged = False
    if not unchanged:
        replace(args.stamp, text)
    if fresh != cache:
        replace(cache_path, json.dumps(fresh, separators=(",", ":")))


if __name__ == "__main__":
    main()
