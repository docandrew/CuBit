#!/usr/bin/env python3
"""Reuse a verified combined Mesa build or build a new immutable generation.

Caller holds the project build lock and has built runtime/libc. This helper
never deletes generations, stages executables, or interprets linking as execution.
"""
import argparse
import fcntl
import json
import os
from pathlib import Path
import subprocess
import sys
import uuid


def ensure(root, cache, jobs=4, verify_fn=None, build_fn=None):
    root, cache = Path(root).resolve(), Path(cache).resolve()
    if verify_fn is None:
        sys.path.insert(0, str(root / 'tools'))
        from verify_native_mesa_build import check
        verify_fn = check
    if build_fn is None:
        def build_fn(directory):
            subprocess.run([sys.executable, str(root / 'tools/build_native_mesa.py'),
                            str(directory), '--jobs', str(jobs)], cwd=root, check=True)
    cache.mkdir(parents=True, exist_ok=True)
    pointer = cache / 'current.json'
    def validate(directory):
        record = json.loads((directory / 'build.json').read_text())
        inputs = record['inputs_sha256']
        # A build from another checkout may verify against its own old files;
        # it must not silently become this checkout's normal dependency.
        for relative in ('tools/build_native_mesa.py',
                         'userspace/runtime/adalib/libgnat-user.a',
                         'userspace/libc/build/sysroot/lib/libc.a'):
            if str(root / relative) not in inputs:
                raise ValueError('Mesa dependency belongs to another source/runtime root')
        result = verify_fn(directory, directory / 'bundle', True)
        if result.get('status') != 'VERIFIED_LINK_ONLY' or result.get('combined') is not True:
            raise ValueError('Combined Mesa dependency did not verify')
    with (cache / 'dependency.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        if pointer.exists():
            try:
                value = json.loads(pointer.read_text())
                name = value['generation']
                if value.get('version') != 1 or not isinstance(name, str) or not name.startswith('generation-') or Path(name).name != name:
                    raise ValueError('Invalid Mesa generation pointer')
                directory = cache / name
                if directory.is_symlink():
                    raise ValueError('Mesa generation must be a real directory')
                validate(directory)
                return directory
            except (ValueError, OSError, KeyError, TypeError) as error:
                print('Mesa dependency needs rebuilding: ' + str(error), file=sys.stderr)
        directory = cache / ('generation-' + uuid.uuid4().hex)
        build_fn(directory)
        validate(directory)
        temporary = cache / ('.current-' + uuid.uuid4().hex + '.json')
        try:
            temporary.write_text(json.dumps({'version': 1, 'generation': directory.name}) + '\n')
            os.replace(temporary, pointer)
        finally:
            temporary.unlink(missing_ok=True)
        return directory


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--root', type=Path, required=True)
    p.add_argument('--cache', type=Path, required=True)
    p.add_argument('--jobs', type=int, default=4)
    args = p.parse_args()
    if not os.environ.get('IN_NIX_SHELL') or not 1 <= args.jobs <= 64:
        p.error('Use Nix and --jobs between 1 and 64')
    directory = ensure(args.root, args.cache, args.jobs)
    print(json.dumps({'build': str(directory / 'native'),
                      'source': str(directory / 'source'),
                      'bundle': str(directory / 'bundle')}))

if __name__ == '__main__':
    main()
