#!/usr/bin/env python3
"""Build a generated ext2 development disk, then verify every payload byte.

debugfs can exit successfully on ENOSPC; file size alone is not evidence that
the payload was written. Publish only a completely populated, checked image.
"""
import argparse
import hashlib
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import tempfile


def sha256(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').digest()


def build(output, files):
    payloads = {}
    for destination, source in files:
        path = PurePosixPath(destination)
        if (path.is_absolute() or not path.parts or '..' in path.parts
                or str(path) != destination
                or any(not (c.isalnum() or c in '/._-') for c in destination)
                or destination in payloads):
            raise ValueError(f'invalid/duplicate image path: {destination}')
        if not source.is_file():
            raise ValueError(f'missing payload: {source}')
        payloads[destination] = source
    # Metadata, future app overlays, and a useful writable workspace. This is
    # image capacity, not guest RAM consumption (the image stays disk-backed).
    quantum = 64 * 1024 * 1024
    size = max(2 * quantum, ((sum(p.stat().st_size for p in payloads.values())
                              * 2 + quantum - 1) // quantum) * quantum)
    output = output.resolve()
    with tempfile.TemporaryDirectory(prefix='.cubit-disk-', dir=output.parent) as tmp:
        root = Path(tmp)
        stage = root / 'root'
        (stage / 'work').mkdir(parents=True)
        expected = {}
        for destination, source in payloads.items():
            target = stage / destination
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, target)
            target.chmod(source.stat().st_mode & 0o777)
            expected[destination] = sha256(target)
        candidate = root / 'disk.img'
        with candidate.open('wb') as stream:
            stream.truncate(size)
        subprocess.run(['mke2fs', '-q', '-t', 'ext2', '-F', '-d', str(stage),
                        str(candidate)], check=True)
        subprocess.run(['e2fsck', '-fn', str(candidate)], check=True,
                       stdout=subprocess.DEVNULL)
        dumped = root / 'payload'
        for destination, digest in expected.items():
            dumped.unlink(missing_ok=True)
            result = subprocess.run(
                ['debugfs', '-R', f'dump /{destination} "{dumped}"', str(candidate)],
                capture_output=True, text=True, check=True)
            if not dumped.is_file() or sha256(dumped) != digest:
                raise RuntimeError(f'image payload mismatch: {destination}\n{result.stderr}')
        os.replace(candidate, output)
    print(f'ext2: {len(payloads)} payloads verified, {size // (1024 * 1024)} MiB image')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('--boot', type=Path, nargs='*', default=[])
    parser.add_argument('--file', action='append', default=[], metavar='DEST=SOURCE')
    args = parser.parse_args()
    files = [(path.name, path) for path in args.boot]
    for item in args.file:
        destination, source = item.split('=', 1)
        files.append((destination, Path(source)))
    build(args.output, files)


if __name__ == '__main__':
    main()
