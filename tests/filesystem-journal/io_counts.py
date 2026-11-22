#!/usr/bin/env python3
"""Device commands per operation on a journaled volume (io_counts.adb):
1000 creates with a 4 KiB write each, then 1000 unlinks, on an ext3 image;
until a commit neither may write to the device (reads that fill the cache
only); then 100 append+fsync pairs, each at most one device flush (the
log ring defers checkpoints, as jbd2). The result must be e2fsck-clean."""
import os
from pathlib import Path
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
PROGRAM = Path(os.environ.get("IO_COUNTS", HERE / "build/crash/io_counts"))


def run(*args):
    result = subprocess.run([str(a) for a in args], stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    if result.returncode:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result.stdout


with tempfile.TemporaryDirectory(prefix="cubit-io-") as tmp:
    image = Path(tmp) / "ext3.img"
    with image.open("wb") as f:
        f.truncate(64 * 1024 * 1024)
    run("mke2fs", "-q", "-t", "ext3", "-b", 4096, "-F", image)
    out = run(PROGRAM, image)
    run("e2fsck", "-fn", image)
    line = next(l for l in out.splitlines() if l.startswith("CREATE COMMANDS"))
    sync = next(l for l in out.splitlines() if l.startswith("FSYNC REQUESTS"))
    print(f"JOURNAL-IO-COUNT: PASS 1000 creates+4 KiB writes, 1000 unlinks: "
          f"{line.lower()}; per 4 KiB append+fsync: {sync.lower()}")
