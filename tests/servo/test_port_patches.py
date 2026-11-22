#!/usr/bin/env python3
"""Check crate_fixes.py's copy of the mozjs_sys fork, in a private directory.

SpiderMonkey's GC chunk allocation is the fork's CuBit change (aligned libc
allocations, no partial munmap); protection and decommit stay upstream's,
compared with the fork's root (the crates.io package). A second run leaves
the copy untouched (bytes and mtimes: cargo's cache stays valid), and a copy
made from another pin is rebuilt from the checkout.
"""
import importlib.util
import os
import pathlib
import subprocess
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("servo_fixes", ROOT / "userspace/servo/crate_fixes.py")
fixes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixes)
sources = pathlib.Path(fixes.fetch(ROOT / "userspace/rust/build/servo-crate-downloads"))
crate = "mozjs_sys-153.3.0-0"
fork = sources / (crate + ".fork")
# The fork's base (the crates.io package): fetch the whole (short) history.
if subprocess.check_output(["git", "-C", str(fork), "rev-parse", "--is-shallow-repository"], text=True).strip() == "true":
    fork_line = next(line.split() for line in (ROOT / "userspace/servo/FORKS").read_text().splitlines()
                     if line.startswith("mozjs "))
    subprocess.run(["git", "-C", str(fork), "fetch", "-q", "--unshallow",
                    os.environ.get("PENNY_MOZJS_URL", fork_line[2]), fork_line[1]], check=True)
root = subprocess.check_output(["git", "-C", str(fork), "rev-list", "--max-parents=0", "HEAD"], text=True).strip()
original = subprocess.check_output(["git", "-C", str(fork), "show", f"{root}:mozjs/js/src/gc/Memory.cpp"], text=True)

with tempfile.TemporaryDirectory(prefix="cubit-servo-patch-") as directory:
    output = pathlib.Path(directory) / "patched"
    fixes.FIXES, fixes.PINNED = {}, {}
    fixes.apply(str(sources), str(output))
    memory = output / crate / "mozjs/js/src/gc/Memory.cpp"
    patched = memory.read_text()
    assert "#if 1  // CuBit: aligned libc allocations" in patched
    assert "// CuBit: only MapAlignedPages allocations use libc ownership." in patched
    start = original.index("static inline void UnmapInternal")
    end = original.index("template <Commit", start)
    patched_start = patched.index("static inline void UnmapInternal")
    assert original[start:end] == patched[patched_start:patched.index("template <Commit", patched_start)]
    assert "  RecordMemoryAlloc(length);\n  return region;\n#else\n\n#  ifdef JS_64BIT" in patched
    assert "return MapAlignedPages(length, alignment, StallAndRetry::No);" in patched
    # The adaptation must not silently weaken protection or decommit behavior.
    assert patched[patched.index("static inline void ProtectMemory"):] == original[original.index("static inline void ProtectMemory"):]
    for function in ["bool MarkPagesUnusedSoft", "bool MarkPagesUnusedHard"]:
        assert patched[patched.index(function):patched.index("size_t GetPageFaultCount")] == original[original.index(function):original.index("size_t GetPageFaultCount")]
    assert not (output / crate / ".cargo_vcs_info.json").exists() and not (output / crate / ".git").exists()
    snapshot = {p: (p.read_bytes(), p.stat().st_mtime_ns) for p in output.rglob("*") if p.is_file()}
    fixes.apply(str(sources), str(output))
    assert all((p.read_bytes(), p.stat().st_mtime_ns) == before for p, before in snapshot.items())
    # A copy from another pin (its stamp) is rebuilt from the checkout.
    (output / crate / fixes.STAMP).write_text("another-commit")
    memory.write_text("stale")
    fixes.apply(str(sources), str(output))
    assert memory.read_text() == patched
print("SERVO-PORT-PATCHES: PASS fork copy, second run untouched, other pin rebuilt, protection unchanged")
