#!/usr/bin/env python3
"""Check a fresh and an already-patched Servo dependency cache in private files.

Only reads the upstream registry; never mutates the active native build tree.
"""
import importlib.util
import pathlib
import shutil
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("servo_fixes", ROOT / "userspace/servo/crate_fixes.py")
fixes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixes)
crate = "mozjs_sys-153.3.0-0"
full = fixes.FIXES[crate]
upstream = next((ROOT / "userspace/rust/build/servo-work/cargo-home/registry/src").glob(f"*/{crate}"))

with tempfile.TemporaryDirectory(prefix="cubit-servo-patch-") as directory:
    directory = pathlib.Path(directory)
    registry = directory / "registry"
    for name, *_ in full["edits"]:
        destination = registry / crate / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(upstream / name, destination)
    output = directory / "patched"
    # Emulate a pre-existing cache with the old static-link/platform edits.
    fixes.FIXES = {crate: {"edits": [entry for entry in full["edits"] if entry[0] != "mozjs/js/src/gc/Memory.cpp"]}}
    fixes.apply(str(registry), str(output))
    fixes.FIXES = {crate: full}
    fixes.apply(str(registry), str(output))
    memory = output / crate / "mozjs/js/src/gc/Memory.cpp"
    original = (upstream / "mozjs/js/src/gc/Memory.cpp").read_text()
    patched = memory.read_text()
    assert "#if 1  // CuBit: aligned libc allocations" in patched
    assert "#elif 1  // CuBit: MapAlignedPages" not in patched
    assert "// CuBit: only MapAlignedPages allocations use libc ownership." in patched
    start = original.index("static inline void UnmapInternal")
    end = original.index("template <Commit", start)
    patched_start = patched.index("static inline void UnmapInternal")
    assert original[start:end] == patched[patched_start:patched.index("template <Commit", patched_start)]
    assert "  RecordMemoryAlloc(length);\n  return region;\n#else\n\n#  ifdef JS_64BIT" in patched
    assert "return MapAlignedPages(length, alignment, StallAndRetry::No);" in patched
    # The adaptation must not silently weaken protection or decommit behavior.
    assert patched[patched.index("static inline void ProtectMemory"): ] == original[original.index("static inline void ProtectMemory"): ]
    for function in ["bool MarkPagesUnusedSoft", "bool MarkPagesUnusedHard"]:
        assert patched[patched.index(function):patched.index("size_t GetPageFaultCount")] == original[original.index(function):original.index("size_t GetPageFaultCount")]
    snapshot = {p: (p.read_bytes(), p.stat().st_mtime_ns) for p in output.rglob("*") if p.is_file()}
    fixes.apply(str(registry), str(output))
    assert all((p.read_bytes(), p.stat().st_mtime_ns) == before for p, before in snapshot.items())
    # Previously built v2 cache used free too broadly: migrate it safely.
    memory.write_text(patched.replace("#elif defined(__wasi__)\n  free(region);",
        "#elif 1  // CuBit: MapAlignedPages returns a libc-owned allocation.\n  free(region);"))
    fixes.apply(str(registry), str(output))
    assert memory.read_text() == patched
    fresh = directory / "fresh"
    fixes.apply(str(registry), str(fresh))
    assert (fresh / crate / "mozjs/js/src/gc/Memory.cpp").read_bytes() == memory.read_bytes()
print("SERVO-PORT-PATCHES: PASS fresh existing-cache idempotent protection-unchanged")
