#!/usr/bin/env python3
"""ASan/UBSan exercise of the actual patched SpiderMonkey allocation functions.

Hosted allocation evidence only; native libc/engine integration is separate.
"""
import importlib.util
import pathlib
import subprocess
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("servo_fixes", ROOT / "userspace/servo/crate_fixes.py")
fixes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixes)
crate = "mozjs_sys-153.3.0-0"
path = "mozjs/js/src/gc/Memory.cpp"
upstream = next((ROOT / "userspace/rust/build/servo-work/cargo-home/registry/src").glob(f"*/{crate}/{path}"))
source = upstream.read_text()
for name, old, new, how in fixes.FIXES[crate]["edits"]:
    if name != path:
        continue
    assert old in source
    source = source.replace(old, new, 1) if how == "once" else source.replace(old, new)


def function(signature):
    start = source.index(signature)
    opened = source.index("{", start)
    depth = 1
    end = opened + 1
    while depth:
        if source[end] == "{":
            depth += 1
        elif source[end] == "}":
            depth -= 1
        end += 1
    return source[start:end]


harness = r"""
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <cstdio>
#include <sys/mman.h>
#define MOZ_RELEASE_ASSERT(value, ...) assert(value)
#define MOZ_ASSERT(value, ...) assert(value)
#define MOZ_MAKE_MEM_UNDEFINED(region, length) ((void)0)
static const size_t pageSize = 4096, allocGranularity = 4096;
enum class StallAndRetry : bool { No, Yes };
static size_t mapped = 0, allocations = 0, releases = 0;
static bool fail_next = false;
static void RecordMemoryAlloc(size_t bytes) { mapped += bytes; allocations++; }
static void RecordMemoryFree(size_t bytes) { assert(mapped >= bytes); mapped -= bytes; releases++; }
static size_t OffsetFromAligned(void* ptr, size_t alignment) { return uintptr_t(ptr) % alignment; }
static int checked_memalign(void** out, size_t alignment, size_t bytes) {
    if (fail_next) { fail_next = false; *out = nullptr; return ENOMEM; }
    return posix_memalign(out, alignment, bytes);
}
#define posix_memalign checked_memalign
"""
harness += "\n".join(function(name) for name in [
    "static inline void UnmapInternal(", "void* MapAlignedPages(", "void UnmapPages("])
harness += r"""
int main() {
    // Address-width probes use mmap/UnmapInternal, not GC aligned ownership.
    for (int cycle = 0; cycle < 32; ++cycle) {
        void* probe = mmap(nullptr, 4096, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        assert(probe != MAP_FAILED);
        UnmapInternal(probe, 4096);
        assert(mapped == 0 && allocations == 0 && releases == 0);
    }
    const size_t dimensions[] = {4096, 65536, 1048576};
    for (size_t alignment : dimensions) {
        for (size_t length : dimensions) {
            for (int cycle = 0; cycle < 32; ++cycle) {
                auto* region = static_cast<unsigned char*>(MapAlignedPages(length, alignment, StallAndRetry::No));
                assert(region && uintptr_t(region) % alignment == 0 && mapped == length);
                for (size_t i = 0; i < length; ++i) assert(region[i] == 0);
                for (size_t i = 0; i < length; ++i) region[i] = static_cast<unsigned char>(i + cycle);
                UnmapPages(region, length);
                assert(mapped == 0 && allocations == releases);
            }
            fail_next = true;
            assert(MapAlignedPages(length, alignment, StallAndRetry::No) == nullptr);
            assert(mapped == 0 && allocations == releases);
        }
    }
    assert(allocations == 288 && releases == 288);
    puts("SERVO-GC-ALLOCATOR: PASS 288 paired allocations, 9 injected failures, 32 mmap probes, ASan/UBSan");
}
"""
with tempfile.TemporaryDirectory(prefix="cubit-servo-gc-") as directory:
    directory = pathlib.Path(directory)
    cpp = directory / "gc.cpp"
    binary = directory / "gc-check"
    cpp.write_text(harness)
    subprocess.run(["g++", "-std=c++20", "-O1", "-g", "-Wall", "-Wextra", "-Wno-unused-parameter",
                    "-fsanitize=address,undefined", "-fno-omit-frame-pointer", str(cpp), "-o", str(binary)], check=True)
    subprocess.run([str(binary)], check=True)
