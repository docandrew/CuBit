#!/usr/bin/env bash
# Build alloc-bench once per allocator: same source, compiler and flags,
# each allocator linked statically into a fully static binary. CuAlloc is
# the release host library (allocator_host.gpr); jemalloc needs a static
# archive (nixpkgs ships only the shared one): pass its directory in
# CUBIT_BENCH_JEMALLOC_STATIC, e.g. from
#   nix build --impure --expr '(builtins.getFlake (toString ./.)).inputs.nixpkgs
#     .legacyPackages.x86_64-linux.jemalloc.overrideAttrs (o: { dontDisableStatic = true; })'
# glibc's static archives: CUBIT_BENCH_GLIBC_STATIC (nixpkgs glibc.static's
# lib directory).
# CUBIT_BENCH_LTO=on also builds alloc-bench-cualloc-lto: harness and
# CuAlloc in one link-time optimised unit (GNAT's gcc for both).
# Linux-hosted only.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
out="${CUBIT_BENCH_OUT:-$here/build}"
mkdir -p "$out"
# CUBIT_BENCH_CUALLOC_LIB: an already built libcubit_heap.a (another
# revision) instead of this checkout's.
cualloc_lib="${CUBIT_BENCH_CUALLOC_LIB:-}"
if [[ -z "$cualloc_lib" ]]; then
  (cd "$root/kernel" && alr exec -- gprbuild -q -p -P ../userspace/allocator/allocator_host.gpr)
  cualloc_lib="$root/userspace/allocator/build/lib/libcubit_heap.a"
fi
lib() { dirname "$1"; }
# Static glibc (nixpkgs glibc.static) for fully static binaries.
cflags=(-std=gnu11 -O2 -g -static -pthread -L"${CUBIT_BENCH_GLIBC_STATIC:?static glibc lib directory}")
gcc "${cflags[@]}" -DENGINE_GLIBC "$here/alloc-bench.c" -o "$out/alloc-bench-glibc"
gcc "${cflags[@]}" -DENGINE_CUALLOC "$here/alloc-bench.c" \
  "$cualloc_lib" -o "$out/alloc-bench-cualloc"
gcc "${cflags[@]}" -DENGINE_MIMALLOC "$here/alloc-bench.c" \
  "$(lib "$CUBIT_BENCH_MIMALLOC")/libmimalloc.a" -o "$out/alloc-bench-mimalloc"
gcc "${cflags[@]}" -DENGINE_TCMALLOC "$here/alloc-bench.c" \
  "$(lib "$CUBIT_BENCH_TCMALLOC")/libtcmalloc_minimal.a" -lstdc++ -lm -o "$out/alloc-bench-tcmalloc"
gcc "${cflags[@]}" -DENGINE_JEMALLOC "$here/alloc-bench.c" \
  "${CUBIT_BENCH_JEMALLOC_STATIC:?static jemalloc lib directory}/libjemalloc.a" -lm -o "$out/alloc-bench-jemalloc"
gcc -O2 -g -shared -fPIC "$here/record.c" -o "$out/record.so"
# Sized frees (Rust GlobalAlloc dealloc, Ada pools, C23 free_sized) where the
# harness knows the size: CuAlloc's cualloc_free_sized, jemalloc's and
# tcmalloc's free_sized.
gcc "${cflags[@]}" -DENGINE_CUALLOC -DSIZED_FREE "$here/alloc-bench.c" \
  "$cualloc_lib" -o "$out/alloc-bench-cualloc-sized"
gcc "${cflags[@]}" -DENGINE_TCMALLOC -DSIZED_FREE "$here/alloc-bench.c" \
  "$(lib "$CUBIT_BENCH_TCMALLOC")/libtcmalloc_minimal.a" -lstdc++ -lm -o "$out/alloc-bench-tcmalloc-sized"
gcc "${cflags[@]}" -DENGINE_JEMALLOC -DSIZED_FREE "$here/alloc-bench.c" \
  "${CUBIT_BENCH_JEMALLOC_STATIC:?static jemalloc lib directory}/libjemalloc.a" -lm -o "$out/alloc-bench-jemalloc-sized"
if [[ "${CUBIT_BENCH_LTO:-off}" == on ]]; then
  # The allocator's objects with LTO bytecode (and fat code), relocated so
  # the release library is untouched; harness, CuAlloc and the inlined
  # entry points optimised as one program by GNAT's gcc.
  lto="${out:?}/lto"
  rm -rf "${lto:?}"
  mkdir -p "$lto"
  (cd "$root/kernel" && alr exec -- gprbuild -q -p -P ../userspace/allocator/allocator_host.gpr \
     --relocate-build-tree="$lto/tree" -cargs:Ada -flto=auto -ffat-lto-objects)
  mapfile -t objects < <(find "$lto/tree" -name '*.o' | sort)
  (cd "$root/kernel" && alr exec -- gcc "${cflags[@]}" -flto=auto -DENGINE_CUALLOC "$here/alloc-bench.c" \
     "${objects[@]}" -o "$out/alloc-bench-cualloc-lto")
  rm -rf "${lto:?}"
fi
# Each binary must carry its own allocator, statically.
for e in glibc cualloc mimalloc tcmalloc jemalloc; do
  file "$out/alloc-bench-$e" | grep -c 'statically linked' > /dev/null || { echo "not static: $e" >&2; exit 1; }
done
nm "$out/alloc-bench-cualloc" | grep -c ' T cualloc_allocate$' > /dev/null
nm "$out/alloc-bench-mimalloc" | grep -c ' mi_malloc$' > /dev/null
nm "$out/alloc-bench-tcmalloc" | grep -c 'tc_malloc$' > /dev/null
nm "$out/alloc-bench-jemalloc" | grep -c 'je_\|jemalloc' > /dev/null
echo "built in $out"
