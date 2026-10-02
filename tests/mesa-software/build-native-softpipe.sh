#!/usr/bin/env bash
# Native compile/link only. Run within the Mesa Nix shell under build.lock.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?pinned Mesa source tree required}
build_tree=${2:-$root/tests/mesa-software/target/native-softpipe-cubit}
libs=(src/gallium/drivers/softpipe/libsoftpipe.a
      src/gallium/winsys/sw/null/libws_null.a
      src/gallium/auxiliary/libgallium.a src/compiler/nir/libnir.a
      src/compiler/libcompiler.a src/util/libmesa_util.a
      src/util/blake3/libblake3.a src/util/libmesa_util_clflush.a
      src/util/libmesa_util_clflushopt.a src/util/libmesa_util_simd.a
      src/c11/impl/libmesa_util_c11.a)
ninja -C "$build_tree" -j4 "${libs[@]}"
objects=()
for lib in "${libs[@]}"; do objects+=("$build_tree/$lib"); done
"$root/userspace/ccl/build/manifest/ccl-manifest" \
  "$root/userspace/ccl/catalogs/native-runtime-services.ccl" \
  "$root/tests/mesa-software/native-manifest.ccl" > "$build_tree/native-manifest.S"
as --64 "$build_tree/native-manifest.S" -o "$build_tree/native-manifest.o"
"$root/userspace/libc/cubit-cc" -O2 -D_GNU_SOURCE -D__cubit__ -DHAVE_ENDIAN_H \
  -I"$source_tree/include" -I"$source_tree/src" \
  -I"$source_tree/src/gallium/include" -I"$source_tree/src/gallium/drivers" \
  -I"$source_tree/src/gallium/auxiliary" \
  -I"$source_tree/src/gallium/winsys/sw" -I"$build_tree/src" \
  -c "$root/tests/mesa-software/native-softpipe.c" -o "$build_tree/native-softpipe.o"
"$root/userspace/libc/cubit-cc" -O2 -c \
  "$root/tests/mesa-software/allocation-diagnostics.c" -o "$build_tree/allocation-diagnostics.o"
"$root/userspace/libc/cubit-cc" -O2 -D_GNU_SOURCE -c \
  "$root/tests/mesa-software/file-diagnostics.c" -o "$build_tree/file-diagnostics.o"
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/allocation-diagnostics.o" \
  "$build_tree/file-diagnostics.o" -Wl,--wrap=fopen,--wrap=open \
  -Wl,--wrap=malloc,--wrap=calloc,--wrap=aligned_alloc,--wrap=posix_memalign \
  "$build_tree/native-softpipe.o" -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/native-manifest.o" \
  -o "$build_tree/native-softpipe.app"
