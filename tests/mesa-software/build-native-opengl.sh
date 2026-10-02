#!/usr/bin/env bash
# Build under the Mesa Nix shell and shared build lock. Never execute on host.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?patched Mesa source tree required}
build_tree=${2:-$root/tests/mesa-software/target/native-softpipe-cubit}
bash "$root/tests/mesa-software/build-native-softpipe.sh" "$source_tree" "$build_tree"
libs=(src/mesa/libmesa.a src/mesa/libmesa_sse41.a
      src/mesa/glapi/glapi/libglapi_bridge.a src/mesa/glapi/shared-glapi/libglapi.a
      src/compiler/glsl/libglsl.a src/compiler/glsl/glcpp/libglcpp.a
      src/compiler/glsl/libglsl_util.a src/compiler/spirv/libvtn.a
      src/gallium/drivers/softpipe/libsoftpipe.a
      src/gallium/winsys/sw/null/libws_null.a
      src/gallium/auxiliary/libgallium.a src/compiler/nir/libnir.a
      src/compiler/libcompiler.a src/util/libmesa_util.a
      src/util/blake3/libblake3.a src/util/libmesa_util_clflush.a
      src/util/libmesa_util_clflushopt.a src/util/libmesa_util_simd.a
      src/c11/impl/libmesa_util_c11.a)
ninja -C "$build_tree" -j4 "${libs[@]}"
python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
  "$root/tests/mesa-software/native-opengl.c" "$build_tree/native-opengl.o"
for probe in buffer-winsys buffer-target-test native-buffer native-mesa-window; do
  python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
    "$root/tests/mesa-software/$probe.c" "$build_tree/$probe.o"
done
objects=()
for lib in "${libs[@]}"; do objects+=("$build_tree/$lib"); done
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/allocation-diagnostics.o" "$build_tree/file-diagnostics.o" \
  -Wl,--wrap=fopen,--wrap=open \
  -Wl,--wrap=malloc,--wrap=calloc,--wrap=aligned_alloc,--wrap=posix_memalign \
  "$build_tree/native-opengl.o" \
  -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/native-manifest.o" -o "$build_tree/native-opengl.app"
python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
  "$root/tests/mesa-software/native-opengl.c" "$build_tree/native-context-lifetime.o" -DCUBIT_CONTEXT_ITERATIONS=3
python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
  "$root/tests/mesa-software/lifetime-diagnostics.c" "$build_tree/lifetime-diagnostics.o"
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/allocation-diagnostics.o" \
  -Wl,--wrap=malloc,--wrap=calloc,--wrap=aligned_alloc,--wrap=posix_memalign \
  "$build_tree/native-context-lifetime.o" "$build_tree/lifetime-diagnostics.o" -Wl,--wrap=munmap \
  -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/native-manifest.o" -o "$build_tree/native-context-lifetime.app"
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/allocation-diagnostics.o" \
  -Wl,--wrap=malloc,--wrap=calloc,--wrap=aligned_alloc,--wrap=posix_memalign \
  "$build_tree/native-buffer.o" "$build_tree/buffer-winsys.o" "$build_tree/buffer-target-test.o" \
  -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/native-manifest.o" -o "$build_tree/native-buffer.app"
"$root/userspace/ccl/build/manifest/ccl-manifest" \
  "$root/userspace/ccl/catalogs/native-runtime-services.ccl" \
  "$root/tests/mesa-software/window-manifest.ccl" > "$build_tree/window-manifest.S"
as --64 "$build_tree/window-manifest.S" -o "$build_tree/window-manifest.o"
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/native-mesa-window.o" "$build_tree/buffer-winsys.o" \
  -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/window-manifest.o" -o "$build_tree/native-mesa-window.app"
python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
  "$root/tests/mesa-software/native-mesa-window.c" "$build_tree/native-mesa-cube.o" -DMESA_WINDOW_CUBE=1
python3 "$root/tests/mesa-software/compile-native-probe.py" "$build_tree" \
  "$root/tests/mesa-software/cube-scene.c" "$build_tree/cube-scene.o"
"$root/userspace/libc/cubit-c++" -Wl,--gc-sections \
  "$build_tree/native-mesa-cube.o" "$build_tree/cube-scene.o" "$build_tree/buffer-winsys.o" \
  -Wl,-Map,"$build_tree/native-mesa-cube.map",--cref \
  -Wl,--start-group "${objects[@]}" -Wl,--end-group \
  --manifest "$build_tree/window-manifest.o" -o "$build_tree/native-mesa-cube.app"
