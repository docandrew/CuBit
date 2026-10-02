#!/usr/bin/env bash
# Run inside host-shell.nix. Cross-build probe, NOT a working CuBit KMD.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?CuBit-patched Mesa source required}
build_tree=${2:?fresh isolated build directory required}
host_tree=${3:-$root/tests/mesa-anv/build-host}
host_tree=$(cd "$host_tree" && pwd)
test "$(<"$source_tree/VERSION")" = 26.2.3
test -f "$source_tree/src/intel/vulkan/anv_kmd_backend.h"
grep -q 'void intel_device_info_finalize_runtime' "$source_tree/src/intel/dev/intel_device_info.h"
grep -q 'define DETECT_OS_CUBIT 1' "$source_tree/src/util/detect_os.h"
test -f "$root/userspace/libc/build/sysroot/lib/libc.a"
test ! -e "$build_tree/meson-private/coredata.dat"
test -x "$host_tree/src/compiler/clc/mesa_clc"
test -x "$host_tree/src/compiler/spirv/vtn_bindgen2"
# These are build-host generators from the same pinned Mesa baseline. They
# must not become target libraries or be packaged as CuBit executables.
export PATH="$host_tree/src/compiler/clc:$host_tree/src/compiler/spirv:$PATH"
meson setup "$build_tree" "$source_tree" \
  --cross-file "$root/tests/mesa-anv/cubit-cross.ini" \
  --buildtype=release --wrap-mode=nofallback \
  -Dplatforms=[] -Dgallium-drivers=[] -Dvulkan-drivers=intel \
  -Dglx=disabled -Degl=disabled -Dgbm=disabled \
  -Dllvm=disabled -Dmesa-clc=system \
  -Dvalgrind=disabled -Dlibunwind=disabled -Dlmsensors=disabled \
  -Dzstd=disabled -Dzlib=disabled -Dexpat=disabled -Dshader-cache=disabled
