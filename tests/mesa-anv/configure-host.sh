#!/usr/bin/env bash
# Run inside host-shell.nix. Configuration evidence only, no native CuBit ABI.
set -euo pipefail
source_tree=${1:?pinned Mesa source directory required}
build_tree=${2:?fresh out-of-tree build directory required}
test -f "$source_tree/src/intel/vulkan/anv_kmd_backend.h"
test ! -e "$build_tree/meson-private/coredata.dat"
meson setup "$build_tree" "$source_tree" \
  --buildtype=release --wrap-mode=nofallback \
  -Dplatforms=[] -Dgallium-drivers=[] -Dvulkan-drivers=intel \
  -Dglx=disabled -Degl=disabled -Dgbm=disabled \
  -Dllvm=enabled -Dvalgrind=disabled -Dlibunwind=disabled \
  -Dlmsensors=disabled
