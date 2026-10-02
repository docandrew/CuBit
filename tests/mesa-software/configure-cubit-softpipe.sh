#!/usr/bin/env bash
# Run in tests/mesa-anv/host-shell.nix. Target libraries use CuBit libc;
# generation tools run on Linux. No LLVM/JIT or host pkg-config dependencies.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?pinned Mesa source tree required}
build_tree=${2:-$root/tests/mesa-software/target/native-softpipe-cubit}
test -f "$source_tree/src/gallium/drivers/softpipe/sp_screen.c"
grep -q 'define DETECT_OS_CUBIT 1' "$source_tree/src/util/detect_os.h" || {
  echo 'Use prepare-cubit-source.sh to apply the CuBit platform patch first.' >&2
  exit 1
}
test -f "$root/userspace/libc/build/sysroot/lib/libc.a"
meson setup "$build_tree" "$source_tree" \
  --cross-file "$root/tests/mesa-software/cubit-cross.ini" \
  --buildtype=release --wrap-mode=nofallback \
  -Dplatforms=[] -Dgallium-drivers=softpipe -Dvulkan-drivers=[] \
  -Dglx=disabled -Degl=disabled -Dgbm=disabled \
  -Dllvm=disabled -Ddraw-use-llvm=false \
  -Dvalgrind=disabled -Dlibunwind=disabled -Dlmsensors=disabled \
  -Dzstd=disabled -Dzlib=disabled -Dexpat=disabled -Dshader-cache=disabled
