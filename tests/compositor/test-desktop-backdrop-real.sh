#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/tests/compositor/build/desktop-backdrop-real"
: "${CUBIT_FONT_HOST_ARCHIVE:?select the matching host font archive}"
python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$out/generated"
export C_INCLUDE_PATH="$out/generated${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
(cd "${CUBIT_COMPOSITOR_ALIRE_ROOT:-$root/kernel}" && alr exec -- gprbuild -q -p -P "$root/tests/compositor/desktop_backdrop_real.gpr")
timeout 120 "$out/desktop_backdrop_real_tests" > "$out/result.log" 2>&1
cat "$out/result.log"
