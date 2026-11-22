#!/usr/bin/env bash
# In vulkan-affine-shell.nix: production allocator and SPARK owner, hosted only.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/tests/compositor/build/vulkan-owned"
python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$out/generated"
export C_INCLUDE_PATH="$out/generated${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
cc -std=c11 -O2 -Wall -Wextra -Werror -I "$root/userspace/lib/compositor" \
  "$root/tests/compositor/vulkan_owned_faults.c" "$root/userspace/lib/compositor/vulkan_owned_image.c" -o "$out/faults"
"$out/faults"
(cd "$root/kernel" && alr exec -- gprbuild -q -p -P ../tests/compositor/vulkan_owned.gpr)
timeout 120 "$out/vulkan_owned_tests" > "$out/positive.log" 2>&1
cat "$out/positive.log"
