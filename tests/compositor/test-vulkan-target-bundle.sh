#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/tests/compositor/build/vulkan-target-bundle"
python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$out/generated"
export C_INCLUDE_PATH="$out/generated${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
cc -std=c11 -O2 -Wall -Wextra -Werror -I "$root/userspace/lib/compositor" \
  "$root/tests/compositor/vulkan_owned_target_faults.c" "$root/userspace/lib/compositor/vulkan_owned_target_binding.c" -o "$out/binding-faults"
"$out/binding-faults"
(cd "$root/kernel" && alr exec -- gprbuild -q -p -P ../tests/compositor/vulkan_target_bundle.gpr)
timeout 120 "$out/vulkan_target_bundle_tests" > "$out/positive.log" 2>&1
cat "$out/positive.log"
