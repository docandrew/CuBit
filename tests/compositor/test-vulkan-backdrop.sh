#!/usr/bin/env bash
# Run in vulkan-affine-shell.nix. Hosted Mesa; no native GPU claim.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
base="$root/tests/compositor/build/vulkan-backdrop"
mkdir -p "$base"
cc -std=c11 -O2 -Wall -Wextra -Werror -I"$root/userspace/lib/compositor" \
  "$root/tests/compositor/backdrop_boundary_test.c" "$root/userspace/lib/compositor/vulkan_backdrop.c" \
  -o "$base/backdrop-boundary"
"$base/backdrop-boundary"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
headers="${C_INCLUDE_PATH:-}"
for variant in normal force-divide; do
    flags=()
    if [[ "$variant" == force-divide ]]; then flags=(--force-divide); fi
    python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$base/generated/$variant" "${flags[@]}"
    export C_INCLUDE_PATH="$base/generated/$variant${headers:+:$headers}"
    (cd "$root/kernel" && alr exec -- gprbuild -q -f -p -P ../tests/compositor/vulkan_backdrop.gpr --subdirs="$variant")
    timeout 120 "$base/$variant/vulkan_backdrop_tests" > "$base/$variant/positive.log" 2>&1
    cat "$base/$variant/positive.log"
done
