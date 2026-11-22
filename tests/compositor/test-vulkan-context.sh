#!/usr/bin/env bash
# Run inside the pinned vulkan-affine-shell.nix; hosted FFI fault test only.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$(mktemp -d "$root/tests/compositor/build/vulkan-context-XXXXXXXX")
printf '%s\n' "$out"
cc -std=c11 -O2 -Wall -Wextra -Werror -I"$root/userspace/lib/compositor" \
 "$root/tests/compositor/vulkan_context_faults.c" \
 "$root/userspace/lib/compositor/vulkan_context.c" \
 "$root/userspace/lib/compositor/vulkan_submission_native.c" -o "$out/faults"
"$out/faults"
cc -std=c11 -O2 -Wall -Wextra -Werror -I"$root/userspace/lib/compositor" \
 "$root/tests/compositor/vulkan_context_host.c" \
 "$root/userspace/lib/compositor/vulkan_context.c" \
 "$root/userspace/lib/compositor/vulkan_submission_native.c" -lvulkan -o "$out/host"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
"$out/host"
sha256sum "$root/userspace/lib/compositor/vulkan_context.h" \
 "$root/userspace/lib/compositor/vulkan_context.c" \
 "$root/userspace/lib/compositor/vulkan_submission_native.c" \
 "$root/tests/compositor/vulkan_context_faults.c" \
 "$root/tests/compositor/vulkan_context_host.c" > "$out/source-hashes.txt"
