#!/usr/bin/env bash
# Run in vulkan-copy-shell.nix. Hosted lavapipe only.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
test -s "$VK_DRIVER_FILES"
cd "$root/kernel"
alr exec -- gprbuild -q -f -p -P ../tests/compositor/vulkan_copy.gpr
out="$root/tests/compositor/build/vulkan-copy"
timeout 60 "$out/vulkan_copy_tests" > "$out/positive.log" 2>&1
cat "$out/positive.log"
set +e
CUBIT_VULKAN_COPY_NEGATIVE=1 timeout 60 "$out/vulkan_copy_tests" > "$out/negative.log" 2>&1
status=$?
set -e
if [[ "$status" == 0 || "$status" == 124 ]] || ! rg -q 'WRITE_AFTER_WRITE hazard detected' "$out/negative.log"; then
    cat "$out/negative.log"
    echo "Synchronization negative control did not fail as expected" >&2
    exit 1
fi
echo "VULKAN-COPY: synchronization negative control PASS (expected rejection)"
