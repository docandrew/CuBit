#!/usr/bin/env bash
# Run in triangle-host-shell.nix. Linux lavapipe only, not native CuBit.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$(mktemp -d /tmp/cubit-vulkan-triangle.XXXXXX)
python3 "$root/tests/mesa-anv/build-triangle-shaders.py" "$out"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
# Do not mix the host distribution's implicit-layer manifests with the pinned
# Nix Mesa manifests (the loader otherwise reports duplicate layer warnings).
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
test -s "$VK_DRIVER_FILES"
cc -std=c11 -O2 -Wall -Wextra -Werror -I"$out" \
    "$root/tests/mesa-anv/triangle-host-test.c" \
    $(pkg-config --cflags --libs vulkan) -o "$out/test"
timeout 30 "$out/test"
set +e
timeout 30 "$out/test" --negative-control > "$out/negative.log" 2>&1
negative_status=$?
set -e
if [[ $negative_status != 7 ]] ||
   ! grep -Eq 'VULKAN VALIDATION errors=[1-9][0-9]* ' "$out/negative.log"; then
    echo "Validation negative control did not fail as expected" >&2
    cat "$out/negative.log" >&2
    exit 1
fi
echo "Validation negative control PASS: invalid buffer rejected by test verdict"
echo "Linux lavapipe oracle retained at $out (NOT CuBit GPU execution)"
