#!/usr/bin/env bash
# Real Vulkan, Linux CPU ICD only: no native Intel performance claims.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$(mktemp -d /tmp/cubit-teapot-gallery.XXXXXX)
echo "$out"
python3 "$root/tests/mesa-teapot/build-assets.py" "$out" --gallery
# Use the real renderer translation unit, not a duplicate of its guard.
# Gallery assets must never silently compile into the single-object renderer.
if cc -std=c11 -O2 -Wall -Wextra -Werror -I"$out" \
   -fsyntax-only "$root/tests/mesa-teapot/host-test.c" \
   $(pkg-config --cflags vulkan) >"$out/mode-negative.log" 2>&1; then
   echo 'Mismatched gallery assets accepted' >&2; exit 1
fi
grep -q 'teapot asset mode mismatch' "$out/mode-negative.log"
python3 "$root/tests/mesa-teapot/build-assets.py" "$out/single"
if cc -std=c11 -O2 -Wall -Wextra -Werror -DCUBIT_TEAPOT_GALLERY=1 \
   -I"$out/single" -fsyntax-only "$root/tests/mesa-teapot/host-test.c" \
   $(pkg-config --cflags vulkan) >"$out/single-mode-negative.log" 2>&1; then
   echo 'Mismatched single-object assets accepted' >&2; exit 1
fi
grep -q 'teapot asset mode mismatch' "$out/single-mode-negative.log"
echo 'Teapot asset mode mismatch rejected in both directions'
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
export TEAPOT_PPM="$out/gallery.ppm"
cc -std=c11 -O2 -Wall -Wextra -Werror -DCUBIT_TEAPOT_GALLERY=1 \
   -DCUBIT_TEAPOT_DETERMINISTIC=1 -DCUBIT_TEAPOT_FRAME_COUNT=16 -I"$out" \
   "$root/tests/mesa-teapot/host-test.c" $(pkg-config --cflags --libs vulkan) -o "$out/test"
timeout 60 "$out/test" >"$out/run.log" 2>&1
tail -5 "$out/run.log"
for stage in resources pipeline first-submit-wait first-validation-present; do
   grep -q "MESA-GALLERY startup stage=$stage ns=.* CPU-clock=1" "$out/run.log"
done
timeout 60 "$out/test" --clock-negative >"$out/clock-negative.log" 2>&1
grep -q 'fps-milli=0 CPU-clock=0' "$out/clock-negative.log"
if grep -q 'fps-milli=.* CPU-clock=1' "$out/clock-negative.log"; then
   echo 'Invalid clock series reported as valid' >&2; exit 1
fi
echo 'Gallery failed-clock negative control PASS (rendering continues, timing unavailable)'
python3 "$root/tests/mesa-teapot/test-gallery-negative.py" "$out"
echo "Gallery HOST ONLY PASS: $out"
