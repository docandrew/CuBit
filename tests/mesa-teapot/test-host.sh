#!/usr/bin/env bash
# Linux lavapipe ONLY. No CuBit GPU execution.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$(mktemp -d /tmp/cubit-teapot-render.XXXXXX)
python3 "$root/tests/mesa-teapot/build-assets.py" "$out"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
export TEAPOT_PPM="$out/teapot.ppm"
cc -std=c11 -O2 -Wall -Wextra -Werror -I"$out" \
   "$root/tests/mesa-teapot/host-test.c" $(pkg-config --cflags --libs vulkan) -o "$out/test"
timeout 60 "$out/test"
TEAPOT_PPM="$out/teapot-no-depth.ppm" timeout 60 "$out/test" --depth-negative > "$out/depth-negative.log" 2>&1
python3 "$root/tests/mesa-teapot/check-depth-control.py" "$out/teapot.ppm" "$out/teapot-no-depth.ppm"
set +e
timeout 60 "$out/test" --negative-control > "$out/negative.log" 2>&1
status=$?
set -e
if [[ $status != 7 ]] || ! grep -Eq 'VULKAN VALIDATION errors=[1-9][0-9]* ' "$out/negative.log"; then
   echo 'Vulkan validation negative control failed' >&2; exit 1
fi
echo "Validation negative control PASS; Linux-only artifacts: $out"
for frames in 8 256; do
   cc -std=c11 -O2 -Wall -Wextra -Werror -DCUBIT_TEAPOT_FRAME_COUNT="$frames" -I"$out" \
      "$root/tests/mesa-teapot/host-test.c" $(pkg-config --cflags --libs vulkan) -o "$out/reuse-$frames"
   timeout 60 "$out/reuse-$frames" > "$out/reuse-$frames.log" 2>&1
   tail -4 "$out/reuse-$frames.log"
done
echo "Repeated resource reuse PASS (Linux-only); artifacts: $out"
