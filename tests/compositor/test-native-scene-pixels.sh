#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
extent=${NATIVE_SCENE_EXTENT:-64}
case "$extent" in 64|256) ;; *) echo "unsupported oracle extent: $extent" >&2; exit 1;; esac
export NATIVE_SCENE_EXTENT=$extent
out="$root/tests/compositor/build/native-scene-pixels-$extent"
python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$out/generated"
export C_INCLUDE_PATH="$out/generated${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
(cd "$root/kernel" && alr exec -- gprbuild -q -p -P ../tests/compositor/native_scene_pixels.gpr)
timeout 120 "$out/native_scene_pixels" > "$out/positive.log" 2>&1
cat "$out/positive.log"
if CUBIT_NATIVE_SCENE_OMIT_BARRIER=1 timeout 120 "$out/native_scene_pixels" > "$out/negative.log" 2>&1; then
    echo 'FAIL: omitted output barrier was not detected' >&2
    exit 1
fi
rg -q 'VULKAN VALIDATION:.*(layout|SYNC-|VUID-)' "$out/negative.log"
echo 'HOST ONLY negative control: missing output barrier detected PASS'
