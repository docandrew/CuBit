#!/usr/bin/env bash
# Run in vulkan-affine-shell.nix. Hosted real Mesa queue/fence; no native GPU claim.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/tests/compositor/build/backdrop-scene-pixels"
python3 "$root/tests/compositor/build-vulkan-affine-shaders.py" "$out/generated"
export C_INCLUDE_PATH="$out/generated${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
export XDG_DATA_DIRS="$MESA_DRIVER_ROOT/share"
(cd "$root/kernel" && alr exec -- gprbuild -q -f -p -P ../tests/compositor/backdrop_scene_pixels.gpr)
timeout 120 "$out/vulkan_submission_tests" > "$out/positive.log" 2>&1
cat "$out/positive.log"
python3 - "$out/positive.log" <<'CHECK'
from pathlib import Path
import sys
assert "HOST ONLY retained wallpaper: 96 ordered scenes" in Path(sys.argv[1]).read_text(), "wallpaper scene hooks were not exercised"
CHECK
