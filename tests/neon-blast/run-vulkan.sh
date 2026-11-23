#!/usr/bin/env bash
# Linux-hosted Neon Blast Vulkan path: the game's shaders and Vulkan glue on
# the GPU canvas widget's host device (Mesa lavapipe, CPU). Shows the look
# and checks the pipeline end to end; not an Intel hardware or CuBit result.
# Run inside nix develop with TMPDIR outside /tmp:
#   tests/neon-blast/run-vulkan.sh [SECONDS] [SHOT_EVERY_FRAMES]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots="${NEON_BLAST_SCREENSHOTS:-/home/doc/cubit-build-tmp/neon-blast/vulkan-host}"
host_shell="$root/tests/mesa-anv/triangle-host-shell.nix"
if [ -z "${MESA_DRIVER_ROOT:-}" ]; then
  exec nix-shell "$host_shell" --run "bash $0 $*"
fi
mkdir -p "$here/build/generated" "$root/tests/gpu-canvas/build/source" "$shots"
cp -u "$root/userspace/runtime/gnat/cubit.ads" "$root/tests/gpu-canvas/build/source/"
gen="$here/build/generated"
includes=""
for source in fullscreen.vert background.frag segment.vert segment.frag post.frag; do
  name="${source//./_}"
  glslangValidator -V --vn "${name}_spv" "$root/userspace/apps/neon-blast/shaders/$source" -o "$gen/$name.h" >/dev/null
  includes="$includes#include \"$name.h\"\n"
done
printf "$includes" > "$gen/neon_shaders.h"
cflags=(-std=c11 -O2 -g -Wall -Wextra -Werror -I"$root/userspace/lib/ui" -I"$gen" $(pkg-config --cflags vulkan))
cc "${cflags[@]}" -c "$root/userspace/lib/ui/gpu_canvas_vk.c" -o "$here/build/gpu_canvas_vk.o"
cc "${cflags[@]}" -c "$root/userspace/apps/neon-blast/neon_vk.c" -o "$here/build/neon_vk.o"
cc "${cflags[@]}" -c "$root/tests/gpu-canvas/host/gpu_canvas_host.c" -o "$here/build/gpu_canvas_host.o"
(cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -P "$here/neon_vulkan_host.gpr" \
   -largs -L"$(pkg-config --variable=libdir vulkan)")
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
out="$here/build/vk-frames"
mkdir -p "$out"
"$here/build/neon_vulkan_host" "$out" "${1:-20}" "${2:-150}"
python3 -I "$root/tests/text-editor/ppm_to_png.py" "$out"/*.ppm --into "$shots" >/dev/null
echo "screenshots: $shots"
