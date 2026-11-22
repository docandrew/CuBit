#!/usr/bin/env bash
# Linux-hosted GPU canvas widget + teapot gallery. Real Vulkan through the
# host loader with Mesa lavapipe (a CPU rasterizer): checks the widget's
# contract and the gallery's output, never Intel hardware behaviour.
# Run inside nix develop with TMPDIR outside /tmp:
#   tests/gpu-canvas/run.sh            build, contract + mesh checks, screenshots
#   tests/gpu-canvas/run.sh --prove    SPARK proof of GPU_Canvas_Slots and the gallery units
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots="${GPU_CANVAS_SCREENSHOTS:-/home/doc/cubit-build-tmp/gpu-canvas}"
mkdir -p "$here/build/source" "$here/build/obj" "$shots"
case "${1:-}" in
  --prove)
    cd "$root/kernel"
    exec systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/prove.gpr" --level=2 --report=fail \
        --checks-as-errors=on -j2 ${PROVE_UNITS:--u gpu_canvas_slots.adb}
    ;;
esac
# Lavapipe and the Vulkan headers/loader come from this shell.
host_shell="$root/tests/mesa-anv/triangle-host-shell.nix"
if [ -z "${MESA_DRIVER_ROOT:-}" ]; then
  exec nix-shell "$host_shell" --run "bash $0 $*"
fi
# Runtime units the toolkit names (read-only copies).
for unit in cubit.ads; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
# Shaders: GLSL -> SPIR-V words in a C header.
gen="$here/build/generated"
mkdir -p "$gen"
glslangValidator -V --vn teapot_vert_spv "$root/userspace/apps/teapots/shaders/teapot.vert" -o "$gen/teapot_vert.h" >/dev/null
glslangValidator -V --vn teapot_frag_spv "$root/userspace/apps/teapots/shaders/teapot.frag" -o "$gen/teapot_frag.h" >/dev/null
printf '#include "teapot_vert.h"\n#include "teapot_frag.h"\n' > "$gen/teapot_shaders.h"
cflags=(-std=c11 -O2 -g -Wall -Wextra -Werror -I"$root/userspace/lib/ui" -I"$gen" $(pkg-config --cflags vulkan))
cc "${cflags[@]}" -c "$root/userspace/lib/ui/gpu_canvas_vk.c" -o "$here/build/gpu_canvas_vk.o"
cc "${cflags[@]}" -c "$root/userspace/apps/teapots/teapot_vk.c" -o "$here/build/teapot_vk.o"
cc "${cflags[@]}" -c "$here/host/gpu_canvas_host.c" -o "$here/build/gpu_canvas_host.o"
(cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -P "$here/teapots_host.gpr" \
   -largs -L"$(pkg-config --variable=libdir vulkan)")
export VK_DRIVER_FILES="$MESA_DRIVER_ROOT/share/vulkan/icd.d/lvp_icd.x86_64.json"
"$here/build/slots_tests"
"$here/build/mesh_tests" "$root/tests/mesa-teapot/teapot-control-points.h"
out="$here/build/frames"
mkdir -p "$out"
"$here/build/teapots_host" "$out"
"$here/build/teapots_host" "$out" unavailable
python3 -I "$root/tests/text-editor/ppm_to_png.py" "$out"/*.ppm --into "$shots"
echo "screenshots: $shots"
