#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root/kernel"
# The observer record buffer is a compiled IPC contract. Refresh both ends:
# a new logstore plus an old statically-linked viewer silently rejects reads.
rom_args=()
platform_args=()
output=cubit_laptop_usb.img
profile=../images/laptop-usb.ccl
extra_inputs=()
if [[ ${2:-} == --uefi ]]; then
    platform_args=(--grub-directory "${CUBIT_GRUB_EFI_DIR:?run in nix develop}")
    output=cubit_live_uefi.img
elif [[ -n ${2:-} ]]; then
    echo 'Expected optional --uefi' >&2
    exit 2
fi
if [[ ${3:-} == --render-session ]]; then
    profile=../images/render-session.ccl
    output=cubit_live_render_session.img
elif [[ ${3:-} == --mesa-device ]]; then
    profile=../images/mesa-device.ccl
    output=cubit_live_mesa_device.img
elif [[ ${3:-} == --mesa-triangle ]]; then
    profile=../images/mesa-triangle.ccl
    output=cubit_live_mesa_triangle.img
elif [[ ${3:-} == --mesa-triangle-window ]]; then
    profile=../images/mesa-triangle-window.ccl
    output=cubit_live_mesa_triangle_window.img
elif [[ ${3:-} == --desktop-mesa-startup ]]; then
    profile=../images/desktop-mesa-startup.ccl
    output=cubit_live_desktop_mesa_startup.img
    desktop_candidate=$(python3 ../tests/hardware/verify-desktop-mesa-startup.py \
        "${CUBIT_DESKTOP_MESA_DIR:?explicit verified Desktop candidate directory required}")
    extra_inputs=(--input "desktop-mesa-startup=$desktop_candidate")
elif [[ -n ${3:-} ]]; then
    echo 'Expected --render-session, --mesa-device, --mesa-triangle, --mesa-triangle-window or --desktop-mesa-startup after the platform option' >&2
    exit 2
fi
if [[ -n ${SAMEBOY_ROMS_DIR:-} ]]; then
    rom_args=(--private-rom-dir "$SAMEBOY_ROMS_DIR")
fi
# Keep named hardware checkpoints without replacing an image being tested.
# Limit overrides to a filename in kernel/, never an arbitrary path.
if [[ -n ${CUBIT_LIVE_OUTPUT:-} ]]; then
    case "$CUBIT_LIVE_OUTPUT" in
        *[!a-zA-Z0-9._-]*|.*|-*) echo 'Invalid CUBIT_LIVE_OUTPUT basename' >&2; exit 2 ;;
        *.img) output=$CUBIT_LIVE_OUTPUT ;;
        *) echo 'CUBIT_LIVE_OUTPUT must end in .img' >&2; exit 2 ;;
    esac
fi
# This wrapper supplies local build inputs, not image membership or shell code
# from CCL. The checked image profile owns every bootstrap/optical placement.
make logstore boot-logs logs ccl-console laptop_live_rw.img
intel_blob=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix blob)
intel_license=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix license)
mesa_notices=$(cat ../userspace/mesa/build/notice-path)
test -s "$mesa_notices/MESA-SOURCE.tar.gz"
if [[ $profile == ../images/mesa-triangle-window.ccl || $profile == ../images/desktop-mesa-startup.ccl ]]; then
    # This diagnostic slot can contain the teapot probe as well as triangles.
    # Retain the geometry's upstream license alongside Mesa's source notices.
    # Never mutate the original notice bundle (it may be in the Nix store).
    combined_notices=$(mktemp -d)
    cp -a "$mesa_notices/." "$combined_notices/"
    cp ../tests/mesa-teapot/teapot-control-points.h "$combined_notices/FreeGLUT-teapot.h"
    mesa_notices=$combined_notices
fi
python3 ../userspace/ccl/tools/ccl-image/realize.py "$profile" \
    --input "intel-guc=$intel_blob" \
    --input "intel-firmware-license=$intel_license" \
    --input "doom-wad=${1:?DOOM WAD path required}" \
    --input "sameboy-license=${SAMEBOY_SRC:?run in nix develop}/LICENSE" \
    --input "openlibm-notices=${SAMEBOY_LIBM_NOTICES:?run in nix develop}" \
    --input "mesa-notices=$mesa_notices" \
    "${extra_inputs[@]}" \
    "${rom_args[@]}" "${platform_args[@]}" --audit-usb --output "$output"
