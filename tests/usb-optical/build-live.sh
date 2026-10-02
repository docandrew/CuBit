#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root/kernel"
# The observer record buffer is a compiled IPC contract. Refresh both ends:
# a new logstore plus an old statically-linked viewer silently rejects reads.
make logstore boot-logs laptop_live_rw.img
rom_args=()
platform_args=()
output=cubit_laptop_usb.img
profile=../images/laptop-usb.ccl
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
elif [[ -n ${3:-} ]]; then
    echo 'Expected optional --render-session, --mesa-device, --mesa-triangle or --mesa-triangle-window after the platform option' >&2
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
intel_blob=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix blob)
intel_license=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix license)
mesa_notices=$(cat ../userspace/mesa/build/notice-path)
test -s "$mesa_notices/MESA-SOURCE.tar.gz"
python3 ../userspace/ccl/tools/ccl-image/realize.py "$profile" \
    --input "intel-guc=$intel_blob" \
    --input "intel-firmware-license=$intel_license" \
    --input "doom-wad=${1:?DOOM WAD path required}" \
    --input "sameboy-license=${SAMEBOY_SRC:?run in nix develop}/LICENSE" \
    --input "openlibm-notices=${SAMEBOY_LIBM_NOTICES:?run in nix develop}" \
    --input "mesa-notices=$mesa_notices" \
    "${rom_args[@]}" "${platform_args[@]}" --audit-usb --output "$output"
