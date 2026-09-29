#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root/kernel"
rom_args=()
platform_args=()
output=cubit_laptop_usb.img
if [[ ${2:-} == --uefi ]]; then
    platform_args=(--grub-directory "${CUBIT_GRUB_EFI_DIR:?run in nix develop}")
    output=cubit_live_uefi.img
elif [[ -n ${2:-} ]]; then
    echo 'Expected optional --uefi' >&2
    exit 2
fi
if [[ -n ${SAMEBOY_ROMS_DIR:-} ]]; then
    rom_args=(--private-rom-dir "$SAMEBOY_ROMS_DIR")
fi
# This wrapper supplies local build inputs, not image membership or shell code
# from CCL. The checked image profile owns every bootstrap/optical placement.
intel_blob=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix blob)
intel_license=$(nix eval --raw --file ../tests/intel-gpu/firmware-source.nix license)
mesa_notices=$(cat ../userspace/mesa/build/notice-path)
test -s "$mesa_notices/MESA-SOURCE.tar.gz"
python3 ../userspace/ccl/tools/ccl-image/realize.py ../images/laptop-usb.ccl \
    --input "intel-guc=$intel_blob" \
    --input "intel-firmware-license=$intel_license" \
    --input "doom-wad=${1:?DOOM WAD path required}" \
    --input "sameboy-license=${SAMEBOY_SRC:?run in nix develop}/LICENSE" \
    --input "openlibm-notices=${SAMEBOY_LIBM_NOTICES:?run in nix develop}" \
    --input "mesa-notices=$mesa_notices" \
    "${rom_args[@]}" "${platform_args[@]}" --audit-usb --output "$output"
