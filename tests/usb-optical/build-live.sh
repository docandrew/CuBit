#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root/kernel"
rom_args=()
if [[ -n ${SAMEBOY_ROMS_DIR:-} ]]; then
    rom_args=(--private-rom-dir "$SAMEBOY_ROMS_DIR")
fi
# This wrapper supplies local build inputs, not image membership or shell code
# from CCL. The checked image profile owns every bootstrap/optical placement.
python3 ../userspace/ccl/tools/ccl-image/realize.py ../images/laptop-usb.ccl \
    --input "doom-wad=${1:?DOOM WAD path required}" \
    --input "sameboy-license=${SAMEBOY_SRC:?run in nix develop}/LICENSE" \
    --input "openlibm-notices=${SAMEBOY_LIBM_NOTICES:?run in nix develop}" \
    "${rom_args[@]}" --audit-usb --output cubit_laptop_usb.iso
