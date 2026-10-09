#!/usr/bin/env bash
# Build DOOM for CuBit: doomgeneric's unmodified C on the CuBit libc, with
# the platform layer in Ada (docs/c-removal.md). Run in the Nix shell after
# the libc (make -C kernel libc) and the CCL manifest compiler.
#   build.sh OUTPUT.elf
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
output=$1
build=$here/build
cc=$root/userspace/libc/cubit-cc
doom=${DOOMGENERIC_SRC:?the Nix shell provides DOOMGENERIC_SRC}/doomgeneric
runtime=$root/userspace/runtime/gnat
gnat_gcc=$(dirname "$(command -v gnat)")/gcc

rm -rf "$build"
mkdir -p "$build/doom" "$build/ada" "$build/include"
# i_sound.c includes SDL_mixer.h with sound enabled but uses nothing from
# it: the sound module is CuBit.Doom_Sound_Module.
: > "$build/include/SDL_mixer.h"

# doomgeneric's sources, minus its other platforms' files.
sources=(dummy am_map doomdef doomstat dstrings d_event d_items d_iwad d_loop
    d_main d_mode d_net f_finale f_wipe g_game hu_lib hu_stuff info i_cdmus
    i_endoom i_joystick i_scale i_sound i_system i_timer i_input i_video memio
    m_argv m_bbox m_cheat m_config m_controls m_fixed m_menu m_misc m_random
    p_ceilng p_doors p_enemy p_floor p_inter p_lights p_map p_maputl p_mobj
    p_plats p_pspr p_saveg p_setup p_sight p_spec p_switch p_telept p_tick
    p_user r_bsp r_data r_draw r_main r_plane r_segs r_sky r_things sha1 sounds
    statdump st_lib st_stuff s_sound tables v_video wi_stuff w_checksum w_file
    w_main w_wad z_zone w_file_stdc doomgeneric)
pids=()
for name in "${sources[@]}"; do
    "$cc" -c -O2 -std=gnu11 -DNORMALUNIX -D_DEFAULT_SOURCE -DFEATURE_SOUND \
        -w -I"$doom" -I"$build/include" "$doom/$name.c" -o "$build/doom/$name.o" &
    pids+=($!)
done
for pid in "${pids[@]}"; do wait "$pid"; done

# The platform layer and the runtime units it uses, without an Ada run-time
# library (the libc's restrictions, userspace/libc/libc-ada.adc).
compile_ada() {
    (cd "$build/ada" && "$gnat_gcc" -c -O2 -g -gnatp -gnatn -fno-pic \
        -ffunction-sections -gnat2022 -gnatec="$root/userspace/libc/libc-ada.adc" \
        -I"$here" -I"$runtime" "$@")
}
for unit in "$here"/cubit-doom_*.adb; do
    compile_ada -gnatwa -gnatys "$unit"
done
for unit in audio messages process_ids memory_grants desktop_protocol desktop_messages; do
    compile_ada "$runtime/cubit-$unit.adb"
done

"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" \
    > "$build/manifest.S"
as --64 "$build/manifest.S" -o "$build/manifest.o"

CUBIT_STACK_SIZE=16777216 "$cc" -o "$output" \
    "$build"/doom/*.o "$build"/ada/*.o --manifest "$build/manifest.o"
