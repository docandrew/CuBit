#!/usr/bin/env bash
# Build SameBoy for CuBit: the upstream core's unmodified C on the CuBit
# libc, with the frontend in Ada (docs/c-removal.md, README.md). Run in the
# Nix shell after the libc (make -C kernel libc) and the CCL manifest
# compiler.
#   build.sh OUTPUT.app
# Also leaves the CuBit test cartridge at build/test.gb.
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
output=$1
build=$here/build
cc=$root/userspace/libc/cubit-cc
core=${SAMEBOY_SRC:?the Nix shell provides SAMEBOY_SRC}
runtime=$root/userspace/runtime/gnat
gnat_gcc=$(dirname "$(command -v gnat)")/gcc

rm -rf "$build"
mkdir -p "$build/core" "$build/ada" "$build/resources"

# The open-source boot ROMs (rgbds) and the test cartridge.
make -s -C "$core" bootroms CONF=release BIN="$build/resources" \
    OBJ="$build/resources/obj" PB12_COMPRESS="$build/resources/pb12"
rgbasm -o "$build/test-rom.o" "$here/test-cartridge.asm"
rgblink -o "$build/test.gb" "$build/test-rom.o"
rgbfix -v -p 0 -t "CUBIT TEST" "$build/test.gb"
gcc -c -Wa,-I,"$build" "$here/bootroms.S" -o "$build/bootroms.o"

# The core, without the debugger, cheats, rewind or the host clock.
pids=()
for source in "$core"/Core/*.c; do
    name=$(basename "$source" .c)
    case $name in debugger|cheat_search|cheats|rewind|sm83_disassembler|symbol_hash) continue ;; esac
    "$cc" -c -O2 -std=gnu11 -ffunction-sections -fdata-sections -w -DNDEBUG \
        -D_GNU_SOURCE -DGB_INTERNAL -DGB_DISABLE_DEBUGGER -DGB_DISABLE_CHEATS \
        -DGB_DISABLE_REWIND -DGB_DISABLE_TIMEKEEPING -DGB_VERSION='"CuBit-port"' \
        -I"$core" -I"$core/Core" "$source" -o "$build/core/$name.o" &
    pids+=($!)
done
for pid in "${pids[@]}"; do wait "$pid"; done

# The frontend and the runtime units it uses, without an Ada run-time
# library (the libc's restrictions, userspace/libc/libc-ada.adc).
compile_ada() {
    (cd "$build/ada" && "$gnat_gcc" -c -O2 -g -gnatp -gnatn -fno-pic \
        -ffunction-sections -gnat2022 -gnatec="$root/userspace/libc/libc-ada.adc" \
        -I"$here" -I"$runtime" "$@")
}
for unit in "$here"/cubit-sameboy_*.adb; do
    compile_ada -gnatwa -gnatys "$unit"
done
for unit in audio messages memory_grants desktop_protocol desktop_messages; do
    compile_ada "$runtime/cubit-$unit.adb"
done

"$root/userspace/ccl/build/manifest/ccl-manifest" \
    "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/manifest.ccl" \
    > "$build/manifest.S"
as --64 "$build/manifest.S" -o "$build/manifest.o"

CUBIT_STACK_SIZE=16777216 "$cc" -o "$output" \
    "$build"/core/*.o "$build/bootroms.o" "$build"/ada/*.o \
    --manifest "$build/manifest.o" -lm
