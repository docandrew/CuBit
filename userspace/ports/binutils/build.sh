#!/usr/bin/env bash
# Build GNU binutils for CuBit (docs/self-hosting.md, "Then the tools"): an
# ordinary x86-64 Linux toolchain, cross-built static on the host with the
# CuBit libc (cubit-cc), not a CuBit target triple (user decision,
# 2026-10-04). Each program's CCL manifest is added afterwards, as sections,
# in a separate step. Run in the Nix shell after `make -C kernel libc
# ccl-manifest`.
#   build.sh OUTPUT_DIRECTORY
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
out=$1
build=$here/build
tools=(as ld ar nm objcopy objdump readelf strip size strings ranlib addr2line)

# The source nixpkgs pins (flake.lock), not a separate download.
tarball=$(nix build --inputs-from "$root" nixpkgs#binutils-unwrapped.src \
          --no-link --print-out-paths)
mkdir -p "$build"
if [ ! -f "$build/source.stamp" ] || [ "$(cat "$build/source.stamp")" != "$tarball" ]; then
    rm -rf "$build/src" "$build/obj"
    mkdir -p "$build/src"
    tar -xf "$tarball" -C "$build/src" --strip-components=1
    echo "$tarball" > "$build/source.stamp"
fi

mkdir -p "$build/obj"
cd "$build/obj"
if [ ! -f Makefile ]; then
    CC="$root/userspace/libc/cubit-cc" CFLAGS="-O2" \
    CXX="$root/userspace/libc/cubit-c++" CXXFLAGS="-O2" LDFLAGS="-static" \
    "$build/src/configure" \
        --build=x86_64-pc-linux-gnu --host=x86_64-linux-musl \
        --target=x86_64-pc-linux-gnu \
        --prefix=/ --disable-nls --disable-shared --enable-static \
        --disable-plugins --disable-gold --disable-gprofng --disable-gprof \
        --disable-gdb --disable-gdbserver --disable-sim --disable-readline \
        --disable-libdecnumber --disable-werror --disable-multilib \
        --without-zstd --without-debuginfod --without-msgpack \
        --with-system-zlib=no > configure.log 2>&1
fi
# binutils' make does not know the libc: relink the tools against the current one.
rm -f gas/as-new ld/ld-new binutils/nm-new binutils/strip-new binutils/ar binutils/objcopy \
      binutils/objdump binutils/readelf binutils/size binutils/strings binutils/ranlib \
      binutils/addr2line
make -j"$(nproc)" MAKEINFO=true all-binutils all-gas all-ld > make.log 2>&1

# The separate step: each program gets its manifest's sections: TOOL.ccl
# (typed parameters, docs/ccl-launch-parameters.md) where there is one,
# otherwise manifest.ccl.
manifest_sections() {  # manifest_sections SOURCE NAME: sets $sections, $dir
    dir=$build/sections/$2
    mkdir -p "$dir"
    rm -f "$dir"/*.bin
    "$root/userspace/ccl/build/manifest/ccl-manifest" \
        "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$1" \
        --schema "$root/userspace/ccl/interfaces/executable-manifest.ccl" > "$dir/manifest.S"
    as --64 "$dir/manifest.S" -o "$dir/manifest.o"
    sections=$(objdump -h "$dir/manifest.o" | awk '$2 ~ /^\.cubit\./ {print $2}')
    for section in $sections; do
        objcopy --dump-section "$section=$dir/$section.bin" "$dir/manifest.o"
    done
}
mkdir -p "$out"
declare -A path=([as]=gas/as-new [ld]=ld/ld-new [nm]=binutils/nm-new
                 [strip]=binutils/strip-new [ar]=binutils/ar [objcopy]=binutils/objcopy
                 [objdump]=binutils/objdump [readelf]=binutils/readelf
                 [size]=binutils/size [strings]=binutils/strings
                 [ranlib]=binutils/ranlib [addr2line]=binutils/addr2line)
for tool in "${tools[@]}"; do
    if [ -f "$here/$tool.ccl" ]; then
        manifest_sections "$here/$tool.ccl" "$tool"
    else
        manifest_sections "$here/manifest.ccl" shared
    fi
    args=()
    for section in $sections; do
        args+=(--add-section "$section=$dir/$section.bin"
               --set-section-flags "$section=readonly,data")
    done
    objcopy "${args[@]}" "$build/obj/${path[$tool]}" "$out/$tool.app"
done
echo "binutils: ${tools[*]} -> $out"
