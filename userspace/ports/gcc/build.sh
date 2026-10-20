#!/usr/bin/env bash
# Build GCC 15.3.0's host programs for CuBit (docs/self-hosting.md, "GCC and
# GNAT"): cc1 (the C compiler proper), gnat1 (the Ada compiler proper),
# gnatbind and the gcc driver, cross-built static with the CuBit libc
# (cubit-cc, cubit-c++) as an x86-64 Linux musl toolchain, not a CuBit
# target triple (user decision, 2026-10-04); and the Ada run-time library
# (libgnat, libgnarl) the programs it builds link. GMP, MPFR and MPC are
# built in-tree. Each program's CCL manifest is added afterwards, as
# sections, in a separate step (as for binutils). Run in the Nix shell after
# `make -C kernel libc ccl-manifest`.
#   build.sh OUTPUT_DIRECTORY
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
out=$1
build=$here/build
# The compilers need far more than the 1 MiB default stack (audit, 2026-10-05).
export CUBIT_STACK_SIZE=$((64 * 1024 * 1024))

# Sources nixpkgs pins (flake.lock): the same GCC as the musl cross compiler
# cubit-c++ links with, and its prerequisites.
source_of() { nix build --inputs-from "$root" "nixpkgs#$1" --no-link --print-out-paths; }
gcc_src=$(source_of gcc15.cc.src)
gmp_src=$(source_of gmp.src)
mpfr_src=$(source_of mpfr.src)
mpc_src=$(source_of libmpc.src)
# Fixes for GNAT's run-time library on musl.
patches=("$here/gnat-musl.patch")
stamp="$gcc_src $gmp_src $mpfr_src $mpc_src $(cat "${patches[@]}" | sha256sum)"
mkdir -p "$build"
if [ ! -f "$build/source.stamp" ] || [ "$(cat "$build/source.stamp")" != "$stamp" ]; then
    rm -rf "$build/src" "$build/obj"
    mkdir -p "$build/src"
    tar -xf "$gcc_src" -C "$build/src" --strip-components=1
    for pair in "gmp:$gmp_src" "mpfr:$mpfr_src" "mpc:$mpc_src"; do
        mkdir -p "$build/src/${pair%%:*}"
        tar -xf "${pair#*:}" -C "$build/src/${pair%%:*}" --strip-components=1
    done
    for patch in "${patches[@]}"; do
        patch -d "$build/src" -p1 < "$patch" > /dev/null
    done
    echo "$stamp" > "$build/source.stamp"
fi

# A compiler for the target is needed for the driver's -dumpspecs step.
cross=$(cat "$root/userspace/libc/build/cross-gcc")
# The Ada compiler that builds GNAT (decision D1): nixpkgs' musl cross GNAT
# of the same version. It has no Ada run-time library of its own, which
# GCC's build does not need: gnat1 and gnatbind are built from the run-time
# sources in the tree (STAGE1=False below), and the target libgnat is
# built from them with it.
gnat=$(nix build --inputs-from "$root" "nixpkgs#pkgsCross.musl64.buildPackages.gnat15" \
       --no-link --print-out-paths | grep -v -- "-man$" | head -1)
gnat_prefix=$gnat/bin/x86_64-unknown-linux-musl-
export CUBIT_CC_DRIVER=${gnat_prefix}gcc
# Everything the compilers read lives under the toolchain place
# (@nvme:0/toolchain, decision D6): the default include directories too, so
# cc1 never looks outside it (a directory outside a process's places is
# refused, and cc1 treats that as an error, unlike a missing one).
configure_args=(
    --build=x86_64-pc-linux-gnu --host=x86_64-linux-musl --target=x86_64-linux-musl
    --prefix=/toolchain --with-local-prefix=/toolchain/local
    --with-native-system-header-dir=/toolchain/include
    --enable-languages=c,ada --disable-nls --disable-shared --enable-static
    --disable-multilib --disable-lto --disable-plugin --disable-bootstrap
    --without-isl --without-zstd --disable-libsanitizer --disable-werror
    --enable-default-pie=no --with-system-zlib=no)
mkdir -p "$build/obj"
cd "$build/obj"
# Reconfigured from scratch when the options change.
if [ ! -f Makefile ] || [ "$(cat configure.args 2>/dev/null)" != "${configure_args[*]}" ]; then
    find . -mindepth 1 -delete
    CC="$root/userspace/libc/cubit-cc" CFLAGS="-O2" \
    CXX="$root/userspace/libc/cubit-c++" CXXFLAGS="-O2" LDFLAGS="-static" \
    CC_FOR_BUILD=gcc CXX_FOR_BUILD=g++ \
    GNATBIND="${gnat_prefix}gnatbind" GNATMAKE="${gnat_prefix}gnatmake" \
    acx_cv_cc_gcc_supports_ada=yes \
    AS_FOR_TARGET=as LD_FOR_TARGET=ld AR_FOR_TARGET=ar NM_FOR_TARGET=nm \
    RANLIB_FOR_TARGET=ranlib OBJDUMP_FOR_TARGET=objdump READELF_FOR_TARGET=readelf \
    "$build/src/configure" "${configure_args[@]}" > configure.log 2>&1
    echo "${configure_args[*]}" > configure.args
fi
# make does not know the libc: relink the programs against the current one.
rm -f gcc/cc1 gcc/gnat1 gcc/gnatbind gcc/xgcc
# STAGE1=False: GCC's build treats a cross-native build like a first stage,
# compiling gnat1 and gnatbind against the building compiler's run-time
# library, which the musl cross GNAT lacks. As for a later stage, they are
# built with the run-time units they need from the tree instead. The
# top-level Makefile passes no command-line variables down, so it goes in
# the gcc directory's make goal.
make -j"$(nproc)" MAKEINFO=true TARGET-gcc="all STAGE1=False" \
    GCC_FOR_TARGET="${gnat_prefix}gcc" all-gcc > make.log 2>&1
# The Ada run-time library for programs: libgnat and libgnarl, built by the
# musl cross GNAT from the same sources, in gcc/ada/rts. Its C parts use
# GNU extensions musl declares only for _GNU_SOURCE (cpu_set_t), which
# glibc's headers expose to GNAT's sources by default. Rebuilt each time,
# like the programs above: make does not know the patches.
rm -rf gcc/ada/rts gcc/stamp-gnatlib* "x86_64-linux-musl/libada/stamp-libada"
make -j"$(nproc)" MAKEINFO=true \
    GCC_FOR_TARGET="${gnat_prefix}gcc" CC_FOR_TARGET="${gnat_prefix}gcc" \
    CFLAGS_FOR_TARGET="-g -O2 -D_GNU_SOURCE" \
    all-target-libada > make-libada.log 2>&1

# The separate step: each program's manifest, as sections.
sections_dir=$build/sections
add_manifest() { # MANIFEST INPUT OUTPUT
    local name dir args=() section
    name=$(basename "$1" .ccl)
    dir=$sections_dir/$name
    rm -rf "$dir"
    mkdir -p "$dir"
    "$root/userspace/ccl/build/manifest/ccl-manifest" \
        "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$1" \
        --schema "$root/userspace/ccl/interfaces/executable-manifest.ccl" > "$dir/manifest.S"
    as --64 "$dir/manifest.S" -o "$dir/manifest.o"
    for section in $(objdump -h "$dir/manifest.o" | awk '$2 ~ /^\.cubit\./ {print $2}'); do
        objcopy --dump-section "$section=$dir/$section.bin" "$dir/manifest.o"
        args+=(--add-section "$section=$dir/$section.bin"
               --set-section-flags "$section=readonly,data")
    done
    objcopy "${args[@]}" "$2" "$3"
}
mkdir -p "$out"
add_manifest "$here/manifest.ccl" gcc/cc1 "$out/cc1.app"
add_manifest "$here/driver.ccl" gcc/xgcc "$out/gcc.app"
add_manifest "$here/gnat1.ccl" gcc/gnat1 "$out/gnat1.app"
add_manifest "$here/gnatbind.ccl" gcc/gnatbind "$out/gnatbind.app"

# The toolchain as installed at @nvme:0/toolchain (--prefix=/toolchain),
# less binutils' as and ld (x86_64-linux-musl/bin), which come from
# userspace/ports/binutils: the driver, cc1, gnat1 and gnatbind; the Ada
# run-time library (adainclude: specifications and bodies, adalib: .ali
# files and the libgnat and libgnarl archives), where gnat1 and gnatbind
# look by default; gcc's support files from the musl cross compiler of the
# same version (libgcc and its unwinder, which Ada exceptions use, the
# static init/fini frame) and the specs; the CuBit libc (start files,
# archives, link script, headers).
machine=x86_64-linux-musl
version=15.3.0
sysroot=$root/userspace/libc/build/sysroot
tree=$build/toolchain
rm -rf "$tree"
mkdir -p "$tree/bin" "$tree/libexec/gcc/$machine/$version" \
    "$tree/lib/gcc/$machine/$version" "$tree/$machine/bin"
cp "$out/gcc.app" "$tree/bin/gcc"
cp "$out/cc1.app" "$tree/libexec/gcc/$machine/$version/cc1"
cp "$out/gnat1.app" "$tree/libexec/gcc/$machine/$version/gnat1"
cp "$out/gnatbind.app" "$tree/bin/gnatbind"
mkdir -p "$tree/lib/gcc/$machine/$version/adainclude" "$tree/lib/gcc/$machine/$version/adalib"
cp -L gcc/ada/rts/*.ads gcc/ada/rts/*.adb "$tree/lib/gcc/$machine/$version/adainclude/"
cp -L gcc/ada/rts/*.ali gcc/ada/rts/libgnat.a gcc/ada/rts/libgnarl.a \
   "$tree/lib/gcc/$machine/$version/adalib/"
cp "$here/cubit.specs" "$tree/lib/gcc/$machine/$version/specs"
for file in libgcc.a libgcc_eh.a crtbeginT.o crtend.o; do
    cp "$("$cross/bin/x86_64-unknown-linux-musl-gcc" -print-file-name="$file")" \
       "$tree/lib/gcc/$machine/$version/"
done
cp "$sysroot"/lib/cubit-crt1.o "$sysroot"/lib/crti.o "$sysroot"/lib/crtn.o \
   "$sysroot"/lib/cubit.ld "$sysroot"/lib/libc.a "$sysroot"/lib/libm.a "$tree/lib/"
cp -r "$sysroot/include" "$tree/include"
chmod -R u+w "$tree"
echo "gcc: cc1 gnat1 gnatbind gcc -> $out; toolchain tree $tree"
