#!/usr/bin/env bash
# Build GCC 15.3.0's host programs for CuBit (docs/self-hosting.md, "GCC and
# GNAT"): cc1 (the C compiler proper) and the gcc driver, cross-built static
# with the CuBit libc (cubit-cc, cubit-c++) as an x86-64 Linux musl
# toolchain, not a CuBit target triple (user decision, 2026-10-04). GMP,
# MPFR and MPC are built in-tree. Each program's CCL manifest is added
# afterwards, as sections, in a separate step (as for binutils). Run in the
# Nix shell after `make -C kernel libc ccl-manifest`.
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
stamp="$gcc_src $gmp_src $mpfr_src $mpc_src"
mkdir -p "$build"
if [ ! -f "$build/source.stamp" ] || [ "$(cat "$build/source.stamp")" != "$stamp" ]; then
    rm -rf "$build/src" "$build/obj"
    mkdir -p "$build/src"
    tar -xf "$gcc_src" -C "$build/src" --strip-components=1
    for pair in "gmp:$gmp_src" "mpfr:$mpfr_src" "mpc:$mpc_src"; do
        mkdir -p "$build/src/${pair%%:*}"
        tar -xf "${pair#*:}" -C "$build/src/${pair%%:*}" --strip-components=1
    done
    echo "$stamp" > "$build/source.stamp"
fi

# A compiler for the target is needed for the driver's -dumpspecs step.
cross=$(cat "$root/userspace/libc/build/cross-gcc")
# Everything the compilers read lives under the toolchain place
# (@nvme:0/toolchain, decision D6): the default include directories too, so
# cc1 never looks outside it (a directory outside a process's places is
# refused, and cc1 treats that as an error, unlike a missing one).
configure_args=(
    --build=x86_64-pc-linux-gnu --host=x86_64-linux-musl --target=x86_64-linux-musl
    --prefix=/toolchain --with-local-prefix=/toolchain/local
    --with-native-system-header-dir=/toolchain/include
    --enable-languages=c --disable-nls --disable-shared --enable-static
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
    AS_FOR_TARGET=as LD_FOR_TARGET=ld AR_FOR_TARGET=ar NM_FOR_TARGET=nm \
    RANLIB_FOR_TARGET=ranlib OBJDUMP_FOR_TARGET=objdump READELF_FOR_TARGET=readelf \
    "$build/src/configure" "${configure_args[@]}" > configure.log 2>&1
    echo "${configure_args[*]}" > configure.args
fi
# make does not know the libc: relink the programs against the current one.
rm -f gcc/cc1 gcc/xgcc
make -j"$(nproc)" MAKEINFO=true \
    GCC_FOR_TARGET="$cross/bin/x86_64-unknown-linux-musl-gcc" all-gcc > make.log 2>&1

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

# The toolchain as installed at @nvme:0/toolchain (--prefix=/toolchain),
# less binutils' as and ld (x86_64-linux-musl/bin), which come from
# userspace/ports/binutils: the driver and cc1; gcc's support files from
# the musl cross compiler of the same version (libgcc, the static
# init/fini frame) and the specs; the CuBit libc (start files, archives,
# link script, headers).
machine=x86_64-linux-musl
version=15.3.0
sysroot=$root/userspace/libc/build/sysroot
tree=$build/toolchain
rm -rf "$tree"
mkdir -p "$tree/bin" "$tree/libexec/gcc/$machine/$version" \
    "$tree/lib/gcc/$machine/$version" "$tree/$machine/bin"
cp "$out/gcc.app" "$tree/bin/gcc"
cp "$out/cc1.app" "$tree/libexec/gcc/$machine/$version/cc1"
cp "$here/cubit.specs" "$tree/lib/gcc/$machine/$version/specs"
for file in libgcc.a crtbeginT.o crtend.o; do
    cp "$("$cross/bin/x86_64-unknown-linux-musl-gcc" -print-file-name="$file")" \
       "$tree/lib/gcc/$machine/$version/"
done
cp "$sysroot"/lib/cubit-crt1.o "$sysroot"/lib/crti.o "$sysroot"/lib/crtn.o \
   "$sysroot"/lib/cubit.ld "$sysroot"/lib/libc.a "$sysroot"/lib/libm.a "$tree/lib/"
cp -r "$sysroot/include" "$tree/include"
chmod -R u+w "$tree"
echo "gcc: cc1 gcc -> $out; toolchain tree $tree"
