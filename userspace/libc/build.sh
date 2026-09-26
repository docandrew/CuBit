#!/usr/bin/env bash
# Build the CuBit libc (musl + CuBit system-call layer) into build/sysroot.
# Run in the Nix shell. See README.md.
set -euo pipefail
# Nix's gcc wrapper adds hardening flags (stack protector, fortify, PIE).
# The libc's start code runs before the thread pointer (and so the stack
# canary at %fs:0x28) exists; CuBit programs are static and non-PIE.
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
build=$here/build
musl_version=1.2.6
musl_sha256=d585fd3b613c66151fc3249e8ed44f77020cb5e6c1e635a616d3f9f82460512a
tarball=$build/musl-$musl_version.tar.gz
mkdir -p "$build"
if [ ! -f "$tarball" ]; then
    curl -sSL -o "$tarball.part" "https://musl.libc.org/releases/musl-$musl_version.tar.gz"
    mv "$tarball.part" "$tarball"
fi
echo "$musl_sha256  $tarball" | sha256sum -c --quiet

src=$build/musl-src
sysroot=$build/sysroot
rm -rf "$src"
mkdir -p "$src"
tar -xzf "$tarball" -C "$src" --strip-components=1
cp -R "$here/overlay/." "$src/"
# The CuBit stream producer, shared with the older C runtime.
cp "$here/../c/cubit_streams.c" "$here/../c/cubit.h" "$src/src/cubit/"

(
    cd "$src"
    ./configure --prefix="$sysroot" --target=x86_64 --disable-shared \
        CFLAGS="-O2 -g" >/dev/null
    make -j"$(nproc)" >/dev/null
    make install >/dev/null
)

gcc -O2 -g -ffreestanding -nostdinc -isystem "$sysroot/include" \
    -c "$here/crt/crt1.c" -o "$sysroot/lib/cubit-crt1.o"
install -m 0644 "$here/link.ld" "$sysroot/lib/cubit.ld"

# C++: nixpkgs' musl cross gcc (libstdc++ built against musl headers of the
# same version); cubit-c++ links it with this libc instead of its own.
nix build --inputs-from "$here/../.." nixpkgs#pkgsCross.musl64.buildPackages.gcc \
    --no-link --print-out-paths | grep -v -- '-man$' | head -1 > "$build/cross-gcc.new"
# Renamed into place: concurrent builds read it through cubit-c++.
mv "$build/cross-gcc.new" "$build/cross-gcc"
# Rust's musl-family std links -lunwind: the musl cross gcc's unwinder
# provides _Unwind_* (the host gcc's is built for glibc).
cross_gxx=$(cat "$build/cross-gcc")/bin/x86_64-unknown-linux-musl-g++
install -m 0644 "$("$cross_gxx" -print-file-name=libgcc_eh.a)" "$sysroot/lib/libunwind.a"
# C++ runtime where a non-C++ link driver (rustc) finds -lstdc++.
install -m 0644 "$("$cross_gxx" -print-file-name=libstdc++.a)" "$sysroot/lib/libstdc++.a"
install -m 0644 "$("$cross_gxx" -print-file-name=libsupc++.a)" "$sysroot/lib/libsupc++.a"
echo "CuBit libc: $sysroot (C++: $(cat "$build/cross-gcc"))"
