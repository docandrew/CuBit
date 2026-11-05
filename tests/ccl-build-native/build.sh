#!/usr/bin/env bash
# ccl-build on CuBit (docs/ccl-build.md, "On CuBit"; SELF-002), the guest
# test ccl-build-native: builds what the disk image carries into $1
# (default kernel/isodir/boot) and tests/ccl-build-native/build:
#   - the GCC port's toolchain (userspace/ports/gcc) and binutils' as and ld
#     (tests/binutils/build.sh, which also builds binutils-compare.app), and
#     the toolchain tree as installed at @nvme:0/toolchain, which must have
#     the identity build-params.ccl pins for CuBit (gnat-15.3.0-cubit);
#   - ccl-build.app (make -C kernel ccl-build-app) and the test launcher,
#     ccl-build-check.app;
#   - cubit-src: the source subset ps2.drv's build reads (the program, the
#     input library, the prebuilt user runtime and crt0.o, the declarations
#     and the manifest tool);
#   - the Linux reference: ps2.drv built by the Linux ccl-build from that
#     subset into a private store (never the shared one);
#   - seed: the manifest step's result from that store, by identity. The
#     manifest tool is a Linux program in this slice (its step identity
#     names its bytes, so the step is the same on both hosts), and CuBit
#     launches only programs a launch table names exactly, which a tool
#     placed in a step directory is not: the image carries the step's
#     result instead (binary substitution), and every other step runs on
#     CuBit.
# Run in the Nix shell under the shared build lock, after make -C kernel
# world (libc, user runtime, ccl-manifest, ccl-build).
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
mkdir -p "$out"
out=$(cd "$out" && pwd)   # the scripts it runs change directory
b=$here/build
tool=$repo/userspace/ccl/build/ccl-build/ccl-build
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$repo/kernel" print-native-gprbuild-config)}
: "${CUBIT_GNAT:?run inside nix develop}" "${CUBIT_BINUTILS:?run inside nix develop}"
mkdir -p "$b" "$out"
remove() {  # remove PATH...: also Storehouse entries, which are read-only
    local p
    for p in "$@"; do
        if [[ -e $p ]]; then chmod -R u+w "$p"; rm -rf "$p"; fi
    done
}

# The toolchain, and the tree as the image installs it.
bash "$repo/tests/binutils/build.sh" "$out" >/dev/null
bash "$repo/userspace/ports/gcc/build.sh" "$out" >/dev/null
remove "$b/toolchain"
cp -r "$repo/userspace/ports/gcc/build/toolchain" "$b/toolchain"
mkdir -p "$b/toolchain/x86_64-linux-musl/bin"
cp "$out/as.app" "$b/toolchain/x86_64-linux-musl/bin/as"
cp "$out/ld.app" "$b/toolchain/x86_64-linux-musl/bin/ld"
identity=$("$tool" hash-tree "$b/toolchain")
pinned=$(grep -A1 'Toolchain "gnat-15.3.0-cubit"' "$repo/build-params.ccl" | grep -Eo '[0-9a-f]{64}' | head -1 || true)
echo "$identity" > "$b/toolchain.identity"
if [[ $identity != "$pinned" ]]; then
    echo "ccl-build-native: the CuBit toolchain tree has identity $identity;" \
         "build-params.ccl pins ${pinned:-none} for gnat-15.3.0-cubit (update the pin after a toolchain" \
         "or libc change)" >&2
    exit 1
fi

# ccl-build.app and the launcher.
make -s -C "$repo/kernel" ccl-build-app >/dev/null
mkdir -p "$here/check/build/generated"
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/check/manifest.ccl" \
    --schema "$repo/userspace/ccl/interfaces/executable-manifest.ccl" \
    --startup-output "$here/check/build/generated/startup.ads" --output "$here/check/build/manifest.S"
as --64 "$here/check/build/manifest.S" -o "$here/check/build/manifest.o"
rm -f "$here/check/build/ccl-build-check.app"
(cd "$repo/kernel" && alr exec -- gprbuild $gprbuild_config -q -P "$here/check/ccl_build_check.gpr")
cp "$here/check/build/ccl-build-check.app" "$out/ccl-build-check.app"

# The source subset: what ps2's steps read, and the declarations.
src=$b/cubit-src
remove "$src"
mkdir -p "$src"
copy() {  # copy PATH...: files and whole directories, relative to the checkout
    local p
    for p in "$@"; do
        mkdir -p "$src/$(dirname "$p")"
        cp -pr "$repo/$p" "$src/$p"
    done
}
copy_files() {  # copy_files DIRECTORY: its files, not its subdirectories
    mkdir -p "$src/$1"
    find "$repo/$1" -maxdepth 1 -type f -exec cp -p {} "$src/$1/" \;
}
copy build-params.ccl build-catalog.ccl \
    userspace/ccl/interfaces/build.ccl userspace/ccl/interfaces/executable-manifest.ccl \
    userspace/ccl/interfaces/setting-catalog.ccl userspace/ccl/interfaces/settings \
    userspace/ccl/catalogs/driver-services.ccl userspace/ccl/catalogs/native-runtime-services.ccl \
    userspace/ccl/build/manifest/ccl-manifest \
    userspace/runtime/ada_source_path userspace/runtime/ada_object_path \
    userspace/runtime/gnat userspace/runtime/adalib \
    shared/time shared/interrupts shared/launch shared/publications shared/capabilities shared/boot \
    userspace/c/link.ld userspace/c/build/crt0.o
copy_files userspace/services/ps2
copy_files userspace/lib/input

# The Linux reference, from the same subset, in a private store.
remove "$b/host-store" "$b/host-out"
"$tool" build --store "$b/host-store" --source-root "$src" \
    --import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS" \
    --out "$b/host-out" "$src/userspace/services/ps2/component.ccl" > "$b/host-build.log"
cp "$b/host-out/userspace/services/ps2/build/ps2.drv" "$b/expected-ps2.drv"

# The manifest step's result, by identity.
remove "$b/seed"
python3 -I "$here/seed.py" "$b/host-store" "$b/host-build.log" \
    "manifest userspace/services/ps2/manifest.ccl" "$b/seed" > "$b/seed.log"
echo "ccl-build-native: toolchain $identity; $(grep -c '^ran ' "$b/host-build.log") steps on Linux; seed $(tail -1 "$b/seed.log")"
