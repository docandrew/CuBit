#!/usr/bin/env bash
# Building an app ON CuBit, through ccl-remote (SELF-002, REMOTE-001 "remote
# staging"), the guest test device-build: prepares what the image carries
# into $1 (default kernel/isodir/boot) and tests/device-builds/build:
#   - the GCC port's toolchain and binutils' as and ld, as installed at
#     @system/toolchain, with the identity build-params.ccl pins
#     (gnat-15.3.0-cubit), and ccl-build.app (as tests/ccl-build-native);
#   - root/: the system volume the image boots with, the installed root on
#     QEMU's UFS disk (@ufs:0, which storage.system-volume makes the
#     filesystem's @system): toolchain/, tools/ccl-manifest.app (the
#     manifest step's tool on CuBit, build-params.ccl; make -C kernel
#     ccl-manifest-app), storehouse/ (empty: every step runs on CuBit),
#     work/remote/ (the source subset an app's build reads, without the
#     app: the host puts that) and work/mail/;
#   - root.ext2: that tree as an ext2 image (4 KiB blocks), sized to what
#     it holds plus Build_Headroom_MiB for the Storehouse and the products.
#     The one device-build payload: the headless test puts it in a GPT
#     partition (tests/device-builds/make-disk.py); the device-build live
#     image (make -C kernel usb-live-build-uefi-iso) boots it as its live
#     RAM root (live-rw.ext2, @mem:0), so it also carries what the plain
#     live RAM root does (the CCL samples in work/, tmp/), and that
#     image's installer copies its files onto the installed root.
#     Reproducible: fixed times, UUID and hash seed, owner 0;
#   - the Linux reference, built by the Linux ccl-build (and Linux's
#     ccl-manifest) from the same subset into a private store:
#     swap-probe.app (userspace/apps/swap-probe) at build 1 and build 2
#     (Build edited as the test edits it), each one's SHA-256 in
#     expected-build-N.sha256, and hello.app (userspace/apps/hello, the
#     app with a manifest the device has never seen) in expected-hello.*.
# Run in the Nix shell under the shared build lock, after make -C kernel
# world (libc, user runtime, ccl-manifest, ccl-build).
set -euo pipefail
export NIX_HARDENING_ENABLE=""
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
out=${1:-$repo/kernel/isodir/boot}
mkdir -p "$out"
out=$(cd "$out" && pwd)
b=$here/build
tool=$repo/userspace/ccl/build/ccl-build/ccl-build
: "${CUBIT_GNAT:?run inside nix develop}" "${CUBIT_BINUTILS:?run inside nix develop}"
# Room for the Storehouse's entries, the steps' scratch copies and the
# exported products of a few small builds.
Build_Headroom_MiB=96
# The root image's fixed identity and times (reproducible builds): its
# files' times, the filesystem's UUID and directory hash seed.
Root_Epoch=0
Root_UUID=5b0e7c2a-2f4d-4c38-9e5a-00000000d0b1
Root_Hash_Seed=5b0e7c2a-2f4d-4c38-9e5a-00000000d0b2
mkdir -p "$b"
remove() {  # remove PATH...: also Storehouse entries, which are read-only
    local p
    for p in "$@"; do
        if [[ -e $p ]]; then chmod -R u+w "$p"; rm -rf "$p"; fi
    done
}

# The toolchain, as the image installs it.
bash "$repo/tests/binutils/build.sh" "$out" >/dev/null
bash "$repo/userspace/ports/gcc/build.sh" "$out" >/dev/null
root=$b/root
remove "$root"
mkdir -p "$root/work/remote" "$root/work/mail/to-host" "$root/work/mail/to-device" "$root/tmp"
# What the plain live RAM root holds too (kernel/Makefile laptop_live_rw.img):
# editable copies of the CCL samples in work/.
cp "$repo"/userspace/ccl/samples/*.ccl "$root/work/"
cp -r "$repo/userspace/ports/gcc/build/toolchain" "$root/toolchain"
mkdir -p "$root/toolchain/x86_64-linux-musl/bin"
cp "$out/as.app" "$root/toolchain/x86_64-linux-musl/bin/as"
cp "$out/ld.app" "$root/toolchain/x86_64-linux-musl/bin/ld"
identity=$("$tool" hash-tree "$root/toolchain")
pinned=$(grep -A1 'Toolchain "gnat-15.3.0-cubit"' "$repo/build-params.ccl" | grep -Eo '[0-9a-f]{64}' | head -1 || true)
if [[ $identity != "$pinned" ]]; then
    echo "device-build: the CuBit toolchain tree has identity $identity;" \
         "build-params.ccl pins ${pinned:-none} for gnat-15.3.0-cubit" >&2
    exit 1
fi
make -s -C "$repo/kernel" ccl-build-app >/dev/null
# The manifest step's tool on CuBit, where build-params.ccl names it.
make -s -C "$repo/kernel" ccl-manifest-app >/dev/null
mkdir -p "$root/tools" "$root/storehouse"
cp "$repo/userspace/ccl/build/manifest-cubit/ccl-manifest.app" "$root/tools/ccl-manifest.app"

# The source subset an Ada app's build reads (as tests/ccl-build-native's
# for ps2, without the input library), in the workspace.
src=$root/work/remote
copy() {
    local p
    for p in "$@"; do
        mkdir -p "$src/$(dirname "$p")"
        cp -pr "$repo/$p" "$src/$p"
    done
}
copy build-params.ccl build-catalog.ccl \
    userspace/ccl/interfaces/build.ccl userspace/ccl/interfaces/executable-manifest.ccl \
    userspace/ccl/interfaces/setting-catalog.ccl userspace/ccl/interfaces/settings \
    userspace/ccl/catalogs/driver-services.ccl userspace/ccl/catalogs/native-runtime-services.ccl \
    userspace/runtime/ada_source_path userspace/runtime/ada_object_path \
    userspace/runtime/gnat userspace/runtime/adalib \
    shared/time shared/interrupts shared/launch shared/publications shared/capabilities shared/boot \
    userspace/c/link.ld userspace/c/build/crt0.o

# The apps as the host puts them: swap-probe at build 1, and build 2 as the
# test edits it; hello as it is.
app=userspace/apps/swap-probe
for n in 1 2; do
    remove "$b/app-$n"
    mkdir -p "$b/app-$n"
    cp -p "$repo/$app/component.ccl" "$repo/$app/manifest.ccl" "$b/app-$n/"
    sed "s/Build : constant String := \"1\";/Build : constant String := \"$n\";/" \
        "$repo/$app/main.adb" > "$b/app-$n/main.adb"
    touch -r "$repo/$app/main.adb" "$b/app-$n/main.adb"
done
grep -q 'Build : constant String := "2";' "$b/app-2/main.adb"
hello=userspace/apps/hello
remove "$b/app-hello"
mkdir -p "$b/app-hello"
cp -p "$repo/$hello/component.ccl" "$repo/$hello/manifest.ccl" "$repo/$hello/main.adb" "$b/app-hello/"

# The Linux reference of each, from the subset plus the app (and Linux's
# manifest tool, where build-params.ccl names it for Linux), in a private
# store (never the shared one).
reference=$b/reference
remove "$b/host-store" "$b/seed" "$b/seed.log"
reference_build() {  # reference_build APP SOURCES NAME: expected-NAME.app and .sha256
    local product
    remove "$reference" "$b/host-out"
    cp -r "$src" "$reference"
    mkdir -p "$reference/userspace/ccl/build/manifest" "$reference/$1"
    cp -p "$repo/userspace/ccl/build/manifest/ccl-manifest" "$reference/userspace/ccl/build/manifest/"
    cp -p "$2/"* "$reference/$1/"
    "$tool" build --store "$b/host-store" --source-root "$reference" \
        --import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS" \
        --out "$b/host-out" "$reference/$1/component.ccl" > "$b/host-$3.log"
    product=$b/host-out/$1/build/$(basename "$1").app
    sha256sum "$product" | cut -d' ' -f1 > "$b/expected-$3.sha256"
    cp "$product" "$b/expected-$3.app"
}
reference_build "$app" "$b/app-1" build-1
reference_build "$app" "$b/app-2" build-2
reference_build "$hello" "$b/app-hello" hello
remove "$reference" "$b/host-out"

# The RAM root image: its contents plus the headroom, in whole MiB.
used_kib=$(du -sk --apparent-size "$root" | cut -f1)
files=$(find "$root" | wc -l)
size_mib=$(( (used_kib * 5 / 4) / 1024 + files * 4 / 1024 + Build_Headroom_MiB ))
# From a tar stream, so names are in order and owners and times fixed.
rm -f "$b/root.ext2" "$b/root.tar"
tar --sort=name --owner=0 --group=0 --numeric-owner --mtime="@$Root_Epoch" \
    -C "$root" -cf "$b/root.tar" .
SOURCE_DATE_EPOCH=$Root_Epoch mke2fs -q -t ext2 -b 4096 -m 0 -F -U "$Root_UUID" \
    -E "hash_seed=$Root_Hash_Seed" -d "$b/root.tar" "$b/root.ext2" "${size_mib}M"
rm -f "$b/root.tar"
echo "device-build: toolchain $identity; ccl-manifest.app $(sha256sum "$root/tools/ccl-manifest.app" | cut -d' ' -f1);" \
     "root $((used_kib / 1024)) MiB in $files files, image ${size_mib} MiB;" \
     "build 1 $(cat "$b/expected-build-1.sha256"); build 2 $(cat "$b/expected-build-2.sha256");" \
     "hello $(cat "$b/expected-hello.sha256")"
