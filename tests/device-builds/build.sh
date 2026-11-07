#!/usr/bin/env bash
# Building an app ON CuBit, through ccl-remote (SELF-002, REMOTE-001 "remote
# staging"), the guest test device-build: prepares what the image carries
# into $1 (default kernel/isodir/boot) and tests/device-builds/build:
#   - the GCC port's toolchain and binutils' as and ld, as installed at
#     @system/toolchain, with the identity build-params.ccl pins
#     (gnat-15.3.0-cubit), and ccl-build.app (as tests/ccl-build-native);
#   - root/: the system volume the image boots with, the installed root on
#     QEMU's UFS disk (@ufs:0, which storage.system-volume makes the
#     filesystem's @system): toolchain/, storehouse/ (the manifest step's
#     seed, below), work/remote/ (the source subset an app's build reads,
#     without the app: the host puts that) and work/mail/;
#   - root.ext2: that tree as an ext2 image (4 KiB blocks), sized to what
#     it holds plus Build_Headroom_MiB for the Storehouse and the products
#     (run.sh puts it in a GPT partition: tests/device-builds/make-disk.py);
#   - the Linux reference: swap-probe.app (userspace/apps/swap-probe) built
#     by the Linux ccl-build from the same subset into a private store, at
#     build 1 and build 2 (Build edited as the test edits it), each one's
#     SHA-256 in expected-build-N.sha256;
#   - seed: the manifest step's result from that store, by identity (the
#     manifest tool is still a Linux program: tests/ccl-build-native).
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
mkdir -p "$root/work/remote" "$root/work/mail/to-host" "$root/work/mail/to-device"
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
    userspace/ccl/build/manifest/ccl-manifest \
    userspace/runtime/ada_source_path userspace/runtime/ada_object_path \
    userspace/runtime/gnat userspace/runtime/adalib \
    shared/time shared/interrupts shared/launch shared/publications shared/capabilities shared/boot \
    userspace/c/link.ld userspace/c/build/crt0.o

# The app as the host puts it (build 1), and build 2 as the test edits it.
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

# The Linux reference of each build, from the subset plus the app, in a
# private store (never the shared one); the manifest step's seed from it.
reference=$b/reference
remove "$b/host-store" "$b/seed"
for n in 1 2; do
    remove "$reference"
    cp -r "$src" "$reference"
    mkdir -p "$reference/$app"
    cp -p "$b/app-$n/"* "$reference/$app/"
    remove "$b/host-out"
    "$tool" build --store "$b/host-store" --source-root "$reference" \
        --import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS" \
        --out "$b/host-out" "$reference/$app/component.ccl" > "$b/host-build-$n.log"
    sha256sum "$b/host-out/$app/build/swap-probe.app" | cut -d' ' -f1 > "$b/expected-build-$n.sha256"
    cp "$b/host-out/$app/build/swap-probe.app" "$b/expected-build-$n.app"
done
remove "$reference" "$b/host-out"
# Both builds share the manifest step (only main.adb differs).
python3 -I "$repo/tests/ccl-build-native/seed.py" "$b/host-store" "$b/host-build-1.log" \
    "manifest $app/manifest.ccl" "$root/storehouse" > "$b/seed.log"

# The RAM root image: its contents plus the headroom, in whole MiB.
used_kib=$(du -sk --apparent-size "$root" | cut -f1)
files=$(find "$root" | wc -l)
size_mib=$(( (used_kib * 5 / 4) / 1024 + files * 4 / 1024 + Build_Headroom_MiB ))
rm -f "$b/root.ext2"
mke2fs -q -t ext2 -b 4096 -m 0 -F -d "$root" "$b/root.ext2" "${size_mib}M"
echo "device-build: toolchain $identity; root $((used_kib / 1024)) MiB in $files files," \
     "image ${size_mib} MiB; build 1 $(cat "$b/expected-build-1.sha256");" \
     "build 2 $(cat "$b/expected-build-2.sha256"); $(tail -1 "$b/seed.log")"
