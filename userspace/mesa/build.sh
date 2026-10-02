#!/usr/bin/env bash
# Invoked from the repository Nix environment, holding coordination/build.lock.
# Build a CuBit ELF, not a Linux-hosted Mesa demonstration.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
build="$root/userspace/mesa/build"
mkdir -p "$build"
source_store=$(nix eval --impure --raw --expr 'toString (import ./tests/mesa-anv/source.nix)')
# Separate prepared sources/configurations when the pinned input or adaptation
# changes. Never patch the store or silently reuse a stale source snapshot.
key=$(sha256sum tests/mesa-anv/source.nix \
    tests/mesa-software/cubit-platform.patch \
    tests/mesa-software/prepare-cubit-source.sh \
    tests/mesa-software/cubit-cross.ini \
    tests/mesa-software/configure-cubit-softpipe.sh | sha256sum | cut -c1-16)
source_tree="$build/source-$key"
build_tree="$build/native-$key"
if [ ! -d "$source_tree" ]; then
    scratch=$(mktemp -d "$build/prepare.XXXXXX")
    bash tests/mesa-software/prepare-cubit-source.sh "$source_store" "$scratch/tree"
    # Moving a directory between parents updates its '..' entry on Linux.
    # The store copy retains a read-only root; only our private copy is changed.
    chmod u+w "$scratch/tree"
    mv "$scratch/tree" "$source_tree"
    rmdir "$scratch"
fi
export CUBIT_MESA_SOURCE="$source_tree" CUBIT_MESA_BUILD="$build_tree"
nix-shell tests/mesa-anv/host-shell.nix --run '
    set -eu
    if [ ! -f "$CUBIT_MESA_BUILD/build.ninja" ]; then
        bash tests/mesa-software/configure-cubit-softpipe.sh "$CUBIT_MESA_SOURCE" "$CUBIT_MESA_BUILD"
    fi
    bash tests/mesa-software/build-native-opengl.sh "$CUBIT_MESA_SOURCE" "$CUBIT_MESA_BUILD"
'
# Keep each notice snapshot immutable, including when only packaging changes.
# Staging refuses existing destinations; a fresh private directory avoids stale
# upstream license files lingering after a future source-version change.
notice_parent=$(mktemp -d "$build/notices.XXXXXX")
bash userspace/mesa/stage-notices.sh "$source_tree" "$notice_parent/mesa"
cp "$build_tree/native-mesa-cube.map" "$notice_parent/mesa/LINK-MAP.txt"
python3 userspace/mesa/link-inventory.py "$build_tree" "$source_tree" \
    > "$notice_parent/mesa/LINKED-SOURCES.json"
sha256sum "$build_tree/native-mesa-cube.app" > "$notice_parent/mesa/ELF-SHA256.txt"
echo "Mesa notices staged: $notice_parent/mesa"
install -m 0755 "$build_tree/native-mesa-cube.app" kernel/isodir/boot/mesa-cube.app
# Trusted image wrapper consumes this only after a successful build/stage.
printf '%s\n' "$notice_parent/mesa" > "$build/notice-path"
echo "Mesa software cube staged: $root/kernel/isodir/boot/mesa-cube.app"
