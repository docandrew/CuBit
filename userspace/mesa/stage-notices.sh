#!/usr/bin/env bash
# Preserve upstream notices from the exact source used to build the CuBit ELF.
# This is a notice bundle, not a claim that every upstream license applies to
# the softpipe binary. Per-file notices remain in the corresponding sources.
set -euo pipefail
source_tree=${1:?Mesa source directory required}
destination=${2:?new notice directory required}
root=$(cd "$(dirname "$0")/../.." && pwd)
test -f "$source_tree/docs/license.rst"
test -f "$source_tree/licenses/MIT"
test -f "$source_tree/VERSION"
if [ -e "$destination" ]; then
    echo "Refusing to overwrite notice bundle: $destination" >&2
    exit 1
fi
mkdir -p "$destination"
cp -R "$source_tree/licenses" "$destination/licenses"
cp "$source_tree/docs/license.rst" "$destination/UPSTREAM-LICENSE.rst"
cp "$source_tree/VERSION" "$destination/MESA-VERSION"
cp "$root/tests/mesa-anv/source.nix" "$destination/SOURCE.nix"
cp "$root/tests/mesa-software/cubit-platform.patch" "$destination/CUBIT-PLATFORM.patch"
cp "$root/userspace/mesa/NOTICE.md" "$destination/README.md"
# Keep complete upstream per-file notices, including headers and generator
# inputs that are not represented by compile_commands.json. Archive the exact
# adapted tree rather than guessing which comment fragments are licenses.
# Fixed archive metadata makes repeated packaging of this tree reproducible.
tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner \
    -C "$source_tree" -cf - . | gzip -n > "$destination/MESA-SOURCE.tar.gz"
