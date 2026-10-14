#!/usr/bin/env bash
#  devmgr's manifest reader (userspace/services/devmgr/driver_manifests):
#  real manifests compiled by ccl-manifest into ELF objects, read back and
#  checked, then truncated and corrupted at every byte (the reader must
#  never fail a check and must only accept valid tables). --prove also runs
#  gnatprove at level 2. Run in the Nix shell from anywhere (after
#  `make -C kernel ccl-manifest`).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
mkdir -p "$here/build/objects"
for d in hda mixer ata netstack; do
    ../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/driver-services.ccl \
        ../userspace/services/$d/manifest.ccl > "$here/build/objects/$d.S"
    alr exec -- gcc -c "$here/build/objects/$d.S" -o "$here/build/objects/$d.o"
done
alr exec -- gprbuild -q -p -P "$here/driver_manifests_tests.gpr"
(cd "$here/build" && ./main)
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/driver_manifests_tests.gpr" -u driver_manifests.adb -j2
fi
