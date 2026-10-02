#!/usr/bin/env bash
# Repository root, Nix environment, shared build lock.
set -euo pipefail
bash tests/compositor/build-metrics-desktop.sh
for fixture in metrics-load metrics-load-observer; do
 (
  cd kernel
  mkdir -p ../tests/compositor/$fixture/build/generated
  ../userspace/ccl/build/manifest/ccl-manifest \
    ../userspace/ccl/catalogs/native-runtime-services.ccl \
    ../tests/compositor/$fixture/manifest.ccl \
    --ada-output ../tests/compositor/$fixture/build/generated/ccl_manifest_bindings.ads \
    > ../tests/compositor/$fixture/build/manifest.S
  alr exec -- gcc -c ../tests/compositor/$fixture/build/manifest.S \
    -o ../tests/compositor/$fixture/build/manifest.o
  if [ "$fixture" = metrics-load ]; then project=load; else project=observer; fi
  alr exec -- gprbuild -p -P ../tests/compositor/$fixture/$project.gpr
  cp ../tests/compositor/$fixture/build/*.app isodir/boot/
 )
done
