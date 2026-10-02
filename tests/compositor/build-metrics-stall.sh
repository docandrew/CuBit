#!/usr/bin/env bash
# Repository root, Nix, shared build lock. Uses the already built manifest tool.
set -euo pipefail
cd kernel
mkdir -p ../tests/compositor/metrics-stall/build/generated
../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl \
  ../tests/compositor/metrics-stall/manifest.ccl \
  --ada-output ../tests/compositor/metrics-stall/build/generated/ccl_manifest_bindings.ads \
  > ../tests/compositor/metrics-stall/build/manifest.S
alr exec -- gcc -c ../tests/compositor/metrics-stall/build/manifest.S \
  -o ../tests/compositor/metrics-stall/build/manifest.o
alr exec -- gprbuild -p -P ../tests/compositor/metrics-stall/stall.gpr
