#!/usr/bin/env bash
# Repository root, Nix, shared build lock. Uses the already built manifest tool.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
cd kernel
mkdir -p ../tests/compositor/metrics-stall/build/generated
../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl \
  ../tests/compositor/metrics-stall/manifest.ccl \
  --startup-output ../tests/compositor/metrics-stall/build/generated/startup.ads \
  > ../tests/compositor/metrics-stall/build/manifest.S
alr exec -- gcc -c ../tests/compositor/metrics-stall/build/manifest.S \
  -o ../tests/compositor/metrics-stall/build/manifest.o
alr exec -- gprbuild $gprbuild_config -p -P ../tests/compositor/metrics-stall/stall.gpr
