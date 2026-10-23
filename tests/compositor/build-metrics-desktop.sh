#!/usr/bin/env bash
# Run from the repository root in Nix while holding coordination/build.lock.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
make -C kernel desktop metricsvc
bash tools/build_desktop_metrics.sh
(
 cd kernel
 mkdir -p ../tests/compositor/metrics-observer/build/generated
 ../userspace/ccl/build/manifest/ccl-manifest \
   ../userspace/ccl/catalogs/native-runtime-services.ccl \
   ../tests/compositor/metrics-observer/manifest.ccl \
   --startup-output ../tests/compositor/metrics-observer/build/generated/startup.ads \
   > ../tests/compositor/metrics-observer/build/manifest.S
 alr exec -- gcc -c ../tests/compositor/metrics-observer/build/manifest.S \
   -o ../tests/compositor/metrics-observer/build/manifest.o
 alr exec -- gprbuild $gprbuild_config -p -P ../tests/compositor/metrics-observer/observer.gpr
 cp ../tests/compositor/metrics-observer/build/desktop-metrics-observer.app isodir/boot/
)
