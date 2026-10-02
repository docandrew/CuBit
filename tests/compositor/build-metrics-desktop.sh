#!/usr/bin/env bash
# Run from the repository root in Nix while holding coordination/build.lock.
set -euo pipefail
make -C kernel desktop metricsvc
bash tools/build_desktop_metrics.sh
(
 cd kernel
 mkdir -p ../tests/compositor/metrics-observer/build/generated
 ../userspace/ccl/build/manifest/ccl-manifest \
   ../userspace/ccl/catalogs/native-runtime-services.ccl \
   ../tests/compositor/metrics-observer/manifest.ccl \
   --ada-output ../tests/compositor/metrics-observer/build/generated/ccl_manifest_bindings.ads \
   > ../tests/compositor/metrics-observer/build/manifest.S
 alr exec -- gcc -c ../tests/compositor/metrics-observer/build/manifest.S \
   -o ../tests/compositor/metrics-observer/build/manifest.o
 alr exec -- gprbuild -p -P ../tests/compositor/metrics-observer/observer.gpr
 cp ../tests/compositor/metrics-observer/build/desktop-metrics-observer.app isodir/boot/
)
