#!/usr/bin/env bash
# Build the metrics-latency outlet fixture twice (MET-002): with and without
# the com.cubit-os.stdmetric outlet in its manifest. Inside the Nix
# environment, under the shared build lock, after
# `make -C kernel user_runtime ccl-manifest`.
set -euo pipefail
fixture=$(cd "$(dirname "$0")" && pwd)
kernel=$fixture/../../../kernel
cd "$kernel"
for variant in declared undeclared; do
  mkdir -p "$fixture/build/$variant"
  ../userspace/ccl/build/manifest/ccl-manifest \
    ../userspace/ccl/catalogs/native-runtime-services.ccl "$fixture/$variant.ccl" \
    > "$fixture/build/$variant/manifest.S"
  alr exec -- gcc -c "$fixture/build/$variant/manifest.S" -o "$fixture/build/$variant/manifest.o"
  alr exec -- gprbuild -q -p -P "$fixture/metrics_outlet_fixture.gpr" -XVARIANT="$variant"
  cp "$fixture/build/$variant/metrics-$variant.app" isodir/boot/
done
