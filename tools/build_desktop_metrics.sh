#!/usr/bin/env bash
# Repository root, Nix environment and shared build lock. The caller builds
# desktop/metricsvc prerequisites; this helper adds the publishing variant.
set -euo pipefail
if [ "$#" -gt 1 ] || { [ "$#" -eq 1 ] && [ "$1" != --stage ]; }; then
    echo "usage: build_desktop_metrics.sh [--stage]" >&2
    exit 2
fi
build_directory=$(python3 tools/desktop_build_variant.py --metrics on)
python3 - <<'PYGEN'
from pathlib import Path
base = Path('userspace/services/desktop')
out = base / 'build-metrics-manifest'
(out / 'generated').mkdir(parents=True, exist_ok=True)
source = (base / 'manifest.ccl').read_text().rstrip()
if not source.endswith(')') or '(request-service metrics ' in source:
    raise SystemExit('unexpected base Desktop manifest; review metrics overlay')
(out / 'manifest.ccl').write_text(source[:-1] + '\n  (request-service metrics read-write metrics))\n')
PYGEN
cd kernel
../userspace/ccl/build/manifest/ccl-manifest \
  ../userspace/ccl/catalogs/native-runtime-services.ccl \
  ../userspace/services/desktop/build-metrics-manifest/manifest.ccl \
  --ada-output ../userspace/services/desktop/build-metrics-manifest/generated/ccl_manifest_bindings.ads \
  > ../userspace/services/desktop/build-metrics-manifest/manifest.S
alr exec -- gcc -c ../userspace/services/desktop/build-metrics-manifest/manifest.S \
  -o ../userspace/services/desktop/build-metrics-manifest/manifest.o
# Preserve explicitly selected backend, timing, storage and test scenarios.
if [ "${CUBIT_COMPOSITOR:-legacy}" = mesa ]; then
  : "${CUBIT_MESA_BUILD:?Set CUBIT_MESA_BUILD to the existing native Mesa build}"
  python3 ../tools/build_mesa_desktop.py "$CUBIT_MESA_BUILD" --metrics on --scenario-output
else
alr exec -- gprbuild -p -P ../userspace/services/desktop/desktop.gpr \
  -XCUBIT_COMPOSITOR="${CUBIT_COMPOSITOR:-legacy}" \
  -XCUBIT_COMPOSITOR_TIMING="${CUBIT_COMPOSITOR_TIMING:-off}" \
  -XCUBIT_COMPOSITOR_STORAGE="${CUBIT_COMPOSITOR_STORAGE:-production}" \
  -XCUBIT_DISPLAY_TEST_MODE="${CUBIT_DISPLAY_TEST_MODE:-production}" \
  -XCUBIT_COMPOSITOR_METRICS=on
fi
if [ "${1:-}" = --stage ]; then
    cp "../userspace/services/desktop/$build_directory/desktop.svc" isodir/boot/desktop.svc
fi
