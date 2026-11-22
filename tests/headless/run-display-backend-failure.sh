#!/usr/bin/env bash
# Run inside nix develop. Keep fault injection out of ordinary desktop boots.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
restore_production() {
    make -C "$ROOT_DIR/kernel" display virtio-gpu display-check CUBIT_GPU_TEST_MODE=production
}
trap restore_production EXIT
make -C "$ROOT_DIR/kernel" display virtio-gpu display-check CUBIT_GPU_TEST_MODE=clear-failure
CUBIT_GPU_TEST_MODE=clear-failure "$ROOT_DIR/tests/headless/run.sh" "$@" --test display-dual-output
