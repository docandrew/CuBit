#!/usr/bin/env bash
# Compile-time metadata rotation fixture; never leave it staged for users.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
restore_display() {
    make -C "$ROOT_DIR/kernel" display display-check CUBIT_DISPLAY_TEST_MODE=production
}
trap restore_display EXIT
make -C "$ROOT_DIR/kernel" display display-check CUBIT_DISPLAY_TEST_MODE=output-rebind
CUBIT_DISPLAY_TEST_MODE=output-rebind "$ROOT_DIR/tests/headless/run.sh" \
    --test display-grants "$@"
