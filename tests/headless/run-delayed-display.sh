#!/usr/bin/env bash
# Run from the Nix development environment. Never leave the delayed service
# staged for an ordinary desktop boot, including when a build/test fails.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
restore_display() {
    make -C "$ROOT_DIR/kernel" display desktop CUBIT_DISPLAY_TEST_MODE=production
}
trap restore_display EXIT
make -C "$ROOT_DIR/kernel" display desktop input-stress CUBIT_DISPLAY_TEST_MODE=delayed
CUBIT_DISPLAY_TEST_MODE=delayed "$ROOT_DIR/tests/headless/run.sh" "$@" --test input-stream
