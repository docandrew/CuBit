#!/usr/bin/env bash
# Hosted checks of the debugger engine (DBG-001) against the real debug-demo
# ELF (make -C kernel debug-demo), with binutils as the oracle. Run inside
# nix develop. Exit status 0 only when every check passes.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
ELF="${1:-$ROOT_DIR/userspace/apps/debug-demo/build/debug-demo.app}"
SOURCES="$ROOT_DIR/userspace/apps/debug-demo"
cd "$ROOT_DIR"
gprbuild -p -q -P tests/debug-engine/debug_engine_hosted.gpr 2>&1 | grep -v "^warning: runtimes" | grep -v "warning:" || true
BUILD_ID="$(readelf -nW "$ELF" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')"
ARGS=()
for SYMBOL in demo_world__insert demo_world__advance demo_world__run_jobs demo_world__run_sensors \
  cubit__debug_stub__checkpoint; do
    ADDRESS="$(nm "$ELF" | awk -v s="$SYMBOL" '$3 == s { print $1 }')"
    LINE="$(addr2line -e "$ELF" "0x$ADDRESS" | sed 's/.*:\([0-9]*\).*/\1/')"
    ARGS+=("$SYMBOL" "$ADDRESS" "$LINE")
done
exec tests/debug-engine/build/hosted_check "$ELF" "$SOURCES" "$BUILD_ID" "${ARGS[@]}"
