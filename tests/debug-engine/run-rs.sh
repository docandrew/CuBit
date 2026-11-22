#!/usr/bin/env bash
# Hosted checks of the debugger engine's Rust support (DBG-001) against the
# real debug-demo-rs ELF (userspace/apps/debug-demo-rs/build.sh), with
# binutils as the oracle. Run inside nix develop.
set -eu
ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
ELF="${1:-$ROOT_DIR/userspace/rust/build/debug-demo-rs.app}"
SOURCES="$ROOT_DIR/userspace/apps/debug-demo-rs/src"
cd "$ROOT_DIR"
mkdir -p tests/debug-engine/build/rust
gprbuild -p -q -P tests/debug-engine/debug_engine_rust_hosted.gpr 2>&1 | grep -v "warning:" || true
BUILD_ID="$(readelf -nW "$ELF" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')"
SYMBOLS="tests/debug-engine/build/rust/symbols.tsv"
# c++filt names crates with their hash (core[66dfa062870c092d]): the engine
# shows paths without them.
nm "$ELF" | awk '$3 ~ /^_R/ { print $3 }' | sort -u | while read -r SYMBOL; do
    printf '%s\t%s\n' "$SYMBOL" "$(printf '%s\n' "$SYMBOL" | c++filt | sed 's/\[[0-9a-f]*\]//g')"
done > "$SYMBOLS"
ARGS=()
for NAME in debug_demo_rs::insert debug_demo_rs::advance debug_demo_rs::run_jobs \
  debug_demo_rs::run_sensors cubit_debug_stub::checkpoint; do
    SYMBOL="$(awk -F'\t' -v n="$NAME" '$2 == n { print $1; exit }' "$SYMBOLS")"
    ADDRESS="$(nm "$ELF" | awk -v s="$SYMBOL" '$3 == s { print $1; exit }')"
    LINE="$(addr2line -e "$ELF" "0x$ADDRESS" | sed 's/.*:\([0-9]*\).*/\1/')"
    ARGS+=("$NAME" "$ADDRESS" "$LINE")
done
# A line-0 row (LLVM's "no source line") right after a row of main.rs: the
# engine gives it the line before it, as the debugger shows a call's line.
export DEBUG_LINE_ZERO="$(objdump --dwarf=decodedline "$ELF" 2>/dev/null |
  awk '$1 == "main.rs" && $2 ~ /^[0-9]+$/ { if ($2 == 0 && last > 0) { sub(/^0x/, "", $3); print $3, last; exit }
                                             last = $2; next } { last = 0 }')"
exec tests/debug-engine/build/hosted_rust_check "$ELF" "$SOURCES" "$BUILD_ID" "$SYMBOLS" "${ARGS[@]}"
