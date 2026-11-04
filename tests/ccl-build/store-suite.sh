#!/usr/bin/env bash
# The Storehouse's hosted reliability tests, none of which need the native
# toolchain (docs/ccl-build.md, "Reliability"): unit tests, a kill at every
# store-changing system call (crash.sh), parallel builders (race.sh), dead
# and reused process numbers (liveness.sh), generations and garbage
# collection (generations.sh). tests/ccl-build/run.sh runs them too, then the
# real builds. Run inside `nix develop` with TMPDIR set, after building the
# tests (ccl_build_tests.gpr):
#   bash tests/ccl-build/store-suite.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
: "${TMPDIR:?set TMPDIR outside /tmp}"
scratch=$(mktemp -d "$TMPDIR/ccl-build-store-suite.XXXXXX")
trap 'chmod -R u+w "$scratch" 2>/dev/null; rm -rf "$scratch"' EXIT
tool=$repo/userspace/ccl/build/ccl-build/ccl-build
driver=$here/build/store_driver
failed=0
run() {  # run NAME COMMAND...: one suite, its last line, and its status
    local name=$1; shift
    if "$@" >"$scratch/$name.log" 2>&1; then echo "PASS $name: $(tail -1 "$scratch/$name.log")"
    else failed=$((failed + 1)); echo "FAIL $name:"; grep -E '^FAIL|failures|failed' "$scratch/$name.log" | head -20; fi
}
mkdir -p "$scratch/unit" "$scratch/tree/sub/deeper"
echo a >"$scratch/tree/a.txt"; echo c >"$scratch/tree/sub/deeper/c.txt"
printf '#!/bin/sh\n' >"$scratch/tree/tool"; chmod +x "$scratch/tree/tool"; ln -s ../a.txt "$scratch/tree/sub/link"
run core "$here/build/core_tests"
run store "$here/build/store_tests" "$scratch/unit"
run crash-step bash "$here/crash.sh" step "$driver" "$scratch/crash-step"
run crash-import bash "$here/crash.sh" tool "$tool" "$scratch/crash-import" -- import --store @STORE@ "$scratch/tree"
run race bash "$here/race.sh" step "$driver" "$scratch/race" 16
run liveness bash "$here/liveness.sh"
run generations bash "$here/generations.sh" "$tool"
echo "store suite: $failed failed"
[[ $failed == 0 ]]
