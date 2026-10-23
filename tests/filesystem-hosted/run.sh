#!/usr/bin/env bash
# Every Linux-hosted filesystem suite, in one run (inside nix develop, with
# TMPDIR outside /tmp): JBD2 replay against e2fsprogs, power cuts, the
# namespace and pressure workloads, I/O counts, Linux round-trip images,
# the service's pure core, and the cross-process coherence cases against
# Linux's own semantics (tests/fs-coherence/host.sh). Prints one line per
# suite and FILESYSTEM-HOSTED: PASS or FAIL; full output in LOG_DIR.
set -uo pipefail
tests=$(cd "$(dirname "$0")/.." && pwd)
logs=${LOG_DIR:-$(mktemp -d "${TMPDIR:-/tmp}/filesystem-hosted.XXXXXX")}
mkdir -p "$logs"
failed=0
suite() {   # name directory command...
    local name=$1 dir=$2
    shift 2
    if (cd "$tests/$dir" && "$@") > "$logs/$name.log" 2>&1; then
        echo "$name: PASS"
    else
        echo "$name: FAIL (see $logs/$name.log)"
        failed=1
    fi
}
suite build-journal filesystem-journal gprbuild -q -p -P journal.gpr
suite build-crash filesystem-journal gprbuild -q -p -P crash.gpr
suite jbd2-replay filesystem-journal python3 run.py
suite power-cuts filesystem-journal python3 crash.py
suite namespace filesystem-journal python3 namespace.py
suite pressure filesystem-journal python3 pressure.py
suite io-counts filesystem-journal python3 io_counts.py
suite build-interop filesystem-interop gprbuild -q -p -P interop.gpr
suite linux-round-trip filesystem-interop python3 run.py
suite build-server-core filesystem-server-core gprbuild -q -p -P server_core.gpr
suite server-core filesystem-server-core build/main
suite coherence-linux fs-coherence ./host.sh
echo "logs: $logs"
if [ "$failed" = 0 ]; then echo "FILESYSTEM-HOSTED: PASS"; else echo "FILESYSTEM-HOSTED: FAIL"; fi
exit "$failed"
