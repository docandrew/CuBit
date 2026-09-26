#!/usr/bin/env bash
# Mutation check for the TCP proofs: each mutant is a plausible bug; gnatprove
# must fail to prove it (a mutant that still proves is a weak specification).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../userspace/net/src
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # name file old new
    total=$((total + 1))
    rm -rf "$work/src" && cp -r "$src" "$work/src"
    python3 - "$work/src/$2" "$3" "$4" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old)
open(p, 'w').write(s.replace(old, new))
PY
    # --checks-as-errors: any unproved check fails the run (nonzero exit).
    if (cd "$here" && gnatprove -P net_tcp_tests.gpr -XNET_SRC="$work/src" -j0 \
          --level=2 --timeout=30 --checks-as-errors=on -u "$2" >/dev/null 2>&1); then
        echo "SURVIVED $1"
    else
        echo "killed   $1"; killed=$((killed + 1))
    fi
}
mutant "skip measured the wrong way" tcp_acceptance.adb \
    "Skip   := Distance (Seg_Seq, Rcv_Nxt);" "Skip   := Distance (Rcv_Nxt, Seg_Seq);"
mutant "window room ignores the offset" tcp_acceptance.adb \
    "Remaining := Rcv_Wnd - Offset;" "Remaining := Rcv_Wnd;"
mutant "backoff can shrink the RTO" tcp_rto.adb \
    "E.RTO := Unsigned_32'Min (2 * E.RTO, Maximum_RTO);" \
    "E.RTO := Unsigned_32'Min (E.RTO / 2 + Minimum_RTO, Maximum_RTO);"
mutant "RTTVAR weight overflows its range" tcp_rto.adb \
    "E.RTTVAR := (3 * E.RTTVAR + Deviation) / 4;" "E.RTTVAR := 3 * E.RTTVAR + Deviation;"
echo "mutants killed: $killed/$total"
