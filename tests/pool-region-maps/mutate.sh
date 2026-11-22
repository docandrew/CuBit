#!/usr/bin/env bash
#  Mutation check of Pool_Region_Maps: each mutant breaks a run; the
#  level 2 proof must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/pool_region_maps.ads
body=$here/../../kernel/src/pool_region_maps.adb
spec_backup=$(mktemp); body_backup=$(mktemp)
cp "$spec" "$spec_backup"; cp "$body" "$body_backup"
trap 'cp "$spec_backup" "$spec"; cp "$body_backup" "$body"; rm -f "$spec_backup" "$body_backup"' EXIT
survivors=0
mutant() {   # old new name: applied to whichever of spec and body holds old
    cp "$spec_backup" "$spec"; cp "$body_backup" "$body"
    python3 - "$spec" "$body" "$1" "$2" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
spec, body, old, new = sys.argv[1:5]
for p in (spec, body):
    s = open(p).read()
    if old in s:
        open(p, "w").write(s.replace(old, new, 1)); break
else:
    sys.exit(1)
PY
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/pool_region_maps_tests.gpr" \
          -u pool_region_maps.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package Pool_Region_Maps with" "package Pool_Region_Maps with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "pool-region-maps: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "         Candidate := Slot * 8;" "         Candidate := Slot * 8 + 1;" "a misaligned run"
mutant "         M.Clean (F) := False;
         pragma Loop_Invariant (for all G in First .. F => not M.Free (G) and then not M.Clean (G));" "         pragma Loop_Invariant (for all G in First .. F => not M.Free (G) and then not M.Clean (G));" "a taken frame stays clean"
mutant "            if not B (Candidate + I) then" "            if I > 0 and then not B (Candidate + I) then" "a run with an unset first frame"
mutant "   procedure Mark_Clean (M : in out Region_Map; F : Frame_Offset) with
     Pre  => Valid (M) and then M.Free (F)," "   procedure Mark_Clean (M : in out Region_Map; F : Frame_Offset) with
     Pre  => Valid (M)," "a used frame marked clean"
[ "$survivors" = 0 ] && echo "pool-region-maps: mutation OK" || { echo "pool-region-maps: $survivors mutants survived" >&2; exit 1; }
