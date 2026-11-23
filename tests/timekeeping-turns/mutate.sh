#!/usr/bin/env bash
#  Mutation check of Timekeeping_Turns: each mutant breaks a promise (msTicks
#  backward, a keeper without one clock, an early silence report); the
#  level 2 proof must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/timekeeping_turns.ads
backup=$(mktemp)
cp "$spec" "$backup"
trap 'cp "$backup" "$spec"; rm -f "$backup"' EXIT
survivors=0
mutant() {   # old new name
    cp "$backup" "$spec"
    python3 - "$spec" "$1" "$2" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
p, old, new = sys.argv[1:4]
s = open(p).read()
if old not in s: sys.exit(1)
open(p, "w").write(s.replace(old, new, 1))
PY
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/timekeeping_turns_tests.gpr" \
          -u timekeeping_turns.ads --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package Timekeeping_Turns with" "package Timekeeping_Turns with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "timekeeping-turns: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "(if Valid and then Published > Previous then Published else Previous)" \
       "(if Valid then Published else Previous)" "a late keeper's reading moves msTicks back"
mutant "(if Valid and then Published > Previous then Published else Previous)" \
       "(if Published > Previous then Published else Previous)" "an invalid reading is taken"
mutant "(CPU = Boot_CPU or Shared_Clock)" "(CPU = Boot_CPU or not Shared_Clock)" \
       "another CPU keeps time without one clock"
mutant "else Previous + Elapsed)" "else Previous + Elapsed / 2)" "CPU 0's elapsed ticks lost"
mutant "(Silence (Last_Run, Now) >= Silence_Report_Ms)" \
       "(Silence (Last_Run, Now) >= Silence_Report_Ms - 1)" "silence reported a millisecond early"
mutant "(if Now > Last_Run then Now - Last_Run else 0)" \
       "(if Now >= Last_Run then Now - Last_Run + 1 else 0)" "silence counted one too long"
[ "$survivors" = 0 ] && echo "timekeeping-turns: mutation OK" || { echo "timekeeping-turns: $survivors mutants survived" >&2; exit 1; }
