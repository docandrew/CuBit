#!/usr/bin/env bash
#  Mutation check of Handoff_Contexts: each mutant breaks a promise (a
#  requester admitted twice or after the seal, a switch that leaves it
#  passing through or touches another, the seal undone); the level 2 proof
#  must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
body=$here/../../kernel/src/handoff_contexts.adb
backup=$(mktemp)
cp "$body" "$backup"
trap 'cp "$backup" "$body"; rm -f "$backup"' EXIT
survivors=0
mutant() {   # old new name
    cp "$backup" "$body"
    python3 - "$body" "$1" "$2" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
p, old, new = sys.argv[1:4]
s = open(p).read()
if old not in s: sys.exit(1)
open(p, "w").write(s.replace(old, new, 1))
PY
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/handoff_contexts_tests.gpr" \
          -u handoff_contexts.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package body Handoff_Contexts with" "package body Handoff_Contexts with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "handoff-contexts: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "Admitted := T.Phases (Who) = Untracked;" "Admitted := T.Phases (Who) /= Translated;" \
       "a passing-through requester admitted again"
mutant "         T.Phases (Who) := Passing_Through;" "         T.Phases (Who) := Translated;" \
       "an admission that does not pass through"
mutant "      T.Is_Sealed := True;" "      T.Is_Sealed := False;" "the seal not set"
mutant "Was_Passing := T.Phases (Who) = Passing_Through;" "Was_Passing := T.Phases (Who) /= Untracked;" \
       "a translated requester reported as switched"
mutant "         T.Phases (Who) := Translated;" "         T.Phases (Who) := Passing_Through;" \
       "the switch leaves it passing through"
mutant "      if Was_Passing then
         T.Phases (Who) := Translated;" "      if True then
         T.Phases (Who) := Translated;" "an untracked requester marked translated"
mutant "      Admitted := T.Phases (Who) = Untracked;
      if Admitted then
         T.Phases (Who) := Passing_Through;
      end if;" "      Admitted := T.Phases (Who) = Untracked;
      if Admitted then
         T.Phases (Who) := Passing_Through;
         T.Phases (Who xor 1) := Passing_Through;
      end if;" "an admission that opens a neighbour"
[ "$survivors" = 0 ] && echo "handoff-contexts: mutation OK" || { echo "handoff-contexts: $survivors mutants survived" >&2; exit 1; }
