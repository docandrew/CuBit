#!/usr/bin/env bash
#  Mutation check of TSC_Sync_Rules: each mutant breaks a promise (a CPU's
#  TSC_ADJUST not set to the boot CPU's, a warp missed or counted wrong, a
#  check with too few samples or a warp taken as agreement, compensation in
#  the wrong direction or for warps both ways, a lost clock shared again);
#  the level 2 proof must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/tsc_sync_rules.ads
body=$here/../../kernel/src/tsc_sync_rules.adb
backup=$(mktemp)
body_backup=$(mktemp)
cp "$spec" "$backup"
cp "$body" "$body_backup"
trap 'cp "$backup" "$spec"; cp "$body_backup" "$body"; rm -f "$backup" "$body_backup"' EXIT
survivors=0
mutant() {   # old new name [file]
    cp "$backup" "$spec"
    cp "$body_backup" "$body"
    python3 - "${4:-$spec}" "$1" "$2" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
p, old, new = sys.argv[1:4]
s = open(p).read()
if old not in s: sys.exit(1)
open(p, "w").write(s.replace(old, new, 1))
PY
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/tsc_sync_tests.gpr" \
          -u tsc_sync_rules.ads --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package TSC_Sync_Rules with" "package TSC_Sync_Rules with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "tsc-sync: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "Adjust_Value (Low) + Adjust_Value'First;" "Adjust_Value (Low) + Adjust_Value'First + 1;" \
       "a negative TSC_ADJUST read one off" "$body"
mutant "((Write => Current /= Boot_Value, Value => Boot_Value))" \
       "((Write => Current /= Boot_Value, Value => Current))" "the boot CPU keeps a firmware offset"
mutant "((Write => Current /= Reference, Value => Reference))" \
       "((Write => Current > Reference, Value => Reference))" \
       "a secondary CPU below the boot CPU's offset left alone"
mutant "(if Previous > Now then Previous - Now else 0)" \
       "(if Previous > Now + 1 then Previous - Now else 0)" "a one-tick warp missed"
mutant "Warps    => (if Previous > Now then O.Warps + 1 else O.Warps)," \
       "Warps    => O.Warps," "warps not counted"
mutant "Max_Warp => Ticks'Max (O.Max_Warp, Warp (Previous, Now))))" \
       "Max_Warp => Warp (Previous, Now)))" "the largest warp forgotten"
mutant "or else Secondary.Rounds < Minimum_Rounds" "or else Secondary.Rounds < 1" \
       "a check with too few secondary samples judged"
mutant "elsif Boot.Warps = 0 and then Secondary.Warps = 0 then Agree" \
       "elsif Boot.Warps = 0 then Agree" "a secondary CPU's warps taken as agreement"
mutant "((Secondary.Warps > 0) /= (Boot.Warps > 0)" "((Secondary.Warps > 0 or else Boot.Warps > 0)" \
       "warps both ways compensated as one offset"
mutant "(if Secondary.Warps > 0 then Plus (Current, Secondary.Max_Warp)" \
       "(if Secondary.Warps > 0 then Minus (Current, Secondary.Max_Warp)" \
       "a secondary CPU behind moved further back"
mutant "elsif Check = Maximum_Checks then Done_Disagree" "elsif Check = Maximum_Checks then Check_Again" \
       "checks without end"
mutant "elsif Result = Incomplete then Check_Again" "elsif Result = Incomplete then Done_Agree" \
       "an incomplete check taken as agreement"
mutant "(Shared_So_Far and then Final = Done_Agree)
   with" "(Final = Done_Agree)
   with" "a lost clock shared again"
mutant "(Now - Start >= Limit)
   with" "(Now - Start > Limit or else Now < Start)
   with" \
       "a wait across a counter wrap ends early"
[ "$survivors" = 0 ] && echo "tsc-sync: mutation OK" || { echo "tsc-sync: $survivors mutants survived" >&2; exit 1; }
