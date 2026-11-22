#!/usr/bin/env bash
#  Mutation check of Free_Link_Codes: each mutant breaks the encoding; the
#  level 2 proof must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/free_link_codes.ads
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
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/free_link_codes_tests.gpr" \
          -u free_link_codes.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package Free_Link_Codes with" "package Free_Link_Codes with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "free-link-codes: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "First_List_Code : constant Code := Code'Last - (List_Count - 1);" \
       "First_List_Code : constant Code := Code'Last - (List_Count - 2);" "list codes overflow"
mutant "subtype Frame_Number is Unsigned_64 range 0 .. Unsigned_64 (First_List_Code) - 1;" \
       "subtype Frame_Number is Unsigned_64 range 0 .. Unsigned_64 (First_List_Code);" "frame codes reach the list codes"
mutant "(First_List_Code + Code (List))" "(First_List_Code + Code (List / 2))" "two lists share a code"
mutant "function Is_List (Item : Code) return Boolean is (Item >= First_List_Code);" \
       "function Is_List (Item : Code) return Boolean is (Item > First_List_Code);" "a list code taken for a frame"
[ "$survivors" = 0 ] && echo "free-link-codes: mutation OK" || { echo "free-link-codes: $survivors mutants survived" >&2; exit 1; }
