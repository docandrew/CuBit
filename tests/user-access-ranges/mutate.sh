#!/usr/bin/env bash
#  Mutation check of User_Access_Ranges: each mutant admits a bad range; the
#  level 2 proof must fail for every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/user_access_ranges.ads
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
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/user_access_ranges_tests.gpr" \
          -u user_access_ranges.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package User_Access_Ranges with" "package User_Access_Ranges with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "user-access-ranges: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "(Address < User_Limit and then Bytes <= User_Limit - Address)" \
       "(Address < User_Limit)" "length past the user limit"
mutant "(Address < User_Limit and then Bytes <= User_Limit - Address)" \
       "(Address < User_Limit and then Bytes <= User_Limit - Address + 1)" "one byte past the user limit"
mutant "or else Address >= Aperture_End))" "or else Address + 1 >= Aperture_End))" "starts inside the aperture"
mutant "(Bytes = 0 or else Address + Bytes <= Aperture_First" "(Bytes = 0 or else Address + Bytes <= Aperture_First + 1" "ends inside the aperture"
[ "$survivors" = 0 ] && echo "user-access-ranges: mutation OK" || { echo "user-access-ranges: $survivors mutants survived" >&2; exit 1; }
