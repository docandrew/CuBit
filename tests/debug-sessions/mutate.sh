#!/usr/bin/env bash
#  Mutation check of Debug_Admission and Debug_View_Walk (DBG-001): each
#  mutant admits what it must not; the level 2 proof must fail for every
#  one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../kernel/src
survivors=0
mutant() {   # file old new name
    local file=$src/$1
    local backup
    backup=$(mktemp)
    cp "$file" "$backup"
    python3 - "$file" "$2" "$3" <<'PY' || { echo "mutate: pattern missing: $4"; cp "$backup" "$file"; rm -f "$backup"; exit 2; }
import sys
p, old, new = sys.argv[1:4]
s = open(p).read()
if old not in s: sys.exit(1)
open(p, "w").write(s.replace(old, new, 1))
PY
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/debug_sessions_tests.gpr" \
          -u debug_admission.adb -u debug_view_walk.adb -u walk_proof.adb --level=2 -j2 \
          --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $4"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $4"
    fi
    cp "$backup" "$file"; rm -f "$backup"
}
# The unmutated units must prove, or every "kill" below means nothing.
mutant debug_admission.ads "package Debug_Admission with" "package Debug_Admission with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "debug-sessions: baseline does not prove" >&2; exit 1; }
survivors=0
mutant debug_admission.ads "elsif F.Kind /= Observe and then To_Operation (Label) /= Describe then Not_Yet" \
       "elsif False then Not_Yet" "a Pause, Modify or Instrument session reaches the target"
mutant debug_admission.ads "elsif On_Target (To_Operation (Label)) and then not F.Target_Current then Target_Gone" \
       "elsif False then Target_Gone" "an exited target is still operated on"
mutant debug_admission.ads "(if not (F.Live and then F.Same_Life and then F.Same_Kind) then Revoked" \
       "(if not (F.Live and then F.Same_Kind) then Revoked" "a revoked generation still works"
mutant debug_admission.ads "elsif Length /= Request_Words (To_Operation (Label)) then Malformed" \
       "elsif False then Malformed" "a malformed request runs"
mutant debug_admission.ads "and then View - Slice_Base (S) <= Slice_Bytes - Pages * Page_Bytes)" \
       "and then View - Slice_Base (S) <= Slice_Bytes)" "a view batch runs past its slice"
# (Dropping "View >= Slice_Base (S)" is an equivalent mutant: the modular
# View - Slice_Base (S) of a lower View wraps past Slice_Bytes and is refused.)
mutant debug_admission.ads "      and then Pages * Page_Bytes <= User_Limit - Target)" \
       "      and then Pages <= User_Limit - Target)" "a target range past the user half"
mutant debug_view_walk.adb "               if not Write_Back (Word, Large) then" \
       "               if False then" "an uncached or write-combining page is viewed"
mutant debug_view_walk.adb "         if (Word and Required) /= Required then" \
       "         if (Word and Present_Bit) /= Present_Bit then" "a supervisor-only page is viewed"
mutant debug_view_walk.adb "      if Address >= User_Limit or else not RAM_Page (Root, Physical_Last) then" \
       "      if not RAM_Page (Root, Physical_Last) then" "a kernel-half address is walked"
[ "$survivors" = 0 ] && echo "debug-sessions: mutation OK" || { echo "debug-sessions: $survivors mutants survived" >&2; exit 1; }
