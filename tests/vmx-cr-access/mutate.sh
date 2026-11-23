#!/usr/bin/env bash
#  Mutation check of VMX_CR_Access: each mutant lets the guest reach a state
#  the processor would refuse; the level 2 proof must fail for every one.
#  Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
body=$here/../../kernel/src/vmx_cr_access.adb
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
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/vmx_cr_access_tests.gpr" \
          -u vmx_cr_access.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated unit must prove, or every "kill" below means nothing.
mutant "package body VMX_CR_Access with" "package body VMX_CR_Access with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "vmx-cr-access: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "if not Valid_CR0 (Wanted, CR4, EFER) then" "if False then" "CR0 #GP checks skipped"
mutant "or (CR0 and CR0_PE);" ";" "LMSW clears PE"
mutant "EFER => Long_Mode_Follows (Wanted, EFER)," "EFER => EFER," "LMA does not follow PG"
mutant "elsif (Source and CR4_VMXE) /= 0" "elsif ((Source and CR4_VMXE) /= 0 and then False)" "CR4.VMXE admitted"
mutant "or else (Source and not CR4_Allowed) /= 0" "or else ((Source and not CR4_Allowed) /= 0 and then False)" "CR4 bits outside FIXED1 admitted"
mutant "(Source and CR4_PAE) = 0)" "(Source and CR4_PAE) = 0 and then False)" "CR4.PAE cleared in long mode"
mutant "         when others =>
            return Unchanged;" "         when others =>
            return (Unchanged with delta Outcome => Applied);" "CR3/CR8 applied"
mutant "when Move_From_CR =>
                     return Unchanged;" "when Move_From_CR =>
                     return (Unchanged with delta Outcome => Applied);" "a read applied"
mutant "Wanted := Wanted and CR0_Defined;" "null;" "undefined CR0 bits kept"
mutant "(Unchanged with delta Outcome => General_Protection);" "(Unchanged with delta Outcome => General_Protection, Flush => True);" "a fault changes state"
mutant "return (Outcome => Applied, CR0 => CR0, CR4 => Source, EFER => EFER," "return (Outcome => Applied, CR0 => CR0 or CR0_PG, CR4 => Source, EFER => EFER," "a CR4 write changes CR0"
[ "$survivors" = 0 ] && echo "vmx-cr-access: mutation OK" || { echo "vmx-cr-access: $survivors mutants survived" >&2; exit 1; }
