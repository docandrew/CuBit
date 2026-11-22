#!/usr/bin/env bash
#  Mutation check of Virtualization_Features: each mutant breaks one rule;
#  the proof (or, for bit positions, the hosted test) must fail for each.
#  Run in the Nix shell under the memory cap (see run.sh).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/virtualization_features.ads
body=$here/../../kernel/src/virtualization_features.adb
sb=$(mktemp); bb=$(mktemp); cp "$spec" "$sb"; cp "$body" "$bb"
trap 'cp "$sb" "$spec"; cp "$bb" "$body"; rm -f "$sb" "$bb"' EXIT
survivors=0
check() {   # old new name
    cp "$sb" "$spec"; cp "$bb" "$body"
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
    if (cd "$here/../../kernel" && alr exec -- gprbuild -p -q -P "$here/virtualization_features_tests.gpr" >/dev/null 2>&1 \
          && "$here/build/main" >/dev/null 2>&1 \
          && alr exec -- gnatprove -P "$here/virtualization_features_tests.gpr" -u virtualization_features.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Virtualization_Features with" "package Virtualization_Features with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "virtualization-features: baseline fails" >&2; exit 1; }
survivors=0
check "       and then F.VPID and then F.EPT_Walk_4" "       and then F.EPT_Walk_4" "VPID dropped from the requirements"
check "        elsif not F.Unrestricted_Guest then
            return No_Unrestricted_Guest;
" "" "the verdict forgets unrestricted guest"
check "    Proc2_EPT                      : constant Control_Bit := 1;" \
      "    Proc2_EPT                      : constant Control_Bit := 2;" "EPT read from the wrong bit"
check "          (Secondary and then Offered (Raw.Proc2, N));" "          (Offered (Raw.Proc2, N));" \
      "secondary features without the secondary controls"
check "    SVM_Next_RIP_Save    : constant Bit_Number := 3;" "    SVM_Next_RIP_Save    : constant Bit_Number := 4;" \
      "next-RIP save read from the wrong bit"
check "SVM_On and then Raw.VM_CR_Read and then Bit (Raw.VM_CR, VM_CR_SVM_Disabled)," "False," \
      "VM_CR.SVMDIS ignored"
[ "$survivors" = 0 ] && echo "virtualization-features: mutation OK" || { echo "virtualization-features: $survivors survived" >&2; exit 1; }
