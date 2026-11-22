#!/usr/bin/env bash
#  Mutation check of Guest_Speculation (a guest's SPEC_CTRL): each mutant
#  breaks one rule; the proof (or, for CPUID bit positions, the hosted test)
#  must fail for each. Run in the Nix shell under the memory cap (run.sh).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/../../kernel/src/guest_speculation.ads
body=$here/../../kernel/src/guest_speculation.adb
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
    if (cd "$here/../../kernel" && alr exec -- gprbuild -p -q -P "$here/vm_units_tests.gpr" >/dev/null 2>&1 \
          && "$here/build/main" >/dev/null 2>&1 \
          && alr exec -- gnatprove -P "$here/vm_units_tests.gpr" -u guest_speculation.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Guest_Speculation with" "package Guest_Speculation with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "vm-units: baseline fails" >&2; exit 1; }
survivors=0
check "if not Present (Mask) or else not Value_Allowed (Mask, Value) then" \
      "if not Present (Mask) then" "a guest write may set a bit the host lacks"
check "        elsif Writes_Passed then
            return Load_And_Save;" "        elsif Writes_Passed then
            return Load_Guest;" "passed-through writes are not read back at exit"
check "       or Flag (Offered (H, Control_BHI_DIS_S), Control_BHI_DIS_S));" ");" \
      "BHI_DIS_S dropped from the writable bits"
check "Flag (SVM and then Bit (H.Extended_2_EAX, Extended_2_AutoIBRS)," \
      "Flag (Bit (H.Extended_2_EAX, Extended_2_AutoIBRS)," "AutoIBRS offered on VMX"
check "    Extended_SSBD  : constant CPUID_Bit := 24;" "    Extended_SSBD  : constant CPUID_Bit := 25;" \
      "AMD SSBD read from VIRT_SSBD's bit"
[ "$survivors" = 0 ] && echo "vm-units: mutation OK" || { echo "vm-units: $survivors survived" >&2; exit 1; }
