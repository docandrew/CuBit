#!/usr/bin/env bash
#  Mutation check of the launch handle's decisions (CAP-012, DBG-001):
#  each mutant admits what it must not; the level 2 proof must fail for
#  every one. Nix shell, memory cap as run.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$here/../..
procmgr=userspace/services/procmgr
runtime=userspace/runtime/gnat
survivors=0
pristine=$(mktemp)
cp "$root/$procmgr/handle_admission.ads" "$pristine"
trap 'cp "$pristine" "$root/$procmgr/handle_admission.ads"; rm -f "$pristine"' EXIT
mutant() {   # file old new name
    local file=$root/$1
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
    if (cd "$root/kernel" && alr exec -- gnatprove -P "$here/launch_handles_tests.gpr" \
          -u handle_admission.adb -u cubit-launch_handles.ads --level=2 -j2 \
          --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $4"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $4"
    fi
    cp "$backup" "$file"; rm -f "$backup"
}
# The unmutated units must prove, or every "kill" below means nothing.
mutant $procmgr/handle_admission.ads "package Handle_Admission with" "package Handle_Admission with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "launch-handles: baseline does not prove" >&2; exit 1; }
survivors=0
mutant $procmgr/handle_admission.ads \
  "      elsif F.State = Revoked then LH.Revoked" \
  "      elsif False then LH.Revoked" "a revoked handle still works"
mutant $procmgr/handle_admission.ads \
  "      elsif F.State = Child_Gone and then Op not in LH.Observe_Op | LH.Revoke_Op then LH.Child_Ended" \
  "      elsif False then LH.Child_Ended" "a handle outlives its child"
mutant $procmgr/handle_admission.ads \
  "      elsif not Carries (F.Rights, Op) then LH.Right_Missing" \
  "      elsif False then LH.Right_Missing" "an operation runs without its right"
mutant $procmgr/handle_admission.ads \
  "      elsif Length /= LH.Request_Words (Op) then LH.Malformed" \
  "      elsif False then LH.Malformed" "a malformed request runs"
mutant $runtime/cubit-launch_handles.ads \
  "        when Debug_Op     => (Needs => True, Of_Right => Debug)," \
  "        when Debug_Op     => (Needs => True, Of_Right => Observe)," "Observe is enough to debug"
mutant $runtime/cubit-launch_handles.ads \
  "        when Control_Op   => (Needs => True, Of_Right => Stop)," \
  "        when Control_Op   => (Needs => False)," "anyone holding a handle stops the child"
mutant $procmgr/handle_admission.ads \
  "        (if LH.Is_Operation_Label (Label) then Handle_Request else Refused)" \
  "        Handle_Request" "a launch handle reaches procmgr's other operations"
mutant $procmgr/handle_admission.ads \
  "      elsif LH.Is_Operation_Label (Label) then Refused" \
  "      elsif False then Refused" "a handle operation without a handle's tag"
mutant $procmgr/handle_admission.ads \
  "        (if Label = LH.Session_Debug_Operation then Session_Debug_Request else Refused)" \
  "        (if Label = LH.Session_Debug_Operation then Session_Debug_Request else Ordinary)" \
  "the session's debug badge reaches procmgr's other operations"
mutant $procmgr/handle_admission.ads \
  "        when Session_Launch.Remote_Badge  => Remote_Rights," \
  "        when Session_Launch.Remote_Badge  => LH.All_Rights," "a remote launch may debug"
mutant $procmgr/handle_admission.ads \
  "     [for R in LH.Right => Held (R) and then Asked (R)]" \
  "     [for R in LH.Right => Asked (R)]" "narrowing amplifies"
# Dropping Debug_Decision's Never_Debuggable check alone is an equivalent
# mutant: Allowed refuses those classes too (No_Kinds), so Done never
# follows. With both checks dropped the proof must fail.
python3 - "$root/$procmgr/handle_admission.ads" <<'PY'
import sys
p = sys.argv[1]; s = open(p).read()
s = s.replace("     (if Never_Debuggable (C) then No_Kinds\n      elsif Path", "     (if False then No_Kinds\n      elsif Path", 1)
open(p + ".allowed", "w").write(s)
PY
cp "$root/$procmgr/handle_admission.ads" "$here/build/handle_admission.ads.orig"
cp "$root/$procmgr/handle_admission.ads.allowed" "$root/$procmgr/handle_admission.ads"
rm -f "$root/$procmgr/handle_admission.ads.allowed"
mutant $procmgr/handle_admission.ads \
  "      elsif Never_Debuggable (F.Class) then LH.Never_Debuggable" \
  "      elsif False then LH.Never_Debuggable" "secrets.svc or the trusted path minted"
cp "$here/build/handle_admission.ads.orig" "$root/$procmgr/handle_admission.ads"
mutant $procmgr/handle_admission.ads \
  "      elsif not F.Held_Control then LH.Not_Held" \
  "      elsif False then LH.Not_Held" "minting without the target's Control"
mutant $procmgr/handle_admission.ads \
  "        (if C = Session_Member then E.Own_Launches else No_Kinds)" \
  "        E.Own_Launches" "a launch handle debugs a process outside its session"
mutant $procmgr/handle_admission.ads \
  "      System_Services => [Debug_ABI.Observe => True, others => False]);" \
  "      System_Services => [others => True]);" "the user session pauses or modifies a system service"
mutant $runtime/cubit-launch_handles.ads \
  "((Tag and not Serial_Mask) = Handle_Tag_Prefix and then (Tag and Serial_Mask) /= 0)" \
  "((Tag and not Serial_Mask) = Handle_Tag_Prefix)" "the bare prefix names an issuance"
mutant $procmgr/handle_admission.ads \
  "     (if Gate /= Development then Gate_Refusal (Gate)" \
  "     (if False then Gate_Refusal (Gate)" "a Disabled or Production image mints debug sessions"
mutant $procmgr/handle_admission.ads \
  "     (Gate = Development)
   with Post => Debug_Authority_Granted'Result" \
  "     (Gate /= Disabled)
   with Post => Debug_Authority_Granted'Result" "a Production image grants the session's debug authority"
mutant $procmgr/handle_admission.ads \
  "     (if Gate = Production then LH.Approval_Needed else LH.Debugging_Disabled);" \
  "     (LH.Approval_Needed);" "a Disabled image says approval would do"
[ "$survivors" = 0 ] && echo "launch-handles: mutation OK" || { echo "launch-handles: $survivors mutants survived" >&2; exit 1; }
