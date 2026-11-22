#!/usr/bin/env bash
#  Mutation check of the VMM-001 ownership model: each mutant weakens one
#  rule the design depends on; the proof must fail for every one.
#  Run in the Nix shell under the same memory cap as run.sh --prove.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/vm_ownership.ads
body=$here/vm_ownership.adb
spec_backup=$(mktemp); body_backup=$(mktemp)
cp "$spec" "$spec_backup"; cp "$body" "$body_backup"
trap 'cp "$spec_backup" "$spec"; cp "$body_backup" "$body"; rm -f "$spec_backup" "$body_backup"' EXIT
survivors=0
mutant() {   # old new name: applied to whichever of spec and body holds old
    cp "$spec_backup" "$spec"; cp "$body_backup" "$body"
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
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/vm_ownership.gpr" \
          -u vm_ownership.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated model must prove, or every "kill" below means nothing.
mutant "package VM_Ownership with" "package VM_Ownership with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "vm-ownership: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "             and then ((Owners (F) = Guest and then Guest_Of (F) = V)" \
       "             and then ((Owners (F) = Guest)" "Map of another VM's frame"
mutant "             and then Use_VM (F) = No_VM
             and then ((Owners" "             and then ((Owners" "Map of a frame another entry names"
mutant "             and then Stamp (V) (G) < Round (V)," \
       "             and then Stamp (V) (G) <= Round (V)," "Retire before a round completed"
mutant "             and then Table_Stamp (F) < Round (Table_Of (F))," \
       "             and then Table_Stamp (F) <= Round (Table_Of (F))," "Free_Table before a round (XSA-480)"
mutant "(if Was_On (C) then Clear_VM" "(if Running (C) then Clear_VM" "a round that flushes only running CPUs"
mutant "             and then Active_On (X) = No_CPU and then Current (C) = No_VCPU," \
       "             and then Current (C) = No_VCPU," "Load of a vCPU active on another CPU"
mutant "             and then not Running (Active_On (X))," "" "VMCLEAR while the guest runs"
mutant "     Pre  => Invariant and then Monitor (V) = P
" "     Pre  => Invariant
" "a view mapped by a process that is not the monitor"
mutant "             and then (for all C in CPU_Index => not Ran_On (V) (C))," "" \
       "Finish_Destroy with CPUs still holding translations"
mutant "                         Guest_Of (F) /= V and then Device_Of (F) /= V
                         and then Table_Of (F) /= V)" "                         Device_Of (F) /= V
                         and then Table_Of (F) /= V)" "Finish_Destroy with guest frames left"
mutant "             and then Owners (F) = Free and then not Kernel_Mapped (F),
     Post => Invariant and then Owners (F) = Guest" "             and then Owners (F) = Free,
     Post => Invariant and then Owners (F) = Guest" "a guest frame from the kernel's mapped pool"
mutant "             and then Owners (Root) = Free and then Kernel_Mapped (Root)," \
       "             and then Owners (Root) = Free," "an EPT root outside the kernel's pool"
mutant "Pre  => Invariant and then Kind (V) (G) = Mapped and then not Dom (V) (G)," \
       "Pre  => Invariant and then Kind (V) (G) = Mapped," "Unmap while an IOMMU domain mirrors the entry"
mutant "             and then Kind (VM_Of (Current (C))) (G) = Mapped," \
       "             and then Kind (VM_Of (Current (C))) (G) /= Absent," "caching an unmapped entry"
mutant "             and then Table_Of (F) = VM_Of (Current (C))
             and then Table_Linked (F)," "             and then Table_Of (F) = VM_Of (Current (C)),"  "walking an unlinked table"
mutant "             and then Use_VM (F) = No_VM and then View_By (F) = No_Process,
     Post => Invariant and then Owners (F) = Free;" "             and then Use_VM (F) = No_VM,
     Post => Invariant and then Owners (F) = Free;" "freeing a guest frame a monitor still views"
mutant "             and then Phase (VM_Of (Current (C))) = Live
             and then not Tag_Stale (Current (C))," "             and then Phase (VM_Of (Current (C))) = Live," \
       "Enter with a stale guest tag"
mutant "      if Paging_Changed then
         Tag_Stale (X) := True;" "      Dom (VM_Of (X)) (0) := False;
      if Paging_Changed then
         Tag_Stale (X) := True;" "an architectural-state change that touches the EPT (CVE-2026-99312 class)"
mutant "      if Paging_Changed then
         Tag_Stale (X) := True;
      end if;" "      null;" "a paging change that does not flush the tag"
mutant "      if Last_On (X) /= C then
         Tag_Stale (X) := True;
      end if;" "      null;" "a vCPU loaded on a new CPU without a tag flush"
[ "$survivors" = 0 ] && echo "vm-ownership: mutation OK" || { echo "vm-ownership: $survivors mutants survived" >&2; exit 1; }
