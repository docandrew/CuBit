#!/usr/bin/env bash
#  Mutation check of the KERN-014 ownership model: each mutant weakens one
#  precondition the invariant needs; the proof must fail for every one.
#  Run in the Nix shell under the same memory cap as run.sh --prove.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
spec=$here/kernel_map_ownership.ads
body=$here/kernel_map_ownership.adb
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
    if (cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/kernel_map_ownership.gpr" \
          -u kernel_map_ownership.adb --level=2 -j2 --checks-as-errors=on --report=fail \
          >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
# The unmutated model must prove, or every "kill" below means nothing.
mutant "package Kernel_Map_Ownership with" "package Kernel_Map_Ownership with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "kernel-map-ownership: baseline does not prove" >&2; exit 1; }
survivors=0
mutant "             and then Clean (F)
             and then not In_Window (F)," "             and then Clean (F)," "Alloc_User of a windowed frame"
mutant "                            and then Contents (F) /= Process_Data))" "))" "Map_Back with process data"
mutant "     Pre  => Isolated and then Laundered and then Pins (F) and then not In_Window (F)," "     Pre  => Isolated and then Laundered and then Pins (F)," "Unpin while windowed"
mutant "             and then (Owners (F) = Free or else Pins (F))," "" "Window_Map of an unpinned owned frame"
mutant "                         (if Region_Of (F) = R then Owners (F) = Free))," "True," "Begin_Unmap with allocated frames"
mutant "             and then not Pins (F)
             and then Viewers (F) = 0," "             and then not Pins (F)," "Free_User of a viewed frame"
mutant "     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner
             and then Viewers (F) < Max_Views," "     Pre  => Isolated and then Laundered and then Owners (F) /= Kernel
             and then Viewers (F) < Max_Views," "View_Map of a frame no process owns"
mutant "             and then not Pins (F)
             and then Viewers (F) > 0," "             and then not Pins (F)," "Free_Viewed of an unviewed frame"
mutant "      Owners (F) := Kind;
      Clean (F) := False;" "      Owners (F) := Kind;" "an allocated frame stays clean"
mutant "      Residue (F) := Rounds;
      Clean (F) := False;" "      Residue (F) := Rounds;" "a windowed frame stays clean"
mutant "             and then Clean (F)
             and then not In_Window (F)," "             and then not In_Window (F)," "a process gets an unlaundered frame"
mutant "             and then Contents (F) = Zero and then Rounds > Residue (F)," "             and then Contents (F) = Zero," "marked clean before a round"
[ "$survivors" = 0 ] && echo "kernel-map-ownership: mutation OK" || { echo "kernel-map-ownership: $survivors mutants survived" >&2; exit 1; }
