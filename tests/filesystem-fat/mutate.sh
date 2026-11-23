#!/usr/bin/env bash
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Mutation check for FAT32 (FS-030). Each mutant is a plausible bug in a
# private copy of the filesystem sources. Kinds:
#   prove   SPARK core: must fail the level-1 proof (fat_core.gpr).
#   core    SPARK core: must fail the hosted core tests (build/core/main),
#           for what the contracts do not state (checksums, case, dates).
#   driver  Fat32: must fail run.py (SECTION: the one run.py section that
#           should kill it, to keep the run short).
# A surviving mutant marks a weak specification or test. Run inside nix
# develop, after building fat_core.gpr and fat_driver.gpr once:
#   bash tests/filesystem-fat/mutate.sh        (ONLY=text selects mutants)
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../userspace/services/filesystem
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # kind name file old new [section]
    case "$2" in *"${ONLY:-}"*) ;; *) return ;; esac
    total=$((total + 1))
    rm -rf "$work/src" && cp -r "$src" "$work/src"
    python3 - "$work/src/$3" "$4" "$5" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old, s.count(old))
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $2"; return; }
    local out=$work/out obj=$work/obj-$total
    case "$1" in
        prove)
            if (cd "$here" && gnatprove -P fat_core.gpr -XFS_SRC="$work/src" -XFS_OBJ="$obj" -j2 \
                  --level=1 --checks-as-errors=on -u "${3%.ad?}.adb" >"$out" 2>&1); then
                echo "SURVIVED $2"
            elif grep -qE "(medium|high): " "$out"; then
                echo "killed   $2: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
                killed=$((killed + 1))
            else
                echo "ERROR    $2 (gnatprove failed without an unproved check):"; tail -5 "$out"
            fi ;;
        core)
            if ! (cd "$here" && gprbuild -q -p -P fat_core.gpr -XFS_SRC="$work/src" -XFS_OBJ="$obj" >"$out" 2>&1); then
                echo "ERROR    $2 (did not compile):"; tail -5 "$out"
            elif "$obj/main" >"$out" 2>&1; then
                echo "SURVIVED $2"
            else
                echo "killed   $2: $(grep -m1 'FAIL' "$out")"
                killed=$((killed + 1))
            fi ;;
        driver)
            if ! (cd "$here" && gprbuild -q -p -P fat_driver.gpr -XFS_SRC="$work/src" -XFS_OBJ="$obj" >"$out" 2>&1); then
                echo "ERROR    $2 (did not compile):"; tail -5 "$out"
            elif FAT_TOOL="$obj/fat_tool" python3 "$here/run.py" --quick --only "$6" >"$out" 2>&1; then
                echo "SURVIVED $2"
            else
                echo "killed   $2: $(grep -m1 'FAIL' "$out" || tail -1 "$out")"
                killed=$((killed + 1))
            fi ;;
    esac
    rm -rf "$obj"
}

# The proved core.
mutant prove walk-step-bound fat_chains.adb \
  "if Item.Next = W.Saved or else W.Steps = W.Last - First_Data_Cluster then" \
  "if Item.Next = W.Saved then"
mutant prove brent-power fat_chains.adb "W.Power := W.Power * 2;" "W.Power := W.Power + 1;"
mutant prove ledger-take fat_chains.adb "      L.Free := L.Free - 1;
" ""
mutant prove boot-fat-too-small fat_format.adb \
  "elsif Byte_Count (Fat_Size) * Byte_Count (Sector_Size / Fat_Entry_Bytes) < Count + 2 then" \
  "elsif False then"
mutant prove boot-root-range fat_format.adb \
  "or else Byte_Count (Root_Raw) > Count + 1" "or else Byte_Count (Root_Raw) > Count + 2"
mutant prove fat-offset fat_format.adb \
  "(Natural (C) mod Entries_Per_Sector (G) * Fat_Entry_Bytes)" \
  "(Natural (C) mod (Entries_Per_Sector (G) + 1) * Fat_Entry_Bytes)"
mutant prove cluster-sector fat_format.adb \
  "return Sector_Number (Byte_Count (G.Data_Start) + Index * Per_Cluster);" \
  "return Sector_Number (Byte_Count (G.Data_Start) + (Index + 1) * Per_Cluster);"
mutant prove gather-base fat_format.adb "Base := (Part.Ordinal - 1) * Units_Per_Part;" \
  "Base := Part.Ordinal * Units_Per_Part;"
mutant prove name-bytes fat_names.adb "if Count > Maximum_Name_Bytes - Bytes then" \
  "if Count > Maximum_Name_Bytes - 1 then"
mutant prove entry-next fat_format.adb "elsif Value >= First_Data_Cluster and then Value <= Last then" \
  "elsif Value >= First_Data_Cluster and then Value <= Last + 1 then"
# What the contracts leave to the hosted core tests.
mutant core checksum-rotate fat_format.adb "Sum := Rotate_Right (Sum, 1) + Name (I);" \
  "Sum := Rotate_Left (Sum, 1) + Name (I);"
mutant core fold-cyrillic fat_names.adb "Cyrillic_Extra_Distance : constant := 16#50#;" \
  "Cyrillic_Extra_Distance : constant := 16#40#;"
mutant core leap-2100 fat_format.adb "(Offset mod 4 = 0 and then Offset /= 2100 - Epoch_Year);" \
  "(Offset mod 4 = 0);"
mutant core device-aux fat_names.adb "           or else (Has (1, 'A') and then Has (2, 'U') and then Has (3, 'X'))
" ""
mutant core exact-lower fat_names.adb "Unit in Unit_Upper_A .. Unit_Upper_Z or else Unit in Unit_Zero .. Unit_Nine" \
  "Unit in Unit_Upper_A .. Unit_Upper_Z or else Unit in Unit_Zero .. Unit_Lower_Z"
mutant core tail-digits fat_names.adb "Name (I) := Byte_Zero + Unsigned_8 (Rest mod 10);" \
  "Name (I) := Byte_Zero + Unsigned_8 (Rest mod 9);"
# The driver: order, allocation, names, policy.
mutant driver barrier-before-size fat32.adb "            --  The data, then the size (and a new first cluster).
            Barrier (V, OK);" "            OK := True;" cuts
mutant driver barrier-before-free fat32.adb "      Barrier (V, OK);
      if not OK then
         S := Device_Error;
         return;
      end if;
      Free_Chain (V, First, S);" "      Free_Chain (V, First, S);" cuts
mutant driver one-fat-copy fat32.adb \
  "Copy_Count : constant Positive := (if V.G.Mirrored then V.G.Fats else 1);" \
  "Copy_Count : constant Positive := 1;" geometries
# (Not a mutant: Choose skipping the clusters it already chose is defence
# in depth; with an exact ledger the search reaches every other free
# cluster before it wraps to them, so dropping the skip is equivalent.)
mutant driver no-dirty-mark fat32.adb "         Set_Clean (V, False, OK);" "         OK := True;" policy
mutant driver ignore-dirty fat32.adb \
  "Candidate.Writable := Volume_Clean (Flags) and then not Boot_Dirty (Boot)" \
  "Candidate.Writable := True" policy
mutant driver lfn-checksum fat32.adb "if Complete_For (Gathered, Info.Short.Name) then" \
  "if Gathered.Active then" adversarial
mutant driver read-unchecked fat32.adb "      if Checked_First /= First or else Checked_Key /= V.Device.volumeKey or else First = No_Cluster then" \
  "      if False then" adversarial
mutant driver same-tail fat32.adb "               if not Taken (K) then" "               if True then" names
mutant driver run-contiguous fat32.adb "               exit when Next_C /= C + 1;" \
  "               exit when Next_C = C;" geometries
mutant driver no-gap-zeroes fat32.adb "         if Offset > Old_Size then
            declare
               Gap" "         if False then
            declare
               Gap" geometries
mutant driver short-chain-writes fat32.adb \
  "      if Unsigned_64 (O.Length) * Unsigned_64 (Cluster_Bytes (V.G)) < Unsigned_64 (O.Size) then" \
  "      if False then" adversarial
mutant driver delete-long-first fat32.adb "      Mark (Short_Sector);
      for I in reverse First_Index .. Short_Index loop" "      for I in First_Index .. Short_Index loop" cuts
mutant driver dotdot-root fat32.adb \
  "Dot_Item.First := (if Parent = Root_Number then No_Cluster else Parent_First);" \
  "Dot_Item.First := Parent_First;" geometries
mutant driver keep-repoint fat32.adb \
  "         if S = Done and then Source_Info.Directory and then Old_Parent /= New_Parent then
            Repoint_Parent (V, Source_Info.Short.First, New_Parent, New_First_Cluster, S);
         end if;
         if S = Done then
            Barrier (V, OK);
            S := (if OK then Done else Device_Error);
         end if;
         if S /= Done then
            return;
         end if;
         --  The old slots" \
  "         if S = Done then
            Barrier (V, OK);
            S := (if OK then Done else Device_Error);
         end if;
         if S /= Done then
            return;
         end if;
         --  The old slots" geometries
echo "MUTANTS: killed $killed of $total"
[ "$killed" -eq "$total" ]
