#!/usr/bin/env bash
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Mutation check for FS-034 (USB stick hot-plug and Eject). Each mutant is
# a plausible bug in a private copy of the sources. Kinds:
#   prove  must fail the level-2 proof (usb_eject_tests.gpr);
#   test   must fail the hosted tests (build/main), for what the contracts
#          leave to fixtures (wire constants).
# A surviving mutant marks a weak specification or test. Run inside nix
# develop (gnatprove memory-capped by the caller):
#   bash tests/usb-eject/mutate.sh        (ONLY=text selects mutants)
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # kind name dir file old new [unit]
    case "$2" in *"${ONLY:-}"*) ;; *) return ;; esac
    total=$((total + 1))
    rm -rf "$work/runtime" "$work/xhci" "$work/devmgr"
    mkdir -p "$work/runtime" "$work/xhci" "$work/devmgr"
    cp "$root"/userspace/runtime/gnat/cubit.ads "$root"/userspace/runtime/gnat/cubit-block_unit_badges.ads \
       "$root"/userspace/runtime/gnat/cubit-disk_presence.ad? "$root"/userspace/runtime/gnat/cubit-volume_ejection.ad? \
       "$work/runtime/"
    cp "$root"/userspace/services/xhci/disk_reports.ad? "$work/xhci/"
    cp "$root"/userspace/services/devmgr/devmgr_usb_disks.ad? "$work/devmgr/"
    python3 - "$work/$3/$4" "$5" "$6" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old, s.count(old))
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $2"; return; }
    local out=$work/out obj=$work/obj-$total
    local vars=(-XUSB_EJECT_RUNTIME="$work/runtime" -XUSB_EJECT_XHCI="$work/xhci" -XUSB_EJECT_DEVMGR="$work/devmgr"
                -XUSB_EJECT_OBJ="$obj")
    local unit=${7:-${4%.ad?}.adb}
    case "$1" in
        prove)
            if (cd "$here" && gnatprove -P usb_eject_tests.gpr "${vars[@]}" -j2 --level=2 \
                  --checks-as-errors=on -u "$unit" >"$out" 2>&1); then
                echo "SURVIVED $2"
            elif grep -qE "(medium|high): " "$out"; then
                echo "killed   $2: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
                killed=$((killed + 1))
            else
                echo "ERROR    $2 (gnatprove failed without an unproved check):"; tail -5 "$out"
            fi ;;
        test)
            if ! (cd "$here" && gprbuild -q -p -P usb_eject_tests.gpr "${vars[@]}" >"$out" 2>&1); then
                echo "ERROR    $2 (did not compile):"; tail -5 "$out"
            elif "$obj/main" >"$out" 2>&1; then
                echo "SURVIVED $2"
            else
                echo "killed   $2: $(grep -m1 'FAIL' "$out")"
                killed=$((killed + 1))
            fi ;;
    esac
    rm -rf "$obj"
}

B=cubit-block_unit_badges.ads; P=cubit-disk_presence; E=cubit-volume_ejection; R=disk_reports
# Unit badges.
mutant prove badge-low-bits runtime $B "or Unsigned_64 (Item))" "or Unsigned_64 (Item / 2))" $B
mutant prove badge-any-prefix runtime $B "(if Prefix_Of (Tag) = Unit_Prefix then" "(if Prefix_Of (Tag) /= 0 then" $B
mutant prove badge-whole-device runtime $B "Unit_Prefix : constant Unsigned_64 := 16#424C_4B55#;" "Unit_Prefix : constant Unsigned_64 := 16#424C_4B57#;" $B
# Presence reports.
mutant prove presence-unit-zero runtime $P.adb "not in 1 .. Unsigned_64" "not in 0 .. Unsigned_64"
mutant prove presence-wp-gone runtime $P.adb "         Item := (Unit => Units.Unit (Item_Words (Unit_Word) and Units.Low_Half), State => Gone,
                  Write_Protected => False);" "         Item := (Unit => Units.Unit (Item_Words (Unit_Word) and Units.Low_Half), State => Gone,
                  Write_Protected => True);"
mutant prove presence-length runtime $P.adb "if Length /= Report_Words or else" "if"
mutant test presence-label runtime $P.ads "Report_Label : constant Unsigned_32 := 16#0245#;" "Report_Label : constant Unsigned_32 := 16#0244#;"
mutant test presence-wp-bit runtime $P.ads "Write_Protected_Bit : constant Unsigned_64 := 16#100#;" "Write_Protected_Bit : constant Unsigned_64 := 16#200#;"
# Ejection.
mutant prove eject-close-unasked runtime $E.ads "elsif Policy = Refuse_If_Open then Refuse_Open" "elsif Policy = Close_Open_Handles then Refuse_Open" $E.adb
mutant prove eject-fixed-disk runtime $E.ads "elsif not Removable then Refuse_Not_Removable" "elsif False then Refuse_Not_Removable" $E.adb
mutant prove eject-flush-failed runtime $E.ads "(if Flushed then (Result => Safe_To_Remove, Open => 0)" "(if True then (Result => Safe_To_Remove, Open => 0)" $E.adb
mutant prove eject-count runtime $E.adb "if (Result = Open_Handles) /= (Count /= 0) then" "if False then"
mutant prove eject-policy runtime $E.adb "      else
         Valid := False;" "      else
         Valid := True;"
mutant test eject-code runtime $E.ads "(Safe_To_Remove => 1, Open_Handles => 2," "(Safe_To_Remove => 0, Open_Handles => 2,"
# xhci's report state.
mutant prove reports-forget-gone xhci $R.adb "         Item (S).Pending_Gone := Item (S).Current;
      end if;" "         null;
      end if;" test_reports.ads
mutant prove reports-present-first xhci $R.adb "      if Item (S).Pending_Gone /= No_Unit then
         Report := (Unit => Item (S).Pending_Gone" "      if False then
         Report := (Unit => Item (S).Pending_Gone" test_reports.ads
mutant prove reports-gone-unannounced xhci $R.adb "      if Item (S).Announced then
         Item (S).Pending_Gone" "      if True then
         Item (S).Pending_Gone" test_reports.ads
mutant prove reports-sent-early xhci $R.adb "      elsif Item (S).Current /= No_Unit then
         Item (S).Announced := True;" "      else
         Item (S).Announced := True;" test_reports.ads
mutant prove reports-other-slot xhci $R.adb "      Item (S).Current := U;" "      Item (Slot'First).Current := U;" test_reports.ads

# devmgr's USB disk names.
D=devmgr_usb_disks
mutant prove names-reuse-detaching devmgr $D.adb "         if T (O).State = Free then
            T (O) := (State => Attach_Owed" "         if T (O).State in Free | Detach_Owed then
            T (O) := (State => Attach_Owed"
mutant prove names-forget-detach devmgr $D.adb "      if Maybe_Known (T (Name)) then
         T (Name).State := Detach_Owed;" "      if False then
         T (Name).State := Detach_Owed;"
mutant prove names-free-unacked devmgr $D.adb "      if Outcome = Answered_OK then
         T (Name) := (others => <>);" "      if Outcome /= Answered_Refused then
         T (Name) := (others => <>);"
mutant prove names-untold devmgr $D.adb "            T (Name).Told := True;" "            null;"
mutant prove names-duplicate devmgr $D.adb "      if Find (T, Driver, Unit) /= 0 then
         Result := Already_Known;" "      if False then
         Result := Already_Known;"
mutant test names-retry devmgr $D.ads "   Retry_Ms : constant := 250;" "   Retry_Ms : constant := 25;"

echo "usb-eject mutation check: $killed of $total killed"
[ "$killed" -eq "$total" ]
