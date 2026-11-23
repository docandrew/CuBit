#!/usr/bin/env bash
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Mutation check for USB mass-storage disks (SELF-003 step 4). Each mutant
# is a plausible bug in a private copy of the sources. Kinds:
#   prove  must fail the level-2 proof of its unit (usb_disk_tests.gpr);
#   test   must fail the hosted tests (build/main), for what the contracts
#          leave to fixtures (constants, the sense table, ISO signatures).
# A surviving mutant marks a weak specification or test. Run inside nix
# develop (gnatprove memory-capped by the caller):
#   bash tests/usb-disk/mutate.sh        (ONLY=text selects mutants)
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
killed=0; total=0
mutant() {   # kind name dir file old new
    case "$2" in *"${ONLY:-}"*) ;; *) return ;; esac
    total=$((total + 1))
    rm -rf "$work/xhci" "$work/scsi"
    mkdir -p "$work/xhci" "$work/scsi"
    cp "$root"/userspace/services/xhci/usb_bulk_only.ad? "$root"/userspace/services/xhci/usb_disk_probe.ad? "$work/xhci/"
    cp "$root"/userspace/lib/scsi/*.ad? "$work/scsi/"
    python3 - "$work/$3/$4" "$5" "$6" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old, s.count(old))
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $2"; return; }
    local out=$work/out obj=$work/obj-$total
    local vars=(-XUSB_DISK_XHCI="$work/xhci" -XUSB_DISK_SCSI="$work/scsi" -XUSB_DISK_OBJ="$obj")
    case "$1" in
        prove)
            if (cd "$here" && gnatprove -P usb_disk_tests.gpr "${vars[@]}" -j2 --level=2 \
                  --checks-as-errors=on -u "${4%.ad?}.adb" >"$out" 2>&1); then
                echo "SURVIVED $2"
            elif grep -qE "(medium|high): " "$out"; then
                echo "killed   $2: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
                killed=$((killed + 1))
            else
                echo "ERROR    $2 (gnatprove failed without an unproved check):"; tail -5 "$out"
            fi ;;
        test)
            if ! (cd "$here" && gprbuild -q -p -P usb_disk_tests.gpr "${vars[@]}" >"$out" 2>&1); then
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

# The BOT wrappers.
mutant prove cbw-direction xhci usb_bulk_only.adb \
  "(if Block.Direction = SCSI.From_Device then Data_In_Flag else 0)" \
  "(if Block.Direction /= SCSI.No_Data then Data_In_Flag else 0)"
mutant prove cbw-word-order xhci usb_bulk_only.adb \
  "Data (First + 1) := Unsigned_8 (Shift_Right (Value, 8) and Byte_Mask);" \
  "Data (First + 1) := Unsigned_8 (Shift_Right (Value, 16) and Byte_Mask);"
mutant prove cbw-length xhci usb_bulk_only.adb \
  "Put_Word (Result, CBW_Length_Field, Block.Expected);" \
  "Put_Word (Result, CBW_Length_Field, Block.Expected / 2);"
mutant prove cbw-cdb-off-by-one xhci usb_bulk_only.adb \
  "Result (CBW_CB_Field + Index - 1) := Block.Data (Index);" \
  "Result (CBW_CB_Field + Index - 1) := Block.Data (Index mod SCSI.Largest_CDB + 1);"
mutant prove csw-no-tag xhci usb_bulk_only.adb \
  "           or else Little_Endian (Data, Data'First + (CSW_Tag_Field - 1)) /= Expected_Tag
" ""
mutant prove csw-status-range xhci usb_bulk_only.adb \
  "           or else Status > Status_Phase_Error" "           or else Status > Status_Phase_Error + 1"
mutant prove csw-phase-is-good xhci usb_bulk_only.adb \
  "            return (Kind => Phase_Error, Residue => 0, Moved => 0);" \
  "            return (Kind => Good, Residue => 0, Moved => Moved);"
mutant prove csw-residue-unchecked xhci usb_bulk_only.adb \
  "elsif Residue > Expected_Bytes or else Moved > Expected_Bytes then" \
  "elsif Moved > Expected_Bytes then"
mutant prove csw-trusts-device-count xhci usb_bulk_only.adb \
  "                 Moved   => Moved);" "                 Moved   => Expected_Bytes - Residue);"
mutant prove max-lun-16 xhci usb_bulk_only.adb \
  "if Data'Length = 1 and then Data (Data'First) <= Logical_Unit'Last then" \
  "if Data'Length = 1 and then Data (Data'First) <= Logical_Unit'Last + 1 then"
mutant prove max-lun-error-is-one xhci usb_bulk_only.adb \
  "         when Control_Error =>
            null;" "         when Control_Error =>
            Valid := True;"
# The SCSI disk decoders.
mutant prove capacity-off-by-one scsi scsi_disks.adb \
  "Result := (Blocks => Unsigned_64 (Last) + 1, Size => Size_Of (Length));" \
  "Result := (Blocks => Unsigned_64 (Last), Size => Size_Of (Length));"
mutant prove capacity16-max scsi scsi_disks.adb \
  "         if Last = Unsigned_64'Last then
            Outcome := Malformed_Capacity;
         elsif" "         if"
mutant prove capacity16-length-field scsi scsi_disks.adb \
  "Block_Length_16  : constant := 8;" "Block_Length_16  : constant := 4;"
mutant prove mode-wp-bit scsi scsi.adb \
  "(Data (Data'First + Mode_Device_Specific) and Write_Protect_Bit) /= 0;" \
  "(Data (Data'First + Mode_Device_Specific) and 16#40#) /= 0;"
mutant prove mode-length-unchecked scsi scsi.adb \
  "        or else Data (Data'First) < Mode_Header_6_Bytes - 1
" ""
# The probe.
mutant prove probe-retry-forever xhci usb_disk_probe.adb \
  "         P.Attempts := P.Attempts + 1;
" ""
mutant prove probe-skip-iso xhci usb_disk_probe.adb \
  "      P.Current := (if ISO_Block (P.Geometry) < P.Geometry.Blocks
                    then Check_ISO else Ready);" "      P.Current := Ready;"
mutant prove probe-iso-ignored xhci usb_disk_probe.adb \
  "P.Current := (if ISO_Signature (Data) then ISO_Medium else Ready);" "P.Current := Ready;"
mutant prove probe-failed-capacity-kept xhci usb_disk_probe.adb \
  "               if not Good then
                  P.Current := Failed;
                  return;
               end if;
               if P.Current = Capacity_10 then" \
  "               if P.Current = Capacity_10 then"
mutant prove probe-wp-dropped xhci usb_disk_probe.adb \
  "P.Write_Protected := Valid and then Header.Write_Protected;" "P.Write_Protected := False;"
# Left to the fixtures.
mutant test sense-changed-retry scsi scsi_disks.ads \
  "           (if Sense.Code = ASC_Medium_Changed then Medium_Changed" \
  "           (if Sense.Code = ASC_Medium_Changed then Retry_Later"
mutant test sense-dataprotect scsi scsi_disks.ads \
  "         when Data_Protect => Write_Protected," "         when Data_Protect => Medium_Failure,"
mutant test sense-absent-asc scsi scsi_disks.ads \
  "ASC_Medium_Not_Present : constant Unsigned_8 := 16#3A#;" \
  "ASC_Medium_Not_Present : constant Unsigned_8 := 16#3B#;"
mutant test mode-sense-page scsi scsi.ads \
  "All_Pages : constant Unsigned_8 := 16#3F#;" "All_Pages : constant Unsigned_8 := 16#08#;"
mutant test mode-sense-opcode scsi scsi.ads "Mode_Sense_6         => 16#1A#," "Mode_Sense_6         => 16#1B#,"
mutant test iso-offset xhci usb_disk_probe.ads \
  "ISO_Descriptor_Offset : constant := 32_768;" "ISO_Descriptor_Offset : constant := 32_256;"
mutant test iso-version xhci usb_disk_probe.ads \
  "      and then Data (Data'First + 6) = 1);" "      and then Data (Data'First + 6) in 1 .. 2);"
mutant test iso-type-range xhci usb_disk_probe.ads \
  "and then Data (Data'First) in 0 .. 3 | 255" "and then Data (Data'First) in 0 .. 4 | 255"
mutant test ready-limit xhci usb_disk_probe.ads \
  "Ready_Attempt_Limit : constant := 20;" "Ready_Attempt_Limit : constant := 2;"
mutant test cbw-signature xhci usb_bulk_only.ads \
  "CBW_Signature : constant Unsigned_32 := 16#4342_5355#;" \
  "CBW_Signature : constant Unsigned_32 := 16#5342_5355#;"
mutant test csw-signature xhci usb_bulk_only.ads \
  "CSW_Signature : constant Unsigned_32 := 16#5342_5355#;" \
  "CSW_Signature : constant Unsigned_32 := 16#4342_5355#;"
echo "MUTANTS: killed $killed of $total"
[ "$killed" -eq "$total" ]
