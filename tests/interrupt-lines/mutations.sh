#!/usr/bin/env bash
# Mutation check for legacy interrupt lines. Each mutation breaks one rule
# in a copy of the kernel's line policy or the SCI route decoder; the hosted
# tests (main.adb) must then fail. --prove also requires gnatprove (level 2)
# to reject the mutations that break a proved contract. A mutation whose
# text is not found, or that does not compile, is stale.
# Run inside the Nix shell: tests/interrupt-lines/mutations.sh [--prove]
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$here/../..
cd "$root/kernel"
WORK=$(mktemp -d "${TMPDIR:-/tmp}/line-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT
prove=0
[[ ${1:-} == --prove ]] && prove=1

caught=0
survived=0
stale=0

mutate() {
    local name=$1 proved=$2 file=$3 from=$4 to=$5
    rm -rf "$WORK/shared" "$WORK/kernel" "$WORK/runtime" "$WORK/obj"
    mkdir -p "$WORK/shared" "$WORK/kernel" "$WORK/runtime"
    cp "$root"/shared/interrupts/interrupt_line_abi.ad? "$WORK/shared/"
    cp "$root"/kernel/src/interrupt_line_policy.ad? "$WORK/kernel/"
    cp "$root"/userspace/runtime/gnat/cubit.ads \
       "$root"/userspace/runtime/gnat/cubit-acpi_fixed_windows.ad? \
       "$root"/userspace/runtime/gnat/cubit-acpi_sci_route.ad? "$WORK/runtime/"
    if ! python3 - "$WORK/$file" "$from" "$to" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1:
    sys.exit(1)
open(path, 'w').write(text.replace(old, new))
PY
    then
        echo "STALE    $name"
        stale=$((stale + 1))
        return
    fi
    local vars=(-XLINES_SHARED_SRC="$WORK/shared" -XLINES_KERNEL_SRC="$WORK/kernel"
                -XLINES_RUNTIME_SRC="$WORK/runtime" -XLINES_OBJ="$WORK/obj"
                -XLINES_EXEC="$WORK/obj" -XLINES_ASSERTIONS=-gnatn)
    if ! alr exec -- gprbuild -p -q -P "$here/interrupt_lines.gpr" "${vars[@]}" \
         >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"
        stale=$((stale + 1))
        return
    fi
    local result
    if timeout 60 "$WORK/obj/main" >"$WORK/run.log" 2>&1; then
        result="SURVIVED"
    else
        result="caught  "
    fi
    local proof=""
    if [[ $prove == 1 && $proved == proved ]]; then
        if alr exec -- gnatprove -P "$here/interrupt_lines.gpr" "${vars[@]}" \
             -u interrupt_line_policy.adb -u cubit-acpi_sci_route.adb \
             --level=2 -j2 --checks-as-errors=on >"$WORK/prove.log" 2>&1; then
            proof=" proof PASSED (mutation not rejected)"
            result="SURVIVED"
        else
            proof=" proof rejected"
        fi
    fi
    if [[ $result == SURVIVED ]]; then
        echo "SURVIVED $name$proof"
        survived=$((survived + 1))
    else
        echo "caught   $name ($(grep -m1 'FAIL\|raised' "$WORK/run.log"))$proof"
        caught=$((caught + 1))
    fi
}

# A storm: the line opens while another owner still has to look.
mutate "acknowledge unmasks while an owner still waits" proved kernel/interrupt_line_policy.adb \
    "      if Was_Masked and then not Any_Waiting (S) then
         S.Held_Masked := False;
         Action := Unmask_Input;
      end if;
   end Acknowledge;" \
    "      if Was_Masked then
         S.Held_Masked := False;
         Action := Unmask_Input;
      end if;
   end Acknowledge;"
mutate "level delivery is not masked (storm until the source is cleared)" proved kernel/interrupt_line_policy.adb \
    "      if S.Trigger = Level then
         for I in Owner_Slot loop" \
    "      if S.Trigger = Edge then
         for I in Owner_Slot loop"
# A lost interrupt: the line stays masked forever.
mutate "acknowledge never unmasks (lost interrupts)" proved kernel/interrupt_line_policy.adb \
    "         S.Held_Masked := False;
         Action := Unmask_Input;
      end if;
   end Acknowledge;" \
    "         null;
      end if;
   end Acknowledge;"
mutate "an ownerless level line is left open" proved kernel/interrupt_line_policy.adb \
    "            S.Held_Masked := True;
            Action := Mask_Input;
         end if;
      elsif" \
    "            null;
         end if;
      elsif"
mutate "a line's owners may disagree on the trigger mode" proved kernel/interrupt_line_policy.adb \
    "      if Has_Owner (S) and then not Same_Mode (S, Trigger, Polarity) then" \
    "      if False and then not Same_Mode (S, Trigger, Polarity) then"
mutate "a non-owner may acknowledge" proved kernel/interrupt_line_policy.adb \
    "      Accepted := Is_Owner (S, Owner);" \
    "      Accepted := Is_Owner (S, Owner) or else Owner /= No_Owner;"
# The SCI's polarity and trigger.
mutate "the SCI's conforming polarity is active high" proved runtime/cubit-acpi_sci_route.adb \
    "(if P = Flags_Active_Low or else (SCI and then P = Flags_Conforming)" \
    "(if P = Flags_Active_Low"
mutate "the SCI without override is edge-triggered" unproved runtime/cubit-acpi_sci_route.adb \
    "                   Trigger    => (if SCI then LA.Level else LA.Edge)," \
    "                   Trigger    => LA.Edge,"
mutate "reserved flags are guessed" proved runtime/cubit-acpi_sci_route.adb \
    "      if P = Reserved or else T = Reserved then" \
    "      if P = Reserved and then T = Reserved and then P /= T then"
mutate "an override's GSI is ignored" unproved runtime/cubit-acpi_sci_route.adb \
    "            Route := (GSI => S.Override_GSI, Trigger => D.Trigger," \
    "            Route := (GSI => GSI_Number (Source), Trigger => D.Trigger,"
mutate "a second override for the source wins silently" unproved runtime/cubit-acpi_sci_route.adb \
    "      elsif S.Overrides > 1 then" \
    "      elsif False then"
mutate "a GSI another I/O APIC serves is routed" unproved runtime/cubit-acpi_sci_route.adb \
    "      elsif S.IO_APICs > 1 and then Route.GSI >= S.Lowest_Other then" \
    "      elsif False then"
mutate "an identity GSI taken by another override is routed" unproved runtime/cubit-acpi_sci_route.adb \
    "      if Taken_By_Other (MADT, V.Length, Route.GSI, Source) then" \
    "      if Taken_By_Other (MADT, V.Length, Route.GSI, Source) and then Route.GSI = GSI_Number'Last then"
mutate "a hardware-reduced FADT still has an SCI" unproved runtime/cubit-acpi_sci_route.adb \
    "         return (Present => False, Why => Hardware_Reduced);" \
    "         null;"
mutate "an override on another bus is accepted" unproved runtime/cubit-acpi_sci_route.adb \
    "              or else U8 (MADT, Here + Override_Bus_Offset) /= ISA_Bus" \
    "              or else False"
mutate "a MADT cut short is read as far as it goes" unproved runtime/cubit-acpi_sci_route.adb \
    "      elsif Natural (Stated) > Item'Length or else Stated < Header_Bytes then" \
    "      elsif Stated < Header_Bytes then"

echo "interrupt-lines mutations: $caught caught, $survived survived, $stale stale"
[[ $survived == 0 && $stale == 0 ]]
