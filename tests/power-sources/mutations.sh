#!/usr/bin/env bash
# Mutation checks for power.svc's ACPI client side (PWR-004): the watch's
# decisions, the identities and names, the reading's restart, the aging of
# an incomplete reading. Each mutant breaks one rule; the hosted tests must
# reject it (the --prove ones: the level-2 proof too). The first mutant
# changes nothing that matters and must pass.
# Run inside nix develop, after run.sh made build/links:
#   bash tests/power-sources/mutations.sh [--prove]
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
prove="${1:-}"
[ -d "$links" ] || { echo "mutations: run tests/power-sources/run.sh first" >&2; exit 2; }
cd "$root/kernel"
failures=0
mutant() {
  local name="$1" file="$2" old="$3" new="$4" expect="$5" with_proof="${6:-}"
  local work; work="$(mktemp -d "$here/build/mutant.XXXXXX")"
  mkdir -p "$work/links"
  cp -L "$links"/* "$work/links/"
  if ! OLD="$old" NEW="$new" python3 -I - "$work/links/$file" <<'PY'
import os, sys
p = sys.argv[1]; s = open(p).read(); old = os.environ["OLD"]; new = os.environ["NEW"]
if s.count(old) != 1: sys.exit(1)
open(p, "w").write(s.replace(old, new))
PY
  then echo "BADTEXT  $name"; failures=$((failures + 1)); rm -rf "$work"; return; fi
  local test_result proof_result=skipped why
  if POWER_LINKS="$work/links" POWER_OBJ="$work/obj" alr exec -- gprbuild -p -q -P "$here/power_sources_tests.gpr" \
       >"$work/build.log" 2>&1 && "$work/obj/power_sources_tests" >"$work/test.log" 2>&1
  then test_result=pass; else test_result=fail; fi
  if [ "$prove" = --prove ] && [ -n "$with_proof" ]; then
    mkdir -p "$work/prove"
    sed -e "s|\"build/links\"|\"$work/links\"|" -e "s|\"build/prove\"|\"$work/prove\"|" \
        -e "s|\"prove\",|\"$here/prove\",|" -e "s|\"../../userspace|\"$root/userspace|" \
        "$here/prove.gpr" >"$work/prove.gpr"
    if systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 \
         alr exec -- gnatprove -P "$work/prove.gpr" -j2 --level=2 --checks-as-errors=on >"$work/prove.log" 2>&1
    then proof_result=pass; else proof_result=fail; fi
  fi
  why="$(grep -h -m1 -E 'FAIL:|: (medium|high):|error:' "$work/test.log" "$work/build.log" "$work/prove.log" 2>/dev/null | head -c 160)"
  if [ "$expect" = survive ]; then
    if [ "$test_result" = pass ] && [ "$proof_result" != fail ]; then echo "control  $name (passes)";
    else echo "BROKEN   $name: tests=$test_result proof=$proof_result $why"; failures=$((failures + 1)); fi
  elif [ "$test_result" = fail ] && [ "$proof_result" != pass ]; then
    echo "killed   $name (proof $proof_result): $why"
  else
    echo "SURVIVED $name: tests=$test_result proof=$proof_result"; failures=$((failures + 1))
  fi
  rm -rf "$work"
}
mutant "control: comment only" power_acpi_watch.adb \
  "         --  A Changed that does not move on is not one acpi.svc writes." \
  "         --  A Changed that does not move on is never written by acpi.svc." survive yes
mutant "a Changed that does not move on is read" power_acpi_watch.adb \
  "      if Why = Changed and then Reply (Generation_Word) <= Seen then" \
  "      if False then" kill yes
mutant "Unavailable ends the watch" power_acpi_watch.adb \
  "         when Unavailable | Full =>
            Next := Retry_Later;" \
  "         when Unavailable | Full =>
            Next := Give_Up;" kill yes
mutant "an earlier acpi.svc's generation is kept" power_acpi_watch.adb \
  "            Next := Resynchronize;
            New_Seen := 0;" \
  "            Next := Resynchronize;" kill yes
mutant "another watch's answer is taken" power_acpi_watch.adb \
  "      if Label /= Watch_Label or else Length /= Request_Words or else Reply (ID_Word) /= ID" \
  "      if Label /= Watch_Label or else Length /= Request_Words" kill yes
mutant "a shared last segment still names" power_acpi_identity.adb \
  "              and then (for all J in 1 .. Count => J = I or else Shorts (J) /= Shorts (I))" \
  "" kill
mutant "padding stays in the name" power_acpi_identity.adb \
  "      while Stop >= First and then P.Text (Stop) = Padding loop" \
  "      while False and then P.Text (Stop) = Padding loop" kill
mutant "a chunk with a foreign character is kept" power_acpi_identity.adb \
  "            if not Path_Character (C) then
               Valid := False;
               return;
            end if;" \
  "            null;" kill yes
mutant "a chunk with bytes past the path's end is kept" power_acpi_identity.adb \
  "      Valid := (for all I in Count .. Chunk_Characters => Byte_Of (Chunk, I) = 0);" \
  "      Valid := True;" kill
mutant "a family moving on mid-path is not read again" power_acpi_sources.adb \
  "         elsif Code = ID.Stale or else (Code = ID.OK and then Generation /= Expected) then
            Outcome := Moved_On;
            return;" \
  "         elsif Code = ID.Stale or else (Code = ID.OK and then Generation /= Expected) then
            return;" kill
mutant "names from another slot" power_source_mapping.adb \
  "            Origins (Count) := Slot;" \
  "            Origins (Count) := 1;" kill yes
mutant "an incomplete reading stays Fresh" power_source_mapping.ads \
  "     (if F = PS.Fresh then PS.Stale else F);" \
  "     (F);" kill yes
mutant "the adapter's own age is ignored" power_acpi_reader.adb \
  "      Read_AC ((if AC_Maximum_Age = 0 then Maximum_Age else AC_Maximum_Age), Result.AC, Result.AC_Generation," \
  "      Read_AC (Maximum_Age, Result.AC, Result.AC_Generation," kill
echo "mutations: $failures failures"
[ "$failures" -eq 0 ]
