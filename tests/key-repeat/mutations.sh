#!/usr/bin/env bash
# Mutation checks for desktop keyboard auto-repeat (Key_Repeat). Each mutant
# breaks one rule; the hosted tests and the level-2 proof must each reject
# it. The first mutant changes nothing that matters and must pass both.
# Run inside nix develop from anywhere: bash tests/key-repeat/mutations.sh
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
desktop="$root/userspace/services/desktop"
units="key_repeat.ads key_repeat.adb"
cd "$root/kernel"
failures=0
mutant() {
  local name="$1" file="$2" old="$3" new="$4" expect="$5"
  local work; work="$(mktemp -d "$here/build/mutant.XXXXXX")"
  mkdir -p "$work/src"
  for u in $units; do cp "$desktop/$u" "$work/src/"; done
  if ! OLD="$old" NEW="$new" python3 - "$work/src/$file" <<'PY'
import os, sys
p = sys.argv[1]; s = open(p).read(); old = os.environ["OLD"]; new = os.environ["NEW"]
if s.count(old) != 1: sys.exit(1)
open(p, "w").write(s.replace(old, new))
PY
  then echo "BADTEXT  $name"; failures=$((failures + 1)); rm -rf "$work"; return; fi
  local test_result proof_result
  if KEY_REPEAT_SRC="$work/src" KEY_REPEAT_OBJ="$work/obj" alr exec -- gprbuild -p -q -P "$here/tests.gpr" >/dev/null 2>&1 &&
     "$work/obj/key_repeat_tests" > "$work/test.log" 2>&1; then test_result=pass; else test_result=fail; fi
  if KEY_REPEAT_SRC="$work/src" KEY_REPEAT_OBJ="$work/prove" alr exec -- gnatprove -P "$here/prove.gpr" --mode=all -u key_repeat.adb > "$work/prove.log" 2>&1
  then proof_result=pass; else proof_result=fail; fi
  local why; why="$(grep -h -m1 -E 'FAIL:|key_repeat\.ad[sb]:[0-9]+:[0-9]+: (medium|high)' "$work/test.log" "$work/prove.log" 2>/dev/null | sed 's/^[^:]*://' | head -c 150)"
  if [ "$expect" = survive ]; then
    if [ "$test_result$proof_result" = passpass ]; then echo "control  $name (tests and proof pass)";
    else echo "BROKEN   $name control failed: tests=$test_result proof=$proof_result"; failures=$((failures + 1)); fi
  elif [ "$test_result" = fail ] && [ "$proof_result" = fail ]; then
    echo "killed   $name: $why"
  else
    echo "SURVIVED $name: tests=$test_result proof=$proof_result"; failures=$((failures + 1))
  fi
  rm -rf "$work"
}
mkdir -p "$here/build"
mutant "control: comment only" key_repeat.adb \
  "         --  Late (a stall): one repeat now, the next a full period later." \
  "         --  Late (a stall): a single repeat now, the next a full period later." survive
mutant "device repeat not dropped" key_repeat.adb \
  "      elsif S.Held (K.Code, K.Extended) then
         Outcome := Held_Repeat;
         return;
      end if;" \
  "      end if;" kill
mutant "modifier press does not stop" key_repeat.adb \
  "      else
         S := (S with delta Active => False, Due => NEVER);
      end if;
   end Press;" \
  "      end if;
   end Press;" kill
mutant "modifier release does not stop" key_repeat.adb \
  "(S.Current = K or else Class_Of (K) = Modifier)" \
  "S.Current = K" kill
mutant "any release stops" key_repeat.adb \
  "(S.Current = K or else Class_Of (K) = Modifier)" \
  "True" kill
mutant "route change ignored" key_repeat.adb \
  "      elsif Where /= S.Target then" \
  "      elsif False then" kill
mutant "no delay (period only)" key_repeat.adb \
  "               Due      => Now + Config.Initial_Delay," \
  "               Due      => Now + MILLISECONDS_PER_SECOND / Config.Rate," kill
mutant "no carry (drifting rate)" key_repeat.adb \
  "      Next := S.Due + Period (S.Rate, S.Fraction);" \
  "      Next := S.Due + MILLISECONDS_PER_SECOND / S.Rate;" kill
mutant "carry not advanced" key_repeat.adb \
  "      Next_Fraction := Next_Carry (S.Rate, S.Fraction);" \
  "      Next_Fraction := S.Fraction;" kill
mutant "stall bursts (no re-anchor)" key_repeat.adb \
  "      if Next <= Now then" \
  "      if False then" kill
mutant "toggles repeat" key_repeat.adb \
  "      if Class_Of (K) = Repeating and then Config.Enabled" \
  "      if Class_Of (K) /= Modifier and then Config.Enabled" kill
mutant "non-repeatable destination repeats" key_repeat.adb \
  "and then Repeatable (Where.Where) then" \
  "then" kill
mutant "cancel forgets held keys" key_repeat.adb \
  "   procedure Cancel (S : in out State) is
   begin
      S := (S with delta Active => False, Due => NEVER);" \
  "   procedure Cancel (S : in out State) is
   begin
      S := (S with delta Active => False, Due => NEVER, Held => [others => [others => False]]);" kill
echo "mutations: $failures unexpected"
[ "$failures" -eq 0 ]
