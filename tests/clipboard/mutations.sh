#!/usr/bin/env bash
# Mutation checks for the desktop clipboard (docs/clipboard.md). Each mutant
# breaks one rule; the hosted tests and the level-2 proof must each reject
# it. The first mutant changes nothing that matters and must pass both.
# Run inside nix develop from anywhere: bash tests/clipboard/mutations.sh
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
desktop="$root/userspace/services/desktop"
ui="$root/userspace/lib/ui"
proved_units="clipboard_store.adb clipboard_gestures.adb clipboard_menu.adb compositor_clipboard_protocol.adb clipboard_chords.adb clipboard_text.adb"
units="clipboard_store.ads clipboard_store.adb clipboard_gestures.ads clipboard_gestures.adb clipboard_menu.ads clipboard_menu.adb"
ui_units="clipboard_chords.ads clipboard_chords.adb clipboard_text.ads clipboard_text.adb"
cd "$root/kernel"
failures=0
mutant() {
  local name="$1" file="$2" old="$3" new="$4" expect="$5"
  local work; work="$(mktemp -d "$here/build/mutant.XXXXXX")"
  mkdir -p "$work/src" "$work/ui"
  for u in $units; do cp "$desktop/$u" "$work/src/"; done
  for u in $ui_units; do cp "$ui/$u" "$work/ui/"; done
  local target="$work/src/$file"
  [ -e "$target" ] || target="$work/ui/$file"
  if ! OLD="$old" NEW="$new" python3 - "$target" <<'PY'
import os, sys
p = sys.argv[1]; s = open(p).read(); old = os.environ["OLD"]; new = os.environ["NEW"]
if s.count(old) != 1: sys.exit(1)
open(p, "w").write(s.replace(old, new))
PY
  then echo "BADTEXT  $name"; failures=$((failures + 1)); rm -rf "$work"; return; fi
  local test_result proof_result
  if CLIPBOARD_SRC="$work/src" CLIPBOARD_UI="$work/ui" CLIPBOARD_OBJ="$work/obj" alr exec -- gprbuild -p -q -P "$here/tests.gpr" >/dev/null 2>&1 &&
     "$work/obj/clipboard_tests" > "$work/test.log" 2>&1; then test_result=pass; else test_result=fail; fi
  if CLIPBOARD_SRC="$work/src" CLIPBOARD_UI="$work/ui" CLIPBOARD_OBJ="$work/prove" alr exec -- gnatprove -P "$here/prove.gpr" --mode=all -u $proved_units > "$work/prove.log" 2>&1
  then proof_result=pass; else proof_result=fail; fi
  local why; why="$(grep -h -m1 -E 'FAIL:|clipboard_[a-z_]+\.ad[sb]:[0-9]+:[0-9]+: (medium|high)' "$work/test.log" "$work/prove.log" 2>/dev/null | sed 's/^[^:]*://' | head -c 150)"
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
mutant "control: comment only" clipboard_store.adb \
  "      --  Paste once. An ordinary entry pasted into a secret field becomes" \
  "      --  Paste once. An ordinary entry pasted into a secret field turns into" survive
mutant "secret not consumed by paste" clipboard_store.adb \
  "      if Meta.Kind = Sensitive or else Target = Sensitive then" \
  "      if Target = Sensitive then" kill
mutant "paste skips expiry" clipboard_store.adb \
  "      Data := Zero_Bytes;
      Expire (S, Now);" \
  "      Data := Zero_Bytes;" kill
mutant "wipe keeps bytes" clipboard_store.adb \
  "      E.Data := Zero_Bytes;
      if Phase = Vacant then" \
  "      if Phase = Vacant then" kill
mutant "secret can be pinned" clipboard_store.adb \
  "        S.Slots (Slot).Meta.Kind /= Ordinary
      then
         return;
      end if;
      E := S.Slots (Slot);
      E.Meta.Pinned := Pinned;" \
  "        False
      then
         return;
      end if;
      E := S.Slots (Slot);
      E.Meta.Pinned := Pinned;" kill
mutant "ticket reusable" clipboard_gestures.adb \
  "      if Granted then
         S (Kind) := (others => <>);
      end if;" \
  "      null;" kill
mutant "ticket ignores surface" clipboard_gestures.adb \
  "      Granted := Fresh (S (Kind), Surface, Now);" \
  "      Granted := S (Kind).Live;" kill
mutant "Enter issues a paste ticket" clipboard_chords.adb \
  "            return (if Is_Activation_Key (Code, Modifiers) then Copy_Ticket" \
  "            return (if Is_Activation_Key (Code, Modifiers) then Paste_Ticket" kill
mutant "Ctrl+Shift+Delete cuts" clipboard_chords.adb \
  "        (Shift and then not Ctrl and then Code = Key_Delete)" \
  "        (Shift and then Code = Key_Delete)" kill
mutant "single line keeps control bytes" clipboard_text.adb \
  "         if Is_Printable (Text (I)) then" \
  "         if Text (I) /= ASCII.LF then" kill
exit $failures
