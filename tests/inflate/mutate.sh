#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  Mutation check of the inflater (userspace/lib/compress): each mutant
#  breaks one rule; the tests (run.sh) or the level-2 proof must fail for
#  each. Run in the Nix shell under the memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/inflate/mutate.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
#  The mutants are copies (build/mutant-src, named to the build by
#  INFLATE_SOURCES); the tree's sources are never touched.
lib=$here/../../userspace/lib/compress
mutants=$here/build/mutant-src
mkdir -p "$mutants"
export INFLATE_SOURCES=$mutants
files=("$mutants/inflate_streams.ads" "$mutants/inflate_streams.adb")
restore() { cp "$lib/inflate_streams.ads" "$lib/inflate_streams.adb" "$mutants/"; }
trap 'rm -rf "$mutants"' EXIT
survivors=0
check() {   # old new name
    restore
    python3 - "$1" "$2" "${files[@]}" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
old, new, *paths = sys.argv[1:]
for p in paths:
    s = open(p).read()
    if old in s:
        open(p, "w").write(s.replace(old, new, 1)); break
else:
    sys.exit(1)
PY
    if (INFLATE_FUZZ=3000 bash "$here/run.sh" >/dev/null 2>&1 \
          && cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/inflate_tests.gpr" -u inflate_streams.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Inflate_Streams with" "package Inflate_Streams with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "inflate: baseline fails" >&2; exit 1; }
survivors=0
check "      if Distance_Value = 0 or else Distance_Value > Window_Have (D) or else" \
      "      if Distance_Value = 0 or else" "a distance before the start of the output"
check " or else Distance_Value > D.Window_Limit then" " then" "a distance past the declared window"
check "                  if Complement /= (Length xor 16#FFFF#) then" "                  if False then" \
      "a stored block's NLEN is not checked"
check "               if D.Expected /= U32 (D.Adler_B) * 2 ** 16 + U32 (D.Adler_A) then" "               if False then" \
      "the Adler-32 is not checked"
check "               elsif (CMF * 256 + FLG) mod HEADER_CHECK_DIVISOR /= 0 then" "               elsif False then" \
      "the zlib header check is not checked"
check "               elsif (FLG / DICTIONARY_FLAG) mod 2 = 1 then" "               elsif False then" \
      "a preset dictionary is ignored"
check "         if Left * 2 < Count (L) then" "         if False then" "an over-subscribed code is accepted"
check "         elsif Longest > 1 then" "         elsif False then" "an incomplete code is accepted"
check "      if D.Lengths (END_OF_BLOCK) = 0 then" "      if False then" "a code without end-of-block is accepted"
check "               if Literals > MAXIMUM_LITERAL_CODES or else" "               if" "HLIT 30 and 31 are accepted"
check "                  if D.Lengths_Read = 0 then
                     Stop_Refused (D, Bad_Repeat, Result, Stop);" "                  if False then
                     Stop_Refused (D, Bad_Repeat, Result, Stop);" "repeat 16 with nothing to repeat"
check "               if D.Lengths_Read + Base + Value > Total then" "               if False then" \
      "a repeat past the code count"
check "               if D.Total_Out >= D.Ceiling then" "               if False then" \
      "a literal past the ceiling"
check "            elsif Sym > LAST_LENGTH_SYMBOL then" "            elsif False then" \
      "literal/length symbols 286 and 287"
check "                  if Sym > LAST_DISTANCE_SYMBOL then" "                  if False then" \
      "distance symbols 30 and 31"
check "(D.Window_Next + WINDOW_BYTES - D.Copy_Distance) mod WINDOW_BYTES" \
      "(D.Window_Next + WINDOW_BYTES - D.Copy_Distance + 1) mod WINDOW_BYTES" "a match copies one byte late"
check "      195, 227, 258];" "      195, 227, 257];" "symbol 285 is length 257"
check "[0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]" \
      "[0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 13, 13, 13]" \
      "distance symbol 27 takes 13 extra bits"
check "[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]" \
      "[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 15, 1]" "the code-length order swaps 1 and 15"
check "                  --  3.2.4: the rest of the current byte is skipped.
                  D.Bits := 0;
                  D.Bit_Count := 0;" "                  --  3.2.4: the rest of the current byte is skipped." \
      "a stored block is not byte-aligned"
check "            D.Final_Block := Value mod 2 = 1;" "            D.Final_Block := False;" "BFINAL is ignored"
check "      else
         Refuse (D, Truncated);
         Result := Refused;" "      else
         Result := Finished;" "a truncated stream ends well"
check "         D.Bits := D.Bits or Interfaces.Shift_Left (U32 (Input (In_Next)), D.Bit_Count);" \
      "         D.Bits := D.Bits or Interfaces.Shift_Left (U32 (Input (In_Next)), D.Bit_Count + 1);" \
      "bits are read misplaced"
check "      if Extended >= First and then Extended - First < Count then" \
      "      if Extended >= First and then Extended - First <= Count then" "a code one past its length's codes"
check "      while D.Stored_Remaining > 0 and then In_Next <= Input'Last and then Out_Next <= Output'Last
        and then D.Total_Out < D.Ceiling" \
      "      while D.Stored_Remaining > 0 and then In_Next <= Input'Last and then Out_Next <= Output'Last" \
      "stored data past the ceiling"
[ "$survivors" = 0 ] && echo "inflate: mutation OK" || { echo "inflate: $survivors survived" >&2; exit 1; }
