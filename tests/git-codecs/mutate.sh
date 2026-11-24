#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  Mutation check of the git codecs (userspace/lib/git): each mutant breaks
#  one rule; the tests (run.sh) or the level-2 proof must fail for each.
#  The mutants are copies (build/mutant-src, named to the build by
#  GIT_SOURCES); the tree's sources are never touched. Run in the Nix
#  shell under the memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/git-codecs/mutate.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
lib=$here/../../userspace/lib/git
mutants=$here/build/mutant-src
mkdir -p "$mutants"
export GIT_SOURCES=$mutants
restore() { rm -f "$mutants"/*; cp "$lib"/*.ads "$lib"/*.adb "$mutants/"; }
trap 'rm -rf "$mutants"' EXIT
survivors=0
check() {   # old new name
    restore
    python3 - "$1" "$2" "$mutants"/*.ads "$mutants"/*.adb <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
old, new, *paths = sys.argv[1:]
for p in paths:
    s = open(p).read()
    if old in s:
        open(p, "w").write(s.replace(old, new, 1)); break
else:
    sys.exit(1)
PY
    if (bash "$here/run.sh" >/dev/null 2>&1 \
          && cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/git_codecs_tests.gpr" -u git_packets.adb \
               -u git_protocol.adb -u git_packs.adb -u git_deltas.adb -u git_objects.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Git_Packs with" "package Git_Packs with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "git-codecs: baseline fails" >&2; exit 1; }
survivors=0
check "                                     and then not (S (I) = '.' and then S (I + 1) = '.')" \
      "                                     and then True" "a ref name with \"..\""
check "                                     (if S (I .. I + 4) = \".lock\" then I + 4 < S'Last and then S (I + 5) /= '/')))));" \
      "                                     True))));" "a ref component ending in .lock"
check "                                     and then not (S (I) = '/' and then S (I + 1) = '.'))" \
      "                                     and then True)" "a ref component starting with ."
check "(C in '!' .. '~' and then C not in '~' | '^' | ':' | '?' | '*' | '[' | '\\');" \
      "(C in '!' .. '~' and then C not in '~' | '^' | '?' | '*' | '[' | '\\');" "a ':' in a ref name"
check "function Lower_Hex (C : Character) return Boolean is (C in '0' .. '9' | 'a' .. 'f');" \
      "function Lower_Hex (C : Character) return Boolean is (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');" \
      "upper-case hex ids"
check "                     when RESERVED_LENGTH_VALUE =>
                        Refuse (Reserved_Length);" "                     when RESERVED_LENGTH_VALUE =>
                        Emit (Data_Packet);" "length 0003 read as an empty packet"
check "                     when LENGTH_DIGITS + 1 .. MAXIMUM_PACKET_BYTES =>" \
      "                     when LENGTH_DIGITS + 1 .. MAXIMUM_PACKET_BYTES + 1 =>" "a packet one past LARGE_PACKET_MAX"
check "                     if Key_Last < Last and then Matches (Payload, Key_Last + 2, Last, \"sha1\") then" \
      "                     if True then" "any object format"
check "               if not A.Ls_Refs then" "               if False then" "no ls-refs needed"
check "            elsif Payload (Payload'First) = ERROR_CHANNEL then
               Refuse (Remote_Error, Payload'First + 1, Payload'Last);" \
      "            elsif Payload (Payload'First) = ERROR_CHANNEL then
               null;" "the server's error channel ignored"
check "               Refuse (Unexpected_Section, Payload'First, Last);" "               null;" "an unasked-for section skipped"
check "               if Git_Protocol.\"=\" (P.Trailer, P.Sum) then" "               if True then" \
      "the pack trailer is not compared"
check "            if P.Delivered /= P.Size then" "            if False then" "an entry's size is not checked"
check " or else P.Distance > P.Entry_At - HEADER_BYTES then" " then" "an OFS base before the pack"
check "               elsif Version not in FIRST_VERSION .. LAST_VERSION then" "               elsif False then" \
      "any pack version"
check "         if Before in Entry_Header | Ofs_Offset | Ref_Base | Entry_Data then" \
      "         if Before in Entry_Data then" "the CRC leaves out the entry header"
check "         if Before /= Trailer then" "         if Before = Entry_Data then" "the checksum leaves out headers"
check "                     if From > A.Base_Bytes or else Size > A.Base_Bytes - From then" \
      "                     if From > A.Base_Bytes then" "a copy past the base"
check "(if Given = 0 then ZERO_SIZE_MEANS else Given)" \
      "Given" "copy size 0 is 0"
check "               elsif A.Done = A.Target then
                  Refuse (Extra_Data);" "               elsif False then
                  Refuse (Extra_Data);" "instructions after the result"
check "                  elsif C = 0 then
                     Refuse (Reserved_Instruction);" "                  elsif False then
                     Refuse (Reserved_Instruction);" "instruction 0 accepted"
check "      and then not (S'Length = 4 and then S (S'First) = '.'" "      and then not (False" "a tree entry named .git"
check "      if Has_Previous and then not Before (Tree, Previous, E) then" "      if False then" \
      "tree entries out of order"
check "      Bytes : SPARKNaCl.Byte_Seq (0 .. Head'Length) := [others => 0];" \
      "      Bytes : SPARKNaCl.Byte_Seq (0 .. Head'Length - 1) := [others => 0];" "object names without the NUL"
check "      if Mode_Is (\"100644\") then" "      if Mode_Is (\"100644\") or else Mode_Is (\"100664\") then" \
      "mode 100664 accepted"
[ "$survivors" = 0 ] && echo "git-codecs: mutation OK" || { echo "git-codecs: $survivors survived" >&2; exit 1; }
