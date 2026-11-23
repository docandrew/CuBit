#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  Mutation check of the tar reader (userspace/lib/archive): each mutant
#  breaks one rule; the hosted tests, the interop walks or the level-2
#  proof must fail for each. Run in the Nix shell under the memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/tar-reader/mutate.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
lib=$here/../../userspace/lib/archive
files=("$lib/tar_archives.ads" "$lib/tar_archives.adb")
saved=$(mktemp -d)
for f in "${files[@]}"; do cp "$f" "$saved/$(basename "$f")"; done
restore() { for f in "${files[@]}"; do cp "$saved/$(basename "$f")" "$f"; done; }
trap 'restore; rm -rf "$saved"' EXIT
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
    if (bash "$here/run.sh" >/dev/null 2>&1 \
          && cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/tar_reader_tests.gpr" -u tar_archives.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Tar_Archives with" "package Tar_Archives with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "tar-reader: baseline fails" >&2; exit 1; }
survivors=0
check "           (S (I + 1) = '.' and then (I + 1 = S'Last or else S (I + 2) = '/'))))" \
      "           False))" "a \"..\" component is allowed"
check "      and then S (S'First) /= '/' and then S (S'Last) /= '/'" \
      "      and then S (S'Last) /= '/'" "an absolute path is allowed"
check "     (C in ' ' .. '~' and then C /= '\\');" "     (C in ' ' .. '~');" "a backslash is allowed"
check "     (C in ' ' .. '~' and then C /= '\\');" "     (C /= '\\');" "control and non-ASCII bytes are allowed"
check "      and then (for all I in S'First .. S'Last - 1 => not (S (I) = '/' and then S (I + 1) = '/'))" \
      "      and then True" "an empty component is allowed"
check "      return Why = No_Refusal and then Stored = U64 (Sum);" "      return Why = No_Refusal;" \
      "the checksum is not compared"
check "Character'Pos ('0') .. Character'Pos ('7') loop" "Character'Pos ('0') .. Character'Pos ('9') loop" \
      "8 and 9 read as octal digits"
check "      if not Confined (W.Current (1 .. W.Current_Length)) then" "      if False then" \
      "an entry's path is not checked"
check "               elsif Key = \"size\"" "               elsif Key = \"no-such-key\"" "a pax size override is obeyed"
check "      if W.Zero_Blocks = 1 then
         Refuse (W, Lone_Zero_Block, E);" "      if False then
         Refuse (W, Lone_Zero_Block, E);" "a lone zero block is accepted"
check "         W.Pending_Length := 0;" "         null;" "a long name names every later entry"
check "         if Posix and then B (PREFIX_AT) /= NUL then" "         if B (PREFIX_AT) /= NUL then" \
      "a GNU header's prefix bytes are read as a prefix"
check "            elsif W.Entries >= W.Ceiling.Maximum_Entries then
               Refuse (W, Too_Many_Entries, E);
            elsif Size" "            elsif W.Entries > W.Ceiling.Maximum_Entries then
               Refuse (W, Too_Many_Entries, E);
            elsif Size" "one file more than the entry ceiling"
check "            elsif Size > W.Ceiling.Maximum_Bytes - W.Bytes then" "            elsif False then" \
      "the byte ceiling is not checked"
check "            elsif W.Specials = Refuse_Specials then" "            elsif False then" \
      "links are skipped though the caller refuses them"
check "               W.Remaining := Before - U64 (N);" "               W.Remaining := Before - U64 (N) + (if Before > 600 then 1 else 0);" \
      "a large file's data runs one byte long"
check "      if W.Phase = Ended_Phase then
         E := (Kind => Finished, others => <>);" "      if W.Phase /= Failed_Phase then
         E := (Kind => Finished, others => <>);" "a truncated archive ends well"
[ "$survivors" = 0 ] && echo "tar-reader: mutation OK" || { echo "tar-reader: $survivors survived" >&2; exit 1; }
