#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  Mutation check of net.fetch's codecs (userspace/lib/fetch): each mutant
#  breaks one rule; the hosted tests or the level-2 proof must fail for
#  each. Run in the Nix shell under the memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/fetch-codecs/mutate.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
lib=$here/../../userspace/lib/fetch
files=("$lib/fetch_urls.ads" "$lib/fetch_urls.adb" "$lib/http_responses.ads" "$lib/http_responses.adb")
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
          && cd "$here/../../kernel" && alr exec -- gnatprove -P "$here/fetch_codecs_tests.gpr" -u fetch_urls.adb -u http_responses.adb \
               --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Fetch_Urls with" "package Fetch_Urls with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "fetch-codecs: baseline fails" >&2; exit 1; }
survivors=0
check "      elsif not Starts_With_Ignoring_Case (Text, SCHEME) then" "      elsif False then" "any scheme is fetched"
check "         if Text (I) = '@' then" "         if False then" "userinfo is accepted"
check "         | ':' | '@' | '/' | '?' | '%');" "         | ':' | '@' | '/' | '?' | '%' | ' ' | ASCII.CR | ASCII.LF);" \
      "CR, LF and space are target characters (header injection)"
check "              or else (Target (I) = '%'" "              or else (False" "a bad percent-encoding is accepted"
check "         Why := (if (for some C of Location => C = ':') then Not_Https else Unsupported_Reference);" \
      "         Parse (\"https://\" & Location, U, Why);" "a relative or http redirect is followed"
check "               if Text (I) not in '0' .. '9' or else Value > 9_999 then" \
      "               if Text (I) not in '0' .. '9' then" "a six-digit port is read"
check "            if Seen_Length and then N /= R.Length then" "            if False then" \
      "the last of two different lengths wins"
check "      if Seen_Length and then Seen_Chunked then" "      if False then" "Content-Length with chunked is accepted"
check "            if Seen_Chunked or else not Same_Ignoring_Case (Value, \"chunked\") then" \
      "            if Seen_Chunked then" "any transfer coding is read as chunked"
check "            if not Same_Ignoring_Case (Value, \"identity\") then" "            if False then" \
      "a gzip content coding is passed through"
check "      if H (First) in ' ' | HTAB then" "      if False then" "a folded line is read as a field"
check "            if Available > 0 then
               Refuse (R, Extra_Data, E);" "            if False then
               Refuse (R, Extra_Data, E);" "bytes after the body are ignored"
check "                  if N > R.Ceiling - R.Delivered then
                     Refuse (R, Body_Too_Large, E);
                     return;
                  end if;
                  E := (Kind => Body_Run, First => Next, Last => Next + Natural (N) - 1, Why => No_Refusal);
                  Next := Next + Natural (N);
                  R.Delivered := R.Delivered + N;
                  if R.Phase" "                  E := (Kind => Body_Run, First => Next, Last => Next + Natural (N) - 1, Why => No_Refusal);
                  Next := Next + Natural (N);
                  R.Delivered := R.Delivered + N;
                  if R.Phase" "the body ceiling is not checked"
check "            R.Phase := Done_Phase;
            E := (Kind => Complete, others => <>);
         when others =>" "            R.Phase := Done_Phase;
            E := (Kind => Complete, others => <>);
         when In_Length_Body =>
            R.Phase := Done_Phase;
            E := (Kind => Complete, others => <>);
         when others =>" "a length-delimited body cut short completes"
check "            elsif R.Status <= INFORMATIONAL_LAST then" "            elsif False then" "a 1xx head is taken as final"
check "               elsif C in ';' | ' ' | HTAB then" "               elsif C in ';' | ' ' | HTAB | LF then" \
      "a chunk size line ending in a bare LF"
[ "$survivors" = 0 ] && echo "fetch-codecs: mutation OK" || { echo "fetch-codecs: $survivors survived" >&2; exit 1; }
