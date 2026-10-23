#!/usr/bin/env bash
# Linux-hosted run of every fs-coherence case (tests/fs-coherence/cases.txt)
# on the host's own filesystem: checks the cases' expectations against
# Linux's semantics (the reference CuBit must match). Not CuBit code.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/fs-coherence.XXXXXX")
trap 'rm -rf "$work"' EXIT
${CC:-gcc} -O2 -Wall -Wextra -o "$work/fs-coherence" "$here/fs-coherence.c"
mkdir "$work/files"
failed=0
while read -r name; do
    out=$("$work/fs-coherence" "$name" "$work/files") || failed=1
    printf '%s\n' "$out" | grep -v ': end$'
    grep -qx "fs-coherence: $name PASS" <<<"$out" || failed=1
done < "$here/cases.txt"
[ "$failed" = 0 ] && echo "fs-coherence host: PASS" || { echo "fs-coherence host: FAIL"; exit 1; }
