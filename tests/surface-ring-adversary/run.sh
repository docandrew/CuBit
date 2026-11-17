#!/usr/bin/env bash
#  Hosted adversary test for Desktop's side of the surface rings (IPC-004):
#  Desktop_Surface_Ring_Service against hostile clients beside an honest
#  one. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/adversary.gpr"
"$here/build/main" | tee "$here/build/result.txt"
grep -q '^SURFACE-RING-ADVERSARY: PASS' "$here/build/result.txt"
