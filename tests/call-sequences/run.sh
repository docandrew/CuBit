#!/usr/bin/env bash
#  Hosted tests for Call_Sequences (docs/ipc-fastpath.md, "Call deadlines");
#  --prove also runs gnatprove (level 1). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/call_sequences_test.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/call_sequences_test.gpr" -u call_sequences.adb \
        --level=1 -j4 --checks-as-errors=on
fi
