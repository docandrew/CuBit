#!/usr/bin/env bash
#  Hosted tests for the per-thread reply slot (BUG-004); --prove also runs
#  gnatprove (level 2) on Capabilities.Operations. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/reply_slots_test.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/reply_slots_test.gpr" -u capabilities-operations.adb \
        --mode=all --level=2 --prover=cvc5,z3 -j2 --checks-as-errors=on
fi
