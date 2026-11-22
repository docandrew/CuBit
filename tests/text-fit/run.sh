#!/usr/bin/env bash
#  Hosted tests for Text_Fit (toolkit fit-with-ellipsis); --prove also runs
#  gnatprove (level 2) through the model instance. Run in the Nix shell; run
#  the proof under a memory cap (systemd-run --user --scope -p MemoryMax=24G
#  -p MemorySwapMax=0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/text_fit_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/text_fit_proof.gpr" -j2 --level=2
fi
