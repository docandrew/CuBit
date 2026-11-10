#!/usr/bin/env bash
#  Hosted tests for the toolkit's layout containers (UI-033):
#  Box_Distribution against random inputs, CuBit.UI.Boxes with the real UI
#  font (make -C kernel ui-fonts-host first), CuBit.UI.Text_Views. --prove
#  also runs gnatprove (level 2) on Box_Distribution; run that under a
#  memory cap (systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/layout_tests.gpr"
"$here/build/layout_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/layout_proof.gpr" -j2 --level=2
fi
