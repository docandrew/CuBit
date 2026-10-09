#!/usr/bin/env bash
#  Hosted tests for Kernel_Credits (docs/ipc-delivery.md); --prove also runs
#  gnatprove (level 2). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/credits.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/credits.gpr" -u kernel_credits.adb \
        --level=2 -j4 --checks-as-errors=on
fi
