#!/usr/bin/env bash
#  Hosted tests for Kernel_Controls (docs/ipc-delivery.md); --prove also runs
#  gnatprove (level 2). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/controls.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/controls.gpr" -u kernel_controls.adb \
        --level=2 -j4 --checks-as-errors=on
fi
