#!/usr/bin/env bash
#  Hosted tests for Grant_Windows (docs/process-objects.md, KERN-003 step 2b);
#  --prove also runs gnatprove (level 2). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/grant_windows_test.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/grant_windows_test.gpr" -u grant_windows.adb \
        --level=2 -j4 --checks-as-errors=on
fi
