#!/usr/bin/env bash
#  Hosted tests for Process_Identities (docs/process-objects.md, KERN-003);
#  --prove also runs gnatprove (level 2). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/process_identities_test.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/process_identities_test.gpr" -u process_identities.adb \
        --level=2 -j4 --checks-as-errors=on
fi
