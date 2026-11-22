#!/usr/bin/env bash
#  Hosted tests for CCL.Interfaces.Programs. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/ccl_programs.gpr"
"$here/build/main"
