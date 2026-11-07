#!/usr/bin/env bash
#  Hosted tests for CuBit.Identity_Tables (procmgr's process and launch
#  tables). Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/identity_tables_test.gpr"
"$here/build/main"
