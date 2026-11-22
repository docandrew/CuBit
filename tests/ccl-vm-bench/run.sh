#!/usr/bin/env bash
#  VM dispatch-loop benchmark (hosted). Run in the Nix shell on a quiet host:
#  bash tests/ccl-vm-bench/run.sh [rounds]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/ccl_vm_bench.gpr"
"$here/build/vm_bench" "${1:-5}"
