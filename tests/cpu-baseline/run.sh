#!/usr/bin/env bash
#  The kernel's x86-64-v3 boot check (CPU_Baseline) over CPUID words of
#  real CPU models; --prove also proves it (level 2; run under a memory cap:
#  systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/test.gpr"
"$here/build/cpu_baseline_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2
fi
