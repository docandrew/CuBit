#!/usr/bin/env bash
#  Hosted tests for the per-CPU times publication and the load arithmetic;
#  --prove also runs gnatprove at level 2 on the shared unit, the reader
#  generic (through Sample_Instance), the seqlock model and CuBit.CPU_Load
#  (under a 24 GB memory scope, two jobs). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/cpu_times_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/cpu_times_tests.gpr" \
        -u cpu_times_publication.ads -u seqlock_model.adb -u sample_instance.ads \
        -u cubit-cpu_load.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail
fi
