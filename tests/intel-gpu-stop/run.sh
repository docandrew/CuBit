#!/usr/bin/env bash
#  intel-gpu's Stop policy (PWR-003), Linux-hosted; --prove also proves it
#  (level 2, under a memory cap, -j2). Inside nix develop, TMPDIR outside /tmp.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/stop_policy.gpr"
"$here/build/stop_policy_tests"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/stop_policy.gpr" -u intel_gpu_stop_policy.adb \
        -j2 --level=2 --checks-as-errors=on --report=fail
fi
