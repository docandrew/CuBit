#!/usr/bin/env bash
#  Hosted tests for the kernel log ring (LOG-004); --prove also runs
#  gnatprove at level 2 on the shared layout and arithmetic, the reader's
#  drain step (through Drain_Instance, with opaque loads) and the typed
#  fault reports' decoding, under a 24 GB memory scope, two jobs. Run in
#  the Nix shell.
#
#  The native tests (tests/headless/run.sh --test ccl-console):
#    make -C kernel world
#    CCL_CONSOLE_DEMO=kernel-log bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 90
#  and the overflow (a kernel whose ring holds 64 records, so a boot
#  overflows it long before logstore starts; run.sh exports
#  KERNEL_LOG_RECORDS=64 for its own builds; rebuild normally afterwards):
#    KERNEL_LOG_RECORDS=64 make -C kernel world
#    KERNEL_LOG_OVERFLOW=1 CCL_CONSOLE_DEMO=kernel-log bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 90
#    make -C kernel world
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/kernel_log_ring_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/kernel_log_ring_tests.gpr" \
        -u kernel_log_publication.adb -u drain_instance.ads -u fault_reports.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail
fi
