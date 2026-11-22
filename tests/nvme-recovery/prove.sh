#!/usr/bin/env bash
#  SPARK proof of nvme.drv's pure parts (BUG-003): the command identifier
#  tracker, the queue cursor arithmetic and the recovery state machine.
#  Run under the memory cap (coordination rules): systemd-run --user --scope
#  -p MemoryMax=24G -p MemorySwapMax=0 bash tests/nvme-recovery/prove.sh
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gnatprove -P "$test_dir/nvme_recovery_tests.gpr" \
    -u nvme_protocol.ads nvme_layout.ads nvme_queue_cursors.adb \
       nvme_command_ids.adb nvme_recovery.adb \
    --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2 "$@"
