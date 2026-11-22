#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
if [[ -z "${IN_NIX_SHELL:-}" ]]; then
    exec nix develop -c bash tests/retained-dma/run.sh
fi
dma_test_objects=$(mktemp -d /tmp/cubit-retained-dma-tests.XXXXXXXX)
gprbuild -p -P tests/retained-dma/tests.gpr -XDMA_TEST_OBJECTS="$dma_test_objects"
for test in retained_dma_budget_test dma_record_blocks_test dma_retirement_steps_test dma_lifecycle_integration_test dma_record_guards_test process_memory_budget_test process_memory_contention_test process_memory_accounts_test quota_width_test page_charge_handoff_test memory_account_store_test memory_identity_map_test owner_record_slabs_test; do
    "$dma_test_objects/$test"
done
if "$dma_test_objects/page_charge_handoff_test" --negative-bound-state >"$dma_test_objects/handoff-negative.log" 2>&1; then
    echo 'FAIL: duplicate-refund negative control unexpectedly passed' >&2
    exit 1
fi
grep -Fq 'MUTATION: attempting duplicate map-failure refund' "$dma_test_objects/handoff-negative.log"
grep -Fq 'ADA.ASSERTIONS.ASSERTION_ERROR' "$dma_test_objects/handoff-negative.log"
echo 'PASS page-charge duplicate-refund negative control'
printf 'Retained DMA hosted evidence: %s\n' "$dma_test_objects"
