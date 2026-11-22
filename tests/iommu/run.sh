#!/usr/bin/env bash
#  Hosted tests for IOMMU-001 (DMAR admission, I/O page tables, the walker
#  over a model store, the domain ledger); --prove also runs gnatprove
#  level 2 on the decoder, the page-table arithmetic, the walker instance
#  and the ledger. Run in the Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/iommu/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/iommu.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/iommu.gpr" \
        -u firmware_tables-remapping.adb -u io_page_tables.adb \
        -u iommu_domains.adb -u table_model.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail \
        2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "iommu: proof FAILED" >&2
        exit 1
    fi
    echo "iommu: proof OK"
fi
