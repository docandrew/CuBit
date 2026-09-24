#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p tests/config-inspection/build/source
cp userspace/runtime/gnat/cubit.ads userspace/runtime/gnat/cubit-config_inspection.ad? tests/config-inspection/build/source/
cp userspace/runtime/gnat/cubit-config_protocol.ad? tests/config-inspection/build/source/
cd kernel
alr exec -- gprbuild -P ../tests/config-inspection/inspection.gpr
../tests/config-inspection/build/main
alr exec -- gprbuild -p -P ../tests/config-inspection/tree_tests.gpr
../tests/config-inspection/build/tree/tree_tests
alr exec -- gprbuild -p -P ../tests/config-inspection/authority.gpr
../tests/config-inspection/build/authority/authority_tests
alr exec -- gprbuild -p -P ../tests/config-inspection/protocol.gpr
../tests/config-inspection/build/protocol/protocol_tests
alr exec -- gprbuild -p -P ../tests/config-inspection/store.gpr
../tests/config-inspection/build/store/store_tests
if [ "${1:-}" = "--prove" ]; then
    alr exec -- gnatprove -P ../tests/config-inspection/inspection.gpr -u cubit-config_inspection.adb --mode=prove --level=2 2>&1 | tee ../tests/config-inspection/build/proof.log
    if rg -q ': (low|medium|high):' ../tests/config-inspection/build/proof.log; then
        exit 1
    fi
    alr exec -- gnatprove -P ../tests/config-inspection/authority.gpr -u config_authority.adb --mode=prove --level=2 2>&1 | tee ../tests/config-inspection/build/authority-proof.log
    if rg -q ': (low|medium|high):' ../tests/config-inspection/build/authority-proof.log; then
        exit 1
    fi
    alr exec -- gnatprove -P ../tests/config-inspection/protocol.gpr -u cubit-config_protocol.adb --mode=prove --level=2 2>&1 | tee ../tests/config-inspection/build/protocol-proof.log
    if rg -q ': (low|medium|high):' ../tests/config-inspection/build/protocol-proof.log; then
        exit 1
    fi
    alr exec -- gnatprove -P ../tests/config-inspection/store.gpr -u config_store.adb --mode=prove --level=2 2>&1 | tee ../tests/config-inspection/build/store-proof.log
    if rg -q ': (low|medium|high):' ../tests/config-inspection/build/store-proof.log; then
        exit 1
    fi
fi
