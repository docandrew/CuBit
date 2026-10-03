#!/usr/bin/env bash
# Linux-hosted tests of the shared service controller, not native IPC/storage.
# Invoke via nix develop -c bash tests/config-activation/run.sh [--prove].
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p tests/config-activation/build/source
cp userspace/runtime/gnat/cubit.ads userspace/runtime/gnat/cubit-config_inspection.ad? userspace/runtime/gnat/cubit-failures.ad? tests/config-activation/build/source/
cd kernel
alr exec -- gprbuild -p -P ../tests/config-activation/activation.gpr
../tests/config-activation/build/activation_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/config-activation/activation.gpr \
        -u config_activation.adb --level=2 -j2 \
        2>&1 | tee ../tests/config-activation/build/proof.log
    if rg -q ': (low|medium|high):' ../tests/config-activation/build/proof.log; then
        exit 1
    fi
fi
