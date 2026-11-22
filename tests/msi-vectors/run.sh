#!/usr/bin/env bash
#  Hosted tests for device interrupt vectors (docs/msi-interrupts.md);
#  --prove also runs gnatprove level 2 on the allocator, the ABI and the
#  MSI / MSI-X plans. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/msi_vectors.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/msi_vectors.gpr" \
        -u device_vectors.adb -u device_vector_abi.adb \
        -u pci_message_interrupts.adb \
        --level=2 -j4 --checks-as-errors=on
fi
