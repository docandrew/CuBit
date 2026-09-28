#!/usr/bin/env bash
# Linux-hosted, shared pure parser only. Invoke through nix develop.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/acpi-tables/tables.gpr
../tests/acpi-tables/build/table_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/acpi-tables/tables.gpr \
        -u firmware_tables.adb --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/acpi-tables/build/proof.log
fi
