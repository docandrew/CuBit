#!/usr/bin/env bash
# Invoke inside nix develop. No native artifacts or global installs.
set -euo pipefail
# Hosted checked builds copy bounded snapshot models for runtime contracts.
# This explicit test budget is not a native whole-call-chain stack proof.
ulimit -S -s 65536
cd "$(dirname "$0")/../.."
tools=$(nix build --no-link --print-out-paths --impure --expr \
    '(builtins.getFlake (toString ./.)).inputs.nixpkgs.legacyPackages.${builtins.currentSystem}.acpica-tools')
(
    cd kernel
    alr exec -- gprbuild -p -P ../tests/aml-core/aml.gpr table_runner.adb service_field_runner.adb table_find_runner.adb conversion_runner.adb field_runner.adb field_data_runner.adb
)
python3 tests/aml-core/acpica_field_data.py --tools "$tools/bin"
python3 tests/aml-core/acpica_service_fields.py --tools "$tools/bin"
python3 tests/aml-core/acpica_fields.py --tools "$tools/bin"
python3 tests/aml-core/acpica_fadt.py --tools "$tools/bin"
python3 tests/aml-core/fwts_tables.py
python3 tests/aml-core/acpica_table_find.py --tools "$tools/bin"
python3 tests/aml-core/acpica_compare.py --tools "$tools/bin"
python3 tests/aml-core/acpica_packages.py --tools "$tools/bin"
python3 tests/aml-core/acpica_package_counts.py --tools "$tools/bin"
python3 tests/aml-core/acpica_coercions.py --tools "$tools/bin"
python3 tests/aml-core/acpica_typed.py --tools "$tools/bin"
python3 tests/aml-core/acpica_literals.py --tools "$tools/bin"
python3 tests/aml-core/acpica_string_conversions.py --tools "$tools/bin"
python3 tests/aml-core/acpica_datatable_regions.py --tools "$tools/bin"
python3 tests/aml-core/acpica_store.py --tools "$tools/bin"
python3 tests/aml-core/acpica_named_store.py --tools "$tools/bin"
python3 tests/aml-core/acpica_serialized.py --tools "$tools/bin"
python3 tests/aml-core/acpica_dynamic_methods.py --tools "$tools/bin"
python3 tests/aml-core/acpica_methods.py --tools "$tools/bin"
python3 tests/aml-core/acpica_upstream.py --tools "$tools/bin" "$@"
