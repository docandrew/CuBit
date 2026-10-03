#!/usr/bin/env bash
# Hosted only; use nix develop -c bash tests/aml-core/run.sh [--prove] [--acpica].
set -euo pipefail
# Hosted checked builds copy bounded snapshot models for runtime contracts.
# This explicit test budget is not a native whole-call-chain stack proof.
ulimit -S -s 65536
prove=no
acpica=no
for option in "$@"; do
    case "$option" in
        --prove) prove=yes ;;
        --acpica) acpica=yes ;;
        *) echo "Unknown option: $option" >&2; exit 2 ;;
    esac
done
cd "$(dirname "$0")/../../kernel"
python3 ../tests/aml-core/integer_oracle.py
alr exec -- gprbuild -p -P ../tests/aml-core/provisioning/provisioning.gpr
../tests/aml-core/build/provisioning/provisioning_tests
alr exec -- gprbuild -p -P ../tests/aml-core/snapshots/snapshots.gpr
../tests/aml-core/build/snapshots/snapshot_tests
alr exec -- gprbuild -p -P ../tests/aml-core/aml.gpr
alr exec -- gprbuild -p -P ../tests/aml-core/backing.gpr
python3 ../tests/aml-core/native-loop/prepare.py
alr exec -- gprbuild -p -P ../tests/aml-core/native-loop/acpi_loop.gpr
../tests/aml-core/build/native-loop/loop_tests
alr exec -- gprbuild -p -P ../tests/aml-core/native-blocks/blocks.gpr
../tests/aml-core/build/blocks/block_tests
alr exec -- gprbuild -p -P ../tests/aml-core/transactions/transactions.gpr
../tests/aml-core/build/transactions/transaction_tests
python3 ../tests/aml-core/native-endpoint/prepare.py
alr exec -- gprbuild -p -P ../tests/aml-core/native-endpoint/endpoint.gpr
../tests/aml-core/build/native-endpoint/endpoint_tests
alr exec -- gprbuild -p -P ../tests/aml-core/hardware-cspace/hardware_cspace.gpr
../tests/aml-core/build/hardware-cspace/hardware_cspace_tests
alr exec -- gprbuild -p -P ../tests/aml-core/hardware-grants/grants.gpr
../tests/aml-core/build/hardware-grants/hardware_grants_tests
alr exec -- gprbuild -p -P ../tests/aml-core/hardware-catalog/catalog.gpr
../tests/aml-core/build/hardware-catalog/hardware_catalog_tests
alr exec -- gprbuild -p -P ../tests/aml-core/hardware-capabilities/capabilities.gpr
../tests/aml-core/build/hardware-capabilities/capability_tests
alr exec -- gprbuild -p -P ../tests/aml-core/regions/regions.gpr
../tests/aml-core/build/regions/region_tests
../tests/aml-core/build/backing/backing_tests
../tests/aml-core/build/catalog_tests
../tests/aml-core/build/exposure_tests
../tests/aml-core/build/decode_tests
../tests/aml-core/build/field_tests
../tests/aml-core/build/field_data_tests
../tests/aml-core/build/name_tests
../tests/aml-core/build/namespace_tests
../tests/aml-core/build/namespace_field_tests
../tests/aml-core/build/resolve_tests
../tests/aml-core/build/load_tests
../tests/aml-core/build/string_tests
../tests/aml-core/build/buffer_tests
../tests/aml-core/build/execute_tests
../tests/aml-core/build/readonly_input_tests
../tests/aml-core/build/service_field_execution_tests
../tests/aml-core/build/field_declaration_tests
../tests/aml-core/build/field_boundary_tests
../tests/aml-core/build/literal_value_tests
../tests/aml-core/build/selection_tests
../tests/aml-core/build/string_conversion_tests
../tests/aml-core/build/region_executor_tests
../tests/aml-core/build/region_service_tests
../tests/aml-core/build/clock_tests
../tests/aml-core/build/timer_executor_tests
../tests/aml-core/build/timer_clock_tests
../tests/aml-core/build/method_tests
../tests/aml-core/build/method_storage_tests
../tests/aml-core/build/expression_tests
../tests/aml-core/build/service_tests
../tests/aml-core/build/integer_oracle_tests
../tests/aml-core/build/branch_tests
../tests/aml-core/build/loop_tests
../tests/aml-core/build/logic_tests
../tests/aml-core/build/binding_tests
../tests/aml-core/build/call_tests
../tests/aml-core/build/object_tests
../tests/aml-core/build/buffer_access_tests
../tests/aml-core/build/byte_reference_tests
../tests/aml-core/build/package_reference_tests
../tests/aml-core/build/data_tests
../tests/aml-core/build/package_count_tests
../tests/aml-core/build/inspect_tests
../tests/aml-core/build/division_tests
../tests/aml-core/build/coercion_tests
../tests/aml-core/build/bootstrap_tests
../tests/aml-core/build/typed_tests
../tests/aml-core/build/store_tests
../tests/aml-core/build/request_tests
../tests/aml-core/build/endpoint_tests
if [[ $prove == yes ]]; then
    alr exec -- gnatprove -P ../tests/aml-core/provisioning/provisioning.gpr \
        -u firmware_tables-provisioning.adb --mode=all --level=2 \
        --timeout=30 --memlimit=2000 --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/provisioning-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/snapshots/snapshots.gpr \
        -u firmware_tables-snapshots.adb --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/snapshot-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/native-loop/acpi_loop.gpr \
        -u acpi_launch.adb --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/launch-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/hardware-cspace/hardware_cspace.gpr \
        -u hardware_grants-cspace.adb --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/hardware-cspace-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/hardware-grants/grants.gpr \
        -u hardware_grants.adb hardware_catalog.adb hardware_authority.ads --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/hardware-grants-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/hardware-catalog/catalog.gpr \
        -u hardware_catalog.adb hardware_authority.ads --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/hardware-catalog-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/hardware-capabilities/capabilities.gpr \
        -u capabilities.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/hardware-capabilities-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/regions/regions.gpr \
        -u hardware_authority.ads acpi_region_policy.adb region_mock.adb region_io_instance.ads --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/regions-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/transactions/transactions.gpr \
        -u acpi_fadt-transactions.adb --mode=all --level=2 \
        --timeout=30 --steps=0 --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/transactions-proof.log
    alr exec -- gnatprove -P ../tests/aml-core/aml.gpr \
        -u aml_coercions-strings.adb aml_table_backing.adb firmware_tables-identifiers.adb firmware_tables-copies.adb firmware_tables-exposure.adb firmware_tables-catalog.adb acpi_fadt-registers.adb acpi_fadt.adb aml_decode.adb aml_fields.adb aml_field_data.adb aml_names.adb aml_execute.adb aml_integers.adb aml_coercions.adb aml_logic.adb aml_objects.adb aml_objects-byte_references.adb aml_objects-package_references.adb aml_data.adb namespace_instance.ads acpi_service.ads aml_clock.adb timer_verification.adb timer_service_verification.adb acpi_bootstrap.adb acpi_requests.adb acpi_endpoint.adb --mode=all --level=2 \
        --timeout=30 --memlimit=2000 --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/proof.log
    alr exec -- gnatprove -P ../tests/aml-core/backing.gpr \
        -u multiboot_memory_map-reclaim.adb --mode=all --level=2 \
        --prover=cvc5,z3 --checks-as-errors=on -j2 \
        2>&1 | tee ../tests/aml-core/build/backing-proof.log
fi

if [[ $acpica == yes ]]; then
    bash ../tests/aml-core/run-acpica.sh
fi
