#!/usr/bin/env bash
#  Hosted tests of PWR-004 slice 1: the power-source contract's wire
#  (CuBit.Power_Sources), power.svc's read of acpi.svc (Power_ACPI_Reader,
#  Power_ACPI_Facts) against acpi.svc's own observer codecs
#  (fixture/Power_ACPI_Fixture), the ACPI -> Power_Source mapping
#  (Power_Source_Mapping) and the Deskbar's text (Desktop_Power_Text).
#  --mutate [--prove] also runs mutations.sh (each mutant must fail the
#  tests; with --prove, some the proof too). --prove also runs gnatprove
#  (level 2) over the contract, the facts, the
#  reader, the mapping and the text, under a memory cap (-j2).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
rm -rf "$links"
mkdir -p "$links"
#  CuBit.Power_Sources comes from runtime-shared (linked there).
for unit in userspace/services/power/power_acpi_facts.ads userspace/services/power/power_acpi_facts.adb \
            userspace/services/power/power_acpi_reader.ads userspace/services/power/power_acpi_reader.adb \
            userspace/services/power/power_source_mapping.ads userspace/services/power/power_source_mapping.adb \
            userspace/services/power/power_acpi_identity.ads userspace/services/power/power_acpi_identity.adb \
            userspace/services/power/power_acpi_sources.ads userspace/services/power/power_acpi_sources.adb \
            userspace/services/power/power_acpi_watch.ads userspace/services/power/power_acpi_watch.adb \
            userspace/services/desktop/desktop_power_text.ads userspace/services/desktop/desktop_power_text.adb \
            shared/acpi/acpi_protocol.ads shared/acpi/acpi_power_observation_protocol.ads \
            shared/acpi/acpi_battery_observation_protocol.ads \
            shared/acpi/acpi_power_watch_protocol.ads shared/acpi/acpi_power_identity_protocol.ads \
            userspace/services/acpi/power/acpi_power_inventory_watches.ads \
            userspace/services/acpi/power/acpi_power_inventory_watches.adb \
            userspace/services/acpi/power/acpi_source_paths.ads userspace/services/acpi/power/acpi_source_paths.adb \
            userspace/services/acpi/power/acpi_battery_status.ads userspace/services/acpi/power/acpi_battery_status.adb \
            userspace/services/acpi/power/acpi_observation_values.ads \
            userspace/services/acpi/power/acpi_cached_power_values.ads \
            userspace/services/acpi/power/acpi_cached_battery_values.ads \
            userspace/services/acpi/power/acpi_cached_power_codec.ads userspace/services/acpi/power/acpi_cached_power_codec.adb \
            userspace/services/acpi/power/acpi_cached_power_dispatch.ads userspace/services/acpi/power/acpi_cached_power_dispatch.adb \
            userspace/services/acpi/power/acpi_cached_battery_codec.ads userspace/services/acpi/power/acpi_cached_battery_codec.adb \
            userspace/services/acpi/power/acpi_cached_battery_dispatch.ads userspace/services/acpi/power/acpi_cached_battery_dispatch.adb; do
    ln -s "$root/$unit" "$links/"
done
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/power_sources_tests.gpr"
"$here/build/tests/power_sources_tests"
if [[ ${1:-} == --mutate ]]; then
    bash "$here/mutations.sh" "${2:-}"
fi
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
