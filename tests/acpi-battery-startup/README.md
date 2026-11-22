# Startup battery collection on installed sources

Run `node tests/acpi-battery-startup/run.mjs` in the pinned Nix environment with
TMPDIR=/home/doc/cubit-build-tmp, stack 64 MiB and core dumps disabled. The runner
uses one compiler worker, nice 19, 180-second builds, 30-second executions,
5-second ACPICA commands and 1 MiB log bounds. Whole-Core builds use -gnatp.
Receipts and generated fixtures live under TMPDIR. ACPI_RESULT_DIR may select
an existing private receipt directory; ACPI_STARTUP_COLLECTOR_REUSE may select
the runner's existing private output for normal dependency-checked rebuilding.
All executions and reference comparisons rerun.

This focused test runs the actual collector, BIF/BIX readers, selector and raw
cache-page adapters on the installed Core plus the proposed startup battery
changes. It passes 345 checks at each AML width. It checks provenance, BIX-only
page applicability for BIF, one total budget, changed presence, discarded partial
data, foreign context, uncertainty/overcharge handling and no legacy fallback
after a malformed or uncertain BIX. Strict ACPICA comparison matches 66 typed
valid-package fields with repairs disabled. Cache diagnostics stay in the logs.

The source is derived from the existing 353-check provenance integration test;
eight CCL/client-schema checks are omitted deliberately because that separate
landing depends on the pending runtime-window foundation. Those original checks
and their complete future cohort remain preserved. No CCL runtime, hardware,
native event source, new SPARK proof or whole-service correctness is claimed.

Also run tests/acpi-battery-legacy/run.mjs and
tests/acpi-battery-selection/run.mjs for the independent reader/selector suites.
The legacy suite runs the pure decoder checked/release and the actual-Core
reader release-only; the selector uses release-only Core builds. Existing
acpi-battery/battery_observer_tests.adb is a separate dynamic _BST reader fixture
with a QWORD-only case; it is intended for AML64, not a cache-provenance test.
