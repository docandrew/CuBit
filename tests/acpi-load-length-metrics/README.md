# Startup Load diagnostics through observer metrics

Run `TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/acpi-load-length-metrics/run.sh` from the repository root. `--validate-only` checks the pinned source manifest without building.

The gate uploads valid copied tables through the actual request core, completes startup, and queries observer metrics. Eight scenarios in both AML integer widths cover table-Field and Buffer header errors (wire43), missing Buffer/Field body (37), valid table source with the default zero copied quota (26), and successful initialization (default error field4 qualified by completion state). Both root and system-bus initialization are counted. Observer mutation, unauthorized reads, revision changes and startup replay are refused; observer reads preserve the complete ghost model in checked mode.

Expected: 736 checks in 32 processes, both checked and release profiles. Fixtures are compiled with pinned IASL and must match the recorded AML hashes. Wrapper bytes are stored as pinned text hex. This tests local service packets and startup logic, not actual CCL/kernel transport, native scheduling or hardware. It adds no new SPARK proof.
