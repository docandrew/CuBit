# Idle-owner module append tests

Run bash tests/aml-ready-module-append/run.sh inside pinned Nix with TMPDIR set
to /home/doc/cubit-build-tmp before and inside the shell. --validate-only checks
source selections and runs no compiler or AML. Outputs use fresh private paths.

Normal iASL compiles DSDT/OEM1 pairs in both integer widths. Two real Ada service
builds run five cases per width/profile: successful append,division-by-zero,
Fatal,zero-budget admission and invalid index. Each case also checks checksum,
short-header,boot DSDT and notification-capture refusal. Twenty runtime processes
passed848checks in the historical standalone run. The current cleanup change is
covered by the broader module-context matrix (10248checks across both profiles
and AML widths), not a fresh standalone run. Existing retained buffers survive
success. After terminal module failure, authenticated results can be released
while idle; retained-result reads and further AML execution remain closed.
Existing diagnostic observations remain available without executing more AML.
Consumed attempts cannot replay on another arena or after Reset.

This uses actual ACPI_Service,not a Core substitute. The operation remains an
internal idle-owner prerequisite. It does not implement Load/LoadTable,nested
execution,table-load ownership or full cleanup/readiness policy. Tests are hosted
functional evidence,not SPARK proof,native execution or hardware validation.

See docs/acpi-dynamic-tables.md and VALIDATED.json for scope and exact evidence.
