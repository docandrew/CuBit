# Dynamic table-load reference baseline

Run bash tests/aml-dynamic-load-reference/run.sh inside pinned Nix with TMPDIR
set to /home/doc/cubit-build-tmp before and inside the shell. --validate-only
checks cached artifacts/source/log semantics and executes no compiler or AML.
The runner creates a fresh private result directory and preserves old outputs.
It pins iASL/acpiexec and its own source,uses normal -oa -r1/-r2 compilation
and repair-disabled execution,and refuses changed inputs or failed commands.
Each command has10s/1MiBlog limits,nice19,64MiBstack,core0.

Six commands compile and execute self-authored DSDT/SSDT pairs in both widths.
The62byte dynamic SSDT's module counter changes0to1. Load and its target are
nonzero Integers(FFFFFFFF in32bit,FFFFFFFFFFFFFFFF in64bit). The loaded method
returns42,source-buffer method literal changes to99,and loaded method remains42.
Missing LoadTable returns0 without another module effect. Four outstanding
cache allocations remain in each raw interpreter log; successful values are not
a clean-teardown claim. Portable reproduction passed all the same observations.

Cached validation checks headers/checksums,width inventory,original source and
script linkage,command statuses,and reparses integer results from raw logs.
Four isolated evidence negatives reject changed log values,missing source
linkage,failed result flags and changed runner. Raw results are in reference/;
full validation is in VALIDATED.json.

This establishes reference semantics,not CuBit Load/LoadTable support,a SPARK
proof,native execution or a full failure/lifetime/concurrency matrix. These
self-authored fixtures are not additional original ACPICA ASLTS entrypoints.
Unload was not executed. See docs/acpi-dynamic-tables.md for modern Boolean
semantics,deprecated Unload compatibility and the actual Core integration gap.
