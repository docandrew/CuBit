# Hosted ACPI passive queries

Run inside the Nix development environment with ACPICA on PATH:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-passive-query/run.sh
# At integration milestones:
nix develop --command bash tests/acpi-passive-query/run.sh --proof
```

If ACPICA is installed separately, set `ACPICA_BIN` to the directory containing
`iasl` and `acpiexec`. The runner records the resolved tool paths and hashes.

`--proof-only` selects the concrete loan/session/pump proof without rebuilding
the AML tests. `--output-root PATH` keeps all artifacts in a chosen existing
directory. Every run snapshots its inputs and uses disjoint outputs, bounded
logs, one compiler/prover job, 180-second build/proof limits and 30-second runtime
limits. No kernel/image/native executable/QEMU build or syscall is run.

The default suite compiles the real AML-backed passive reader, ID/ADR search,
Describe and Resources handlers, unified dispatch, loan/session/event-pump code
and native byte-copy edge. Checked assertions/overflow checks and release builds
each run AML revisions 1 and 2. Fresh ACPICA observations check static fixture
facts; expected fixture diagnostics and exit-zero shutdown cache notes remain
captured. Constructed resource packets and mocked grant/reply/poll callbacks are
not independent ACPICA or live CuBit tests. Three native syscall-wrapper units
are compile-checked against the actual runtime interfaces, never executed.

Coverage includes complete CIDs and UID/path paging, explicit STA defaults,
descriptor/subentry ordering, source spans, malformed templates/cursors, roles,
opaque identities, exact copy extents, no passive AML execution/root leaks,
failed acquisition/copy/return/delivery, saved reply retention and cleanup retry
deadlines. The shared native service entry point is not changed by this suite.

The optional level-2 proof instantiates actual generic policies with unrestricted
flags/metadata and arbitrary byte prefixes zero-extended to any admitted extent.
It audits nonzero discharged checks and zero unproved/justified obligations or
pragma Assume. Twelve model-only callback-mode warnings are retained because
the generic API requires IN OUT while the model only reads context. This proves
reported safety/state contracts under model and dependency contracts; it does
not prove complete functional/liveness/call-order semantics, codec bodies, AML,
native memory/kernel/clock/IRQ behavior or the complete service. Native adapters
remain explicitly trusted SPARK_Mode Off boundaries.
