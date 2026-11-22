# Hosted non-debug AML BreakPoint checks

This standalone suite implements the ACPI non-debugger behavior of BreakPoint
as a charged Noop, following
[ACPI 6.5 §19.6.9](https://uefi.org/specs/ACPI/6.5/19_ASL_Reference.html#breakpoint-execution-break-point).
GNAT checked assertions do not enable an AML debugger.
Run `python3 tests/aml-breakpoint/run.py --mode release` (or `checked`) in the
repository's pinned Nix development environment with
`TMPDIR=/home/doc/cubit-build-tmp`. `--validate-only` checks source/reference
inventories without compilation. The opt-in central shortcut is
`python3 tests/acpi-hosted/run.py --group breakpoint --mode release`.
It does not add a service-milestone label or change that suite's 33 selections.

The exact fixture passed 209 checks in each profile: release worker 8964 and
checked worker 96822. This covers direct/nested/skipped/module behavior, namespace initialization/sealing,
and failure/cleanup behavior within the constructed fixture. Portable relocation passed all eight actual `--validate-only` commands; five
unsupported runners were inspected as metadata only and were not invoked. The
central `--group breakpoint` entry point then passed release worker 18634 and
checked worker 75622, each with 209 checks. The service suite retains its 33
selections; this is not an all-33 runtime claim. No broader regression, native, debugger or proof
coverage follows from this focused result.

## Cached normal ACPICA observations

`reference/normal` retains all eight original ASL tables, emitted AML, DSL,
24 raw compile/disassembly/execution logs, report and audit. Source/tool/runner
provenance and terminal audit are in `reference/provenance`; hosted receipts
are retained separately. These are immutable cached evidence, not regenerated
by this suite. Every reference file is pinned by `reference-manifest.json`;
no private path is used as a runtime fallback. Absolute paths inside historical
logs/receipts describe the original run only.

All eight normal compilations and disassemblies succeeded and every DSL retains
BreakPoint. Direct/nested cases in both revisions entered ACPICA's debugger,
reported debugger reentry errors and returned no TEST object. Skipped cases
returned TEST=102 and READ=0 twice. Both module executions timed out at five
seconds. Cache diagnostics are preserved. These are host-debugger-policy
observations, NOT eight scalar passes or CuBit equivalence goldens. No compiler
flags, AML bytes or logs were changed to force matching behavior.

Each build is bounded to 180 seconds; each runtime to 30 seconds, with 64 MiB
stack and 1 MiB log bounds. Actual compiler, ALI dependencies, tools and binary
hashes are audited. The existing runner's temporary GNAT configuration-file
retention limitation remains documented by its original hosted receipts.
Fatal and broader extended-operator behavior remain unfinished. This suite does
not deliver debugger notifications or platform fatal actions.
