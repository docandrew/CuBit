# Passive device dependencies

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-passive-dependencies/run.sh
```

ACPICA must be on PATH, or select its tool directory with `ACPICA_BIN`. Artifacts
go under TMPDIR or `--output-root PATH`. Each run uses disjoint input snapshots,
checked assertions/overflow checks and release builds, AML revisions1/2, and
fresh iASL/acpiexec observations. Builds use one job and a180second limit,
runtimes30seconds, ACPICA5seconds, logs1MiB, stack64MiB and no core dumps.

ACPI_Passive_Dependencies observes only a canonical direct static `_DEP` package.
It returns live device descriptors, preserving order/duplicates and self/cyclic
edges. It reports method-valued data without invocation, distinguishes missing
and empty lists, and refuses malformed elements/quota/capacity failures without
partial output. This reader does not recursively follow dependencies, assert
provider readiness or grant hardware authority. The owner supplies an explicit
element work limit and must serialize access/recheck descriptor freshness.

Tests cover actual Core package/device references, forward references, both
device and member aliases, high output bounds, no ancestor fallback, foreign
descriptors and root pressure while preserving an unrelated retained result.
ACPICA independently verifies ordered device references, package sizes and the
method side-effect counter. Invalid aliased data are constructed rejection
fixtures, not claims that those forms are valid `_DEP` definitions. ACPICA's
exit-zero cache-allocation notes remain retained.

This is SPARK-mode code with assertion-enabled hosted tests, not a new accepted
proof milestone or native/hardware validation. CCL delivery, graph scheduling,
provider activation and device initialization remain separate integration work.
`_DEP` supplies start-order hints; `_REG` is the region-availability mechanism:
[ACPI device dependencies](https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#dep-device-dependencies).
