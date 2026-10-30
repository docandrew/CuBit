# Controller-evaluated ACPI dependencies

Run in the pinned Nix development environment, exporting TMPDIR before Nix:

    bash tests/acpi-evaluated-dependencies/run.sh --output-root <private-directory>

Select ACPICA tools through ACPICA_BIN or PATH. The runner snapshots the explicit
source manifest, pins tool/input hashes, compiles fresh AML revisions 1 and 2,
checks exact ACPICA device-reference order and HITS counter, and builds/runs
checked (-gnata -gnato) and release (-gnatp) hosted profiles. Saved logs and JSON
remain in a unique output directory. No native/kernel/image/QEMU build runs.
Limits: one compiler job, 180s build, 30s runtime, 5s ACPICA, 64MiB stack,
no core dumps, 1MiB captured output. Invoke nice -n 19 for shared-host courtesy.

ACPI_Device_Dependencies is an in-process trusted-controller API. It evaluates
only a selected device's direct canonical _DEP once, retaining a Package root
until explicit Close. Copy retries for quota/capacity do not re-evaluate AML.
ACPI_Dependency_Reader borrows that root, preserves reference order/duplicates,
accepts only authenticated live Device metadata, releases temporary child roots,
and returns no partial output on failure. No conversion, recursion or scheduling.

Coverage: passive queries never execute methods; one evaluation across repeated
copies, high array bounds, exact capacity/work refusals, wrong-context/busy/closed
actions, static aliases, missing versus empty, wrong integer/string results,
execution failure, finite/zero operation budgets, malformed late elements,
provider overcharge diagnostics, foreign/non-device descriptors, no ancestor
fallback, borrowed wrong/stale handles, and caller/session root pressure.
148 checks per process, 296 per profile across both AML revisions.

Caller requirements: controller authority and hardware provider readiness already
established; no Core reset, catalog replacement or AML mutation between pending
session calls; keep sessions within their Core generation. Retained roots do not
freeze mutable firmware objects. Method effects are not rolled back. Device
dependencies are descriptive ordering hints, not proof that OperationRegions
are accessible; _REG/provider readiness remain separate.

Evidence is hosted regression testing, with ACPICA comparison for the successful
method result/order/counter only. There is no new SPARK proof, actual syscall,
native boot, hardware, dependency-scheduler or complete AML correctness claim.
No CCL operation, capability, launcher policy or native event loop is added.
