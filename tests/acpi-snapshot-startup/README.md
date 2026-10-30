# Direct local snapshot startup

Run `bash tests/acpi-snapshot-startup/run.sh --mode checked` or `--mode release`
inside the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp. Default release;
`--validate-only` audits input hashes and closed source inventory without builds.

Actual Requests/Core/AML executes local startup with modeled immutable snapshot
reads and clock.508checks cover exact descriptor-sized allocation beyond64KiB,
full-width input refusal, descriptor/aggregate inconsistencies, bounded copies,
all short-copy positions, checksum/revision failure, malformed AML, every clock
boundary, no published partial arena and adapter exception cleanup.

142additional checks cover 32/64-bit module/root/bus/device Fatal, ordinary
initializer failure and inaccessible-region cases. Copied Fatal reports are
compared with direct Core execution and survive owner teardown and overwritten
source bytes. Snapshot completion is distinct from successful device readiness.
The native entry attempts one Critical record from this copied report; logger
delivery and native startup behavior are not established by these pure tests.

86strictsourceALIs/profile; checked gnata+gnato vs release gnatp. Build180s,
each runtime30s, stack64MiB, logs1MiB, nice19/j1. Inputs, closed Ada inventory,
compiler tools and artifacts are pinned. No new proof/ACPICA/native build claim.
CuBit's real allocator exits on OOM; no claim of hosted Storage_Error recovery
on native failure. Allocation failure is not forced by this suite.
