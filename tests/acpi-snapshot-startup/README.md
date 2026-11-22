# Direct local snapshot startup

Run `bash tests/acpi-snapshot-startup/run.sh --mode checked` or `--mode release`
inside the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp. Default release;
`--validate-only` audits input hashes and closed source inventory without builds.

Actual Requests/Core/AML executes local startup with modeled immutable snapshot
reads and clock.513checks cover exact descriptor-sized allocation beyond64KiB,
full-width input refusal, descriptor/aggregate inconsistencies, bounded copies,
all short-copy positions, checksum/revision failure, malformed AML, every clock
boundary, no published partial arena and adapter exception cleanup.

142additional checks cover 32/64-bit module/root/bus/device Fatal, ordinary
initializer failure and inaccessible-region cases. Copied Fatal reports are
compared with direct Core execution and survive owner teardown and overwritten
source bytes. Snapshot completion is distinct from successful device readiness.
The native entry attempts one Critical record from this copied report; logger
delivery and native startup behavior are not established by these pure tests.

88strictsourceALIs/profile; checked gnata+gnato vs release gnatp. Build180s,
each runtime30s, stack64MiB, logs1MiB, nice19/j1. Inputs, closed Ada inventory,
compiler tools and artifacts are pinned. No new proof/ACPICA/native build claim.
CuBit's real allocator exits on OOM; no claim of hosted Storage_Error recovery
on native failure. Allocation failure is not forced by this suite.

A third main supplies401 formatting checks for typed startup freeze/preparation/module/member outcomes and bounded log length. The malformed-AML case verifies the failed table is the DSDT, rather than the last copied OEM table, and the diagnostic survives candidate cleanup. Native entry uses the same formatter once on Startup_Refused. It does not invent an opcode offset or claim delivery. The source pin for region initialization is rebased to the already-present current source; its old mismatch is recorded in the checkpoint closure issues.

The device-format matrix covers all device phases, region outcomes, attempted
markers and error counts, plus Fatal and maximum numeric values. Actual startup
checks the copied device outcome and preservation of the revision. The two
profiles execute1172checks each. Documentation counts were updated after the
functional run; no executable test or production source changed in that update.


The controlled module-startup milestone is recorded in MODULE-STARTUP-EVIDENCE.json.
Its current-source input/closure refresh does not renew historical proof results.
Fresh hosted checks cover both profiles; no native execution or hardware
authority/region connection follows from these tests. The native API gate
selects 59 units and produces 56 ALIs with zero objects per profile.
