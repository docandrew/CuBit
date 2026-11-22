# Status-checked lid and AC-source observations

Sample_Ready in the Bootstrap/Requests Lids and Power_Sources owners composes
first-match identity search,direct STA observation,and fixed LID/PSR query on the
same private Core/evaluator. All phases share one AML budget. Device startup
must already be complete. Absent,disabled or nonfunctioning flags prevent the
member query; failed/wrong-type/exhausted STA also prevents it. Unknown remains
unknown,never closed/offline. Reports retain the exact search,status and query
outcomes and accepted charge. Cleanup/provider uncertainty requires the owner
to stop further AML/reset; no retry or event ACK is performed here.

These are ordered point observations,not an atomic status snapshot,live cache,
full dependency/readiness guarantee or hardware authority. Only the first match
is sampled. Existing raw query/cache APIs retain their caller-provided status
policy; this new checked path is intended for native power bring-up. Native
lid/AC/battery startup wiring and notification/cache publication remain open.

ACPI_Device_Status owns the common direct STA implementation extracted from
HID,whose public report remains unchanged via an explicit enum/field mapping.
Named/aliased STA values,full64flags,defaulting and result cleanup are retained.
Missing STA keeps the existing uint32 all-ones default,as in
[ACPICA AcpiUtExecute_STA](https://raw.githubusercontent.com/acpica/acpica/master/source/components/utilities/uteval.c).
No existing default policy was changed.

Hosted checked/release each274sampling checks,24fresh processes/profile,
32/64bit AML,68commands; normal iASL compilation,no forced compile. Covers
post-init absent/disabled/broken/wrong-type status,default/empty/unready/no-INI,
zero/tight budget,and actual fixed-member invocation counters. One-short budgets
ensure status fuel is deducted before the member query. Native HID312checks and
same-Core HID owner382checks/profile also pass against the shared body.
Actual-runtime semantics25selections/24ALIs/305mirrors,zeroobjects per profile.
Startup's source inventory is refreshed only; no new startup runtime pass.

New ACPICA repair-disabled raw STA references match ready[15,15],absent[0,0]
and no-INI[0,0] after initialization. Each reports the known4cache-allocation
exit diagnostic; qualified reference,not a clean tool log or proof of our gate.
Initial ordinary Ada visibility/warning failures and fixture-assumption failure
are retained in private evidence. No compiler-crash investigation,new SPARK
proof,native linking/boot,hardware or full-service correctness is claimed.

Run run.sh in pinned Nix with TMPDIR=/home/doc/cubit-build-tmp before and inside
Nix. --validate-only checks source hashes only. Builds180s,processes30s,
stack64MiB,logs1MiB,nice19/j1. Source/tool/profile/artifact hashes are audited.
