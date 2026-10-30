# Composed field graph execution (hosted)

The normal 222-byte ACPICA 20260408 table compiles without errors, warnings or
remarks. Thirteen typed observations cover an IndexField with banked index/data,
a BankField with a banked selector, and a mixed Bank->Index->Bank->direct chain.
Only the four direct SEL0/IDX0/DAT0/MEM0 fields receive named leaf bindings.
IDX0/DAT0/MEM0 have separately installed named region catalog entries. Logical
fields acquire no implicit binding or raw-address authority.

The original ssVLtxBO baseline compiled/setup successfully but all three reads
returned Unsupported_Value without requests. That result is retained as a gap,
not counted as coverage. The new iterative router now matches all thirteen
observations; a combined leaf trace checks four/three/five requests respectively.
The stronger matrix adds before/after-effect unknown completion at every request
(24 cases), exact destination/direction/value receipts, completed-prefix values,
no replay, reconciliation and explicit subsequent invocation, plus deep permission
preflight and revocation after the first selector effect.

Actual field-graph-prefix-WQiEivOp runs pass 656 checks in each release/checked
profile. The earlier jwGSXXJP stronger test wrongly expected a completed mixed
write to disappear when the later read was uncertain. The corrected expectation
retains request4's write for either uncertainty outcome at request5. No production
change was made for that test correction. Original failing evidence is retained.

Run in the pinned Nix shell, TMPDIR=/home/doc/cubit-build-tmp:
`python3 tests/aml-field-graphs/run.py --mode release` (or `checked`).
`bash tests/aml-field-graphs/check_reference.sh` checks cached ordered observations,
exact embedded table extent and checksum. Raw ACPICA teardown diagnostics remain;
no clean teardown or physical bank-storage/hardware claim.

Routing uses bounded explicit frames and read-only graph preflight. The shared
provider request budget includes every nested selector and data dispatch. A
separate 65,536-step work budget also limits CPU-only graph validation/execution;
stack depth is bounded by namespace capacity, and active identity cycles reject.
These are implemented policies, not newly proved properties. Field/source graph
cycle/work exhaustion, wider/deeper graph combinations, Buffer/region lifetime,
protocol attributes, GlobalLock/concurrency and native service remain unfinished.
Full namespace/executor/I/O correctness is not proved by these tests.

A subsequent caller-work-policy checkpoint adds Maximum_Work, forwarded by
Service_Core with named Field_Work_Limit (1..1,048,576, default65,536). The actual
kKwkMac3 graph suite passes805 checks/profile, including one-step exhaustion with
no provider effects/uncertainty, a sufficient64-step allowance for the same
operations, and repeated explicit invocations with fresh counters. Loop variants
and a precise Charge postcondition run in the checked profile. These are checked
contracts, not a new accepted SPARK proof. Prior656 evidence remains scoped above.

The subsequent NACKhTp0 matrix also sweeps work limits1..64 for all three graphs.
It verifies the exact completed request prefix, every modeled register value,
no invented uncertain receipt, and no retained roots, requiring actual coverage
of zero-effect exhaustion, partial-prefix exhaustion and successful completion.
The checks do not depend on a specific internal step count for each operation.
Exact run results and counts are recorded in the enclosing handoff.
