# Resource layout proof milestone

The actual DMA_Descriptors, Dependent_Groups and
Address_Spaces.Extended_Descriptors bodies discharge 206 checks/414 proof goals
plus 21 flow checks. All thirteen entities are in SPARK, without assumptions,
skips or unresolved messages. Per-unit checks/goals/flow: DMA 39/66/4,
dependent groups 101/194/6, Extended Address 66/154/11.

The scope is runtime safety, existing decoder extent contracts, initialization,
dependencies and dependent-group loop progress. Analyze additionally proves
error outputs and unused slots defaulted, caller-sized capacity, bounded spans,
item counts bounded by span length, and end-tag/group bounds. Its result must be
an unconstrained variable, as required by the explicit precondition. The grammar
uses one pass after the framing scan, with complete output reset on every error;
no external observer or callback sees partial output. There is no hidden output
allocation or smaller accepted grammar for the proof.

These contracts do not prove grammar completeness, exact field/source-to-layout
or checksum equivalence, resource selection, allocation, hardware authority,
native execution or whole AML correctness. Independent grammar/position tests
and retained normal ACPICA/Core replay provide separate functionality evidence.
Earlier descriptor and geometry/memory/register proofs remain separate on
unchanged sources.

Inside the pinned Nix shell, set TMPDIR=/home/doc/cubit-build-tmp before and
inside Nix, then run bounded phases separately:

    nice -n19 bash tests/acpi-resource-layout-proof/run.sh check
    nice -n19 bash tests/acpi-resource-layout-proof/run.sh flow
    nice -n19 bash tests/acpi-resource-layout-proof/run.sh prove
    bash tests/acpi-resource-layout-proof/check-result.sh "$PWD" "$RESULT_DIRECTORY" "$RECEIPT_JSON"

Level 1, VC timeout 5 seconds, one job, phase 180 seconds, stack 64 MiB and log
1 MiB. The closed project includes all local resource/FADT dependencies and
selects only these three bodies. Process exit status is not acceptance: the
checker verifies inputs/prover hashes, SPARK eligibility, no skips/assumptions,
resolved flow and a valid prover attempt for every check tree.

Retained accepted run: resource-layout-bounded-UyD1WFxC/source,
tests/acpi-resource-layout-proof/results/prove-wec7VtkC. The publication carries
identical Ada/GPR sources. This README, checker and proof receipt are metadata
added after that run. Original two-pass baseline prove-V5KVe4tN hit its unchanged
180-second phase limit with unresolved group obligations. Initial refinement
compiler diagnostics are preserved in private snapshots; none is accepted as
proof evidence, and no limit was raised.
