# EC event replay policy proof

Run run.sh in Nix with TMPDIR=/home/doc/cubit-build-tmp. --source-root DIR
selects an exactly pinned source tree; --validate-only checks retained evidence
and sources without running provers. check-proof.mjs also works outside Nix.

The actual receiver uses ACPI_EC_Event_Policy for binding,revocation,claim,
callback guarding and settlement. Each level1/2 run has13all-SPARK entities,
11flow results and10proved checks,including5functional postconditions.
Retained trees contain96CVC5 and11Trivial Valid attempts per level,no unresolved
or skipped checks. Ten negative audit fixtures reject empty/non-SPARK/missing
postcondition/unproved/skipped evidence.

The contracts prove exact failure preservation,immutable controller keys,
greater-epoch replacement only after revoke/quiescence,authentication and
strictly increasing nonzero tokens before admission,exact token/number matching,
revoke preserving history,and sticky committed uncertainty. Scope-current and
pending/uncertainty observations must be truthful. The provisional callback
guard clears only through an explicit matching settlement operation; actual
receiver sequencing supplies that operation after normal return.

This is a modular proof of the actual pure policy,not a substitute Core model.
Kernel-origin authentication,namespace/current-scope checks,controller slot
allocation,callback truth/non-reentry,transport order,AML and native hardware
correctness remain separate obligations. Full receiver/Core proof still meets
the previously documented AML dependency/global-contract-generation diagnostic.
No compiler research,assertion weakening or whole-interpreter proof claim.

Proof runs are j1/nice19 with5-second solver and180-second command bounds,
64MiB stack and1MiB captured logs. The portable receiver integration regression
remains tests/acpi-ec-receiver,including ACPICA reference execution.
