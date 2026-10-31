# Hosted Field geometry and selected proofs

Run `python3 tests/aml-field-geometry/run.py --mode release` or `--mode checked`
inside the pinned Nix environment. Use the required TMPDIR before entering Nix.
The fixtures perform 162,373 datum checks and 686 read-plan checks.
They exercise admission, widths, extraction/update masks and boundary values.
The runner records exact compiler inputs, flags, runtime and ALI dependencies.
`--validate-only` verifies registration without compiling anything.

For each selection (`datums`, `plans` or `alignment`), run these phases separately and inspect
each result before advancing:

```sh
nice -n19 bash tests/aml-field-geometry/prove.sh datums check
nice -n19 bash tests/aml-field-geometry/prove.sh datums flow
nice -n19 bash tests/aml-field-geometry/prove.sh datums prove
```

Repeat with `plans` and `alignment`. The latter selects only Octets/Aligned
at the recorded source region, not the entire FADT transactions unit.
No retry or automatic limit increase occurs. Each phase
has a 180-second limit, 64 MiB stack and 1 MiB log; proof uses one worker and
five seconds per prover attempt. The runtime fixture retains 180-second build
and 30-second execution limits. Use at most two heavy hosted workers.

The selected datum body has 58 proof checks; the read-plan body has eight.
The alignment dependency selection has three.
They cover runtime safety, existing postconditions, the private plan invariant
and explicit overlap bounds. Acceptance checks actual proof trees, not just
the process exit status. Global-effect/dependency closure is separately reviewed.
Exact Octets/Aligned safety/effects and standard Ada/Shift_Left arithmetic models
are dependencies. These results are not complete functional proofs of every
returned mask, offset, admission decision, AML operation or native hardware path.

`EVIDENCE.json` preserves historical source and receipt scopes. Current runs
compile and prove repository sources directly; there is no private-file fallback.

The Memory Qword functionality successor passed 163,059 checks per profile.
The datum helper body is unchanged. Older receipts in EVIDENCE.json retain
their historical scope. The fresh plan checkpoint below records acceptance
of the current Qword-capable plan body separately.

## Current EC Field geometry proof checkpoint

The current layout and plan helpers have fresh level-1 and level-2 runs. At
each level all12selected entities are in SPARK,15flow results are informational
and all15safety/contract checks prove,including3postconditions. CVC5 alone
timed out on one plan postcondition; the existing CVC5/Z3 pair succeeded with
the same five-second solver limit. No contract was weakened or assumed.

Run `bash tests/aml-field-geometry/check_plan_proof.sh` to validate current
source hashes, retained complete result trees and exact proof commands without
executing a prover. The schema-2 CURRENT-PLAN-PROOF.json pins14explicit source
files and the retained evidence under reference/ec-fields/. Original private
project paths remain provenance; retained logs are not a portable build recipe.
Use the existing `prove.sh plans prove` for a fresh selected plan proof, or a
fresh equivalent project selecting AML_Field_Layout at levels1/2.

The original Qword plan record and checker remain under reference/current-plans/
as original-current-plan-proof.json and original-check-plan-proof.sh. Historical
evidence hashes are preserved rather than relabeled as current EC proof.

These prove helper runtime safety and their stated contracts,not complete
functional correctness of every admission decision,mask or offset. Dependency
contracts and standard runtime arithmetic models remain assumptions or separate
historical evidence. The interpreter,remote provider,banked I/O and native
hardware are not proved by these selected runs.
