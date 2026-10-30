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

## Current selected plan proof checkpoint

The current Qword-capable `AML_Field_Plans` body now has a fresh level-1 run:
all 10 selected entities are in SPARK, 11 flow results are informational and all
8 safety/contract obligations have successful prover trees. No source, contract,
acceptance count or prover limit changed to obtain this result.

Run `bash tests/aml-field-geometry/check_plan_proof.sh` to check the retained
record without executing a prover. `CURRENT-PLAN-PROOF.json` pins all 14 explicit
GPR source files plus the GPR, recipe and tool-pin file, and the retained result
snapshots. Original source/output paths remain provenance. Only selected logs,
commands, audits and complete result trees are retained here; the original
additional artifacts remain at their recorded output. The checker does not
assert that omitted artifacts are present or require the old compiler installation.

The eight obligations cover three range checks, one division check, two call
preconditions and two existing postconditions. This is not a complete functional
proof of masks, offsets, admission or every return value. Dependency contracts
and standard runtime arithmetic models remain assumptions or separate historical
evidence. Namespace, executor, banked I/O and native correctness are not proved
by this helper run. The reproduction recipe remains `plans check`, then inspect,
`plans flow`, then inspect, and `plans prove`, as described above.
