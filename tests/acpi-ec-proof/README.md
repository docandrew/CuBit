# EC transaction-owner contract milestone

Run inside the Nix development environment with TMPDIR set beforehand:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-ec-proof/run.sh
```

`--validate-only` verifies production/harness hashes without proof execution.
`--source-root DIR` selects an explicit matching tree. The runner uses private
outputs, nice19/j1, 180s/proof, CVC5 at5s/goal, levels1/2, stack64MiB/core0 and
logs at most1MiB. Each level selects the actual EC_Proof_Instance.Owner generic
instance in explicit SPARK mode. The audit requires initialization and all four
public functional postconditions, positive proof/flow results, all selected
entities in SPARK and only valid goals; empty/non-SPARK/missing-post/unproved/
skipped reports are refused. The private publication evidence exercises those
negative audit fixtures without rerunning the prover.

Proved scope, conditional on coherent entry and the declared callback boundary:

- Initialization establishes coherence. Start exactly initializes the observable
  request/budget state or preserves it on rejection.
- Each still-running Step consumes one work unit; work never increases. The
  counted register callbacks increase by at most one. Terminal Step is inert.
- Accesses occur only at a checked clock sample before the finite deadline and
  no earlier than the previously computed wake time. A completed write enters
  a separate stamp phase; that phase performs no I/O and, when it continues,
  computes the next wake from its fresh sample plus one microsecond.
- State/phase and operation relationships are preserved. Completed success has
  checked time, possible effects and no recovery requirement. Failed results
  expose no data. Checked sample time never moves backward once established.
- Request_Stop changes only the stop flag for a running operation. Acknowledge
  exactly preserves a rejected/faulted state, or returns an eligible terminal
  owner to Idle and clears its public result. It cannot clear recovery ownership.

Ghost views/counters specify these properties; release builds erase them. The
actual I/O branches remain in the production body. Ownership is sampled once
into a local Boolean before branching, allowing volatile callbacks in SPARK.
The private failure helper combines its existing forced-recovery outcomes in
one exact transition; no protocol retry/reset or new hardware action is added.

Trusted/unproved boundary: callback effects/termination, no reentry or aliasing
of owner state, truthful access outcomes, fresh clock units and actual exclusive
reservation/Global Lock. Callback return values are otherwise unconstrained in
proof, so clock loss/reversal, refusal and unknown outcomes remain possible.
One callback per Step is not a proof of the callback's physical bus accesses.
Clock arithmetic is not validation of a native clock implementation.

This does not prove the full EC command protocol against a hardware model,
register catalog/endpoint authorization, _Qxx lookup/invocation/cleanup, kernel
IPC, firmware admission, GPE/event lifecycle, physical effects or the whole AML
interpreter/service. Those remain separate obligations. Tests/acpi-ec supplies
hosted protocol, named rights and actual AML/lid regression evidence; its model
is not hardware or native transport acceptance.

The publication milestone has25proof checks+26flow checks/level,19selected SPARK
entities,88ValidCVC5goals/level,zero explicit Assume/skipped/unproved results.
Initial syntactic/contract failures and the overly strong private intermediate-
state precondition are retained privately; no public guarantee was weakened.
No compiler crash research or new native builds were performed.
