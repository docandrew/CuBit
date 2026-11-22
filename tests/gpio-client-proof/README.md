# GPIO control-client and wire SPARK milestone

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/gpio-client-proof/run.sh
```

The gate proves the actual GPIO_Client_Controls generic in an explicit SPARK
wrapper, plus the complete GPIO_Control_Wire body, at levels1 and2. It requires
nonempty proof/flow results, all selected entities in SPARK mode, all goals Valid
(including trivial leaves), five client functional postconditions, and no
explicit Assume or skipped proof/flow. A successful command alone is insufficient.
Raw proof/flow records, dependency names, command statuses and audits are retained.

Per level: client92proof/19flow,14SPARKentities,138ValidCVC5+5Trivial goals;
wire42proof/16flow,14entities,75ValidCVC5+1Trivial. Combined134proof/35flow.
These counts describe this exact source/instance, not whole service/AML proof.

The client contracts establish per-State token high-water progression and its
preservation across acknowledgement, exact bound-sender preservation, matching
completion operation/lease/event identity, timeout ownership retention, late
completion preservation of the preceding quarantine reason, and bounded wake
hints. Step and Receive have proved null global effects: submission is separate.
Acknowledge requires Finished and clears the active binding while retaining the
consumed-token high-water. Process-global token allocation remains an obligation
of the enclosing owner; it is not proved by a per-State invariant.

Two ghost observations expose the stored token high-water and bound sender only
to contracts. The existing Matches predicate is an expression function so its
unchanged logic is visible to the prover. The private token range excludes the
already-rejected kernel no-completion sentinel. No wire operation, operational
branch, process-ID width, native interface or cleanup policy is changed.

GPIO_Client_Proof_Boundary imports an arbitrary acceptance result. Its declared
global effects/termination, no reentry/owner-state aliasing and no accepted
submission on False are trusted. Kernel envelope truth, capability admission,
transport delivery and caller cleanup remain outside this proof. The shared
Is_Process expression is used; no claim is made for the entire Process_ID
formatting/parser package. A separate overbroad selection retained an existing
unproved Image index obligation; runtime source was not changed.

`--validate-only` checks seven shared source hashes and two local proof fixtures
without executing. `--source-root DIR` selects a checkout. Default outputs are
unique TMPDIR directories; nice19/j1,180s per gate,5s solver calls,64MiBstack,
core0 and1MiBlogs. No private source fallback or native executable is needed.

Regressions on this exact source passed26,935checks/profile checked/release across
GPIO/HID reset724, GPIO/HID input3171, GPIO/HID/I2C14289 and HID native-hosted8751
(46build/runtime commands). Forty-seven actual HID native semantic units/profile
compiled with -gnatc and zeroobjects. These remain hosted/model or compilation
evidence, not native/kernel/hardware execution.

An initial proof harness omitted SPARK on the instance. It exited0 with zero
client checks and is explicitly rejected as evidence. The final raw-result audit
requires all entities in SPARK and positive proof/flow counts; the retained empty
result is rejected by that audit. One real Receive invariant obligation was then
resolved by exposing the unchanged completion predicate, without assumptions,
skips, contract weakening or raised resource limits.
