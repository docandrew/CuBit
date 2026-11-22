# Completion admission before input

Desktop previously drained its completion queue until empty. The queue capacity
bounds simultaneous entries, but a producer can refill it as Desktop consumes
entries, so capacity alone did not bound the work preceding input dispatch.

`Compositor_Dispatch_Budget` now admits at most 64 completions per event-loop
pass and checks a 500 µs elapsed budget before admitting each subsequent one.
One completion remains admissible with an unavailable clock; subsequent work
requires a valid nondecreasing reading. Empty polling stops immediately. A
malformed poll result quarantines the existing affected clients and returns,
instead of continuing to route the reset completion or polling repeatedly.

Unconsumed entries remain in the kernel queue. The existing non-consuming
activity wait sees queued completions, so remaining work does not require a new
producer event to wake Desktop. The next pass begins with a new budget. Existing
input/request admission and the second input drain before paint are unchanged.

This is an admission bound. A single handler, foreign call or scheduler delay
may overrun 500 µs. It is not a worst-case execution-time, frame-rate or physical
latency guarantee. The pure scheduling policy is proved; the Desktop adapter,
clock/IPC implementation and callback execution remain integration boundaries.

## Verification

Inside Nix, from `kernel`:

```sh
alr exec -- gprbuild -q -p -P ../tests/compositor/dispatch_budget.gpr
../tests/compositor/build/dispatch-budget/dispatch_budget_tests
alr exec -- python3 ../tests/compositor/test-completion-drain.py
alr exec -- python3 ../tests/compositor/test-dispatch-integration.py
alr exec -- gnatprove -P ../tests/compositor/dispatch_budget.gpr \
  -u compositor_dispatch_budget.adb --level=2 --report=all --checks-as-errors=on -j1
```

The combined scheduling policy has 29 analysis results (15 flow, 14 prover),
zero unproved or justified. Hosted tests cover 10,000 policy cycles; 7,000
actual completion-admission scenarios; and 8,000 existing actual input/request
drain scenarios. Completion routing is replaced with a costed handler in this
fixture: it does not validate presentation identity, IPC or GPU fences.

Negative controls remove the admission bound, ignore the clock, or continue
after malformed polling; all are rejected. Existing dispatch mutations also
fail. The dispatch mutation runner now forces recompilation, preventing
same-timestamp source changes from reusing a previous fixture binary.

Both native Desktop variants compile. With the new scheduling policy and
Display lifetime integration, a private CuBit virtio-primary VM passed 27 viewer
updates, six visible pause cycles, graph/table switching, paging, refresh and
close during four CPU workers' overlap. All four 120-second workers completed;
final fault scan, input hashes and private base checks passed. This native OS
integration evidence is under QEMU, not a hardware completion-flood or 240 Hz
measurement. The continuous-refill adversary is covered by the hosted fixture.

Evidence: `/tmp/cubit-completion-budget-evidence/`, native log
`/tmp/cubit-completion-budget-native.log`, hosted log
`/tmp/cubit-completion-budget-host.log`, and final proof/dispatch run
`/tmp/cubit-completion-budget-proof.log`. No staging promotion or GPU changes.
