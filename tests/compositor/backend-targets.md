# Display backend target lifetime

`CuBit.Backend_Targets` is the SPARK policy for Display's two GPU backing
buffers. It replaces the raw active-buffer index in the native Display service.
It applies to synchronous legacy presents and deferred pooled-frame presents.

A successful initial GPU clear establishes buffer zero as active. Preparation
may write only the other buffer. Display seals preparation before invoking GPU
IPC; neither target is writable during submission/completion. A validated
matching successful completion switches the active buffer. Abandoned preparation,
failed submission, malformed/stale completion, or an unknown completion closes
the affected policy permanently (unknown routing closes all outputs). Ordinary
clear requests cannot reopen an uncertain target. This preserves partially
modified target contents rather than accidentally treating them as an older,
clean buffer on a later partial redraw.

Both submission paths use the shared monotonic `Compositor_Requests.Allocate`
sequence. Exhaustion returns no token and prevents submission. The source
acquisition, saved reply, output/session generation, and backend target lifetime
remain distinct; returning a Desktop source does not make a GPU target writable.

## Proof and foreign boundaries

The pure policy's 13 SPARK analysis results (7 flow, 6 prover; zero unproved or
justified) establish its transition contracts: active targets cannot be writable,
sealing removes write permission, and failed/mismatched completion retains the
active index and outstanding identity. The policy itself does not access memory,
perform IPC, authenticate endpoints, or prove GPU fences.

Display's adapter is regression tested and native compiled, not proved SPARK.
It supplies the evidence that GPU clear/present replies are authenticated and
that successful present completion permits reuse of the former active target.
Those are backend contract assumptions. This change does not grant Desktop a
GPU mapping, remove the remaining source copy, implement i915 acceleration, or
establish physical tear-free scanout. A future exported target lease must preserve
these transitions and add the driver's explicit retirement/fence contract.

## Reproduction

Use the pinned Nix shell; native builds and tests hold `coordination/build.lock`.
Hosted outputs are isolated.

```sh
cd kernel
alr exec -- gprbuild -q -p -P ../tests/compositor/backend_targets.gpr
../tests/compositor/build/backend-targets/backend_targets_tests
alr exec -- gnatprove -P ../tests/compositor/backend_targets.gpr \
  -u cubit-backend_targets.adb --level=2 --report=all --checks-as-errors=on -j1
alr exec -- python3 ../tests/compositor/test-display-repair.py
alr exec -- python3 ../tests/compositor/test-backend-completions.py
```

The first test exercises 10,000 successful flips and sticky failed-state reuse
attempts. The actual Display prepare/synchronous-flip fixture checks 2,000 frames,
exact pixels and minimal repair bytes on two outputs, writable-state admission at
every copy, sealing before foreign IPC, busy preparation rejection and failed-flip
retention. Missing repair and redundant-copy mutants must fail.

The actual `collectFrames` fixture mocks the completion queue, reply transport,
and source-release helper. It checks both output routes, ten malformed or stale
completion cases, duplicate retirement and bounded unknown-completion flooding.
It does not claim to test actual capability authentication or source-grant return.

## Native integration evidence

The production Display binary built with this policy passed native CuBit tests
on four emulated CPUs: the firmware fallback restored the cursor exactly across
four moves and 48,000 wallpaper pixels across three resize cycles; both complete
100-second desktop regression runs passed. The virtio-primary path completed
39 viewer updates, 20 visible pause/resume cycles, graph/table switching, paging,
refresh and close. A second run with four 120-second CPU workers completed
26 updates, six pause cycles and close during worker overlap; all workers then
finished and the final fault scan passed. Input hashes and the private base disk
were checked, and temporary Display staging was restored byte-for-byte.

Retained manifests, serial logs, reports, proof summary and screenshot are in
`/tmp/cubit-backend-target-evidence/`; run logs are
`/tmp/cubit-target-native-v2.log` and `/tmp/cubit-target-overload.log`.
The first native attempt read a stale reused capture log and failed its screenshot
observer; fresh run names fixed the harness invocation without production edits.
These are native OS integration tests under QEMU, not i915 hardware throughput,
240 Hz, physical latency or display tearing measurements.
