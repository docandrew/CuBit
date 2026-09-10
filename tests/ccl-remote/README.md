# Native control boundary checks

All commands run from the repository root, inside Nix:

```sh
nix develop -c make -C kernel test-ccl-remote test-ccl-web-preview
nix develop -c make -C kernel prove-ccl-remote
```

Hosted Ada tests enable runtime assertions and validity/overflow checks. Native
app builds do not enable executable assertions. No new `pragma Assume` or
`SPARK_Mode => Off` is used in the pure HTTP/CBOR boundary.

Focused SPARK result (2026-09-08): **143/143 obligations discharged**, including
129 runtime checks, six initializations, two functional contracts, and six
termination checks. Zero unproved/justified checks or assumptions in these
three units. This does **not** prove complete HTTP semantics, TCP correctness,
kernel IPC, native pointer/grant adapters, browser code, or all transitive CCL
and upstream CBOR dependencies. Upstream float-proof gaps remain documented.

The hosted suite checks HTTP fragmentation at every byte boundary of a valid
request, duplicate/framing/origin failures, preflight, malformed/truncated CBOR,
and shared CCL evaluation. Browser codec tests independently check strict
scalar types, canonicality, uint64 IDs, truncation and bounds.

`host_tests` additionally instantiates the shared interpreter with an explicit
deterministic test host: missing/forged grants, no host-free side effects,
whole-source type checking before invocation, skipped branches, fuel exhaustion,
fresh samples per call, host failures/wrong result types, unsupported lifecycle
contracts, and the HH:MM:SS expression. Native smoke tests separately validate
the real clock invocation and formatter over HTTP/CBOR.

```sh
nix develop -c make -C kernel prove-ccl-interpreter-host
```

That target covers the refactored language, shared catalog and a concrete SPARK
test-host instantiation; it does not prove the native IPC adapter or clock
service's wall-clock response time. Interpreter fuel bounds computation, not
time spent waiting inside a trusted synchronous host call.

Hardened result (2026-09-08): **1468/1468 obligations discharged**, including
all 11 functional-contract checks. No justified checks or assumptions. The
earlier 20 unresolved checks were eliminated using bounded literal slices,
separate region/per-value capacities, bulk concatenation copies, and an isolated
bounded decimal formatter. See resolved SEC-017.

`make -C kernel prove-ccl-periodic` independently proves the reusable periodic
state machine (21/21, including the stop-state postcondition). `periodic_tests`
covers duplicate/stale completions, stop during a non-cancellable invocation,
no catch-up bursts, failed runs, and timestamp exhaustion.

The expanded HTTP/CBOR boundary also passes (145/145). An isolated output
directory can be used when proving it alongside the interpreter:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P../tests/ccl-remote/remote.gpr --subdirs=wire-proof -u control_http.adb control_wire.adb ccl-control.adb --level=2 --report=all --checks-as-errors=on -j2'
```

To test real native networking and evaluation:

```sh
# Terminal 1: KVM lab guest, temporary disk, loopback forward 18445
nix develop -c make -C kernel ccl-remote-lab
# Terminal 2: pause any Observatory polling while running this test
nix develop -c node tests/ccl-remote/smoke.mjs
```

The host sends real CBOR; only the guest evaluates it. Assertions cover actual
nonzero process IDs, clock IPC, CORS preflight, fragmented body, integer/string
results and type errors, rejected origin/CBOR, a stalled request's five-second
deadline, and successful evaluation afterwards. The monitor test additionally
waits without making requests and checks that native timer wakeups advance the
run count, then checks stop, replacement generations, and failure isolation.
Run this against a fresh lab guest with no active widget. This is not a TLS or remote
authentication test. See the Observatory README for the explicit lab scope.
