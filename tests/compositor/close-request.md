# Per-window close requests

Servo's multiple windows expose a legacy behavior: Desktop's title-bar close
button kills the entire owning process. The new opt-in protocol feature
`Graceful_Close` (bit 256) instead requests closure of that surface. Clients
receive input event `Close_Requested` (10), with zero payload words, and decide
whether to save, refuse or call `Destroy_Surface`. Existing clients retain
their previous behavior unless they opt in. Closing is not automatic or timed.

The server retains one unacknowledged serial per input channel, separately
from the 32-entry ordinary input queue. Repeated close clicks coalesce. A
close request shares the channel's nonwrapping serial allocator and is merged
in order with queued input. Ordinary overflow and source resynchronization
cannot erase the retained serial. Delivery does not clear it: an authenticated
poll/wait with `After_Serial >= close serial` acknowledges it. Retrying the same
poll can therefore redeliver the same serial after an uncertain reply. Clients
must follow the existing serial watermark protocol. Acknowledgment means
receipt, not agreement to destroy the window; a later click may ask again.

`Compositor_Close_Request` is pure SPARK. Its request and acknowledgment
contracts passed five checks (two functional contracts, three termination), with
zero unproved or justified checks. Hosted regression ran 1,000 cycles, each
including 1,000 repeated clicks and 1,000 real queue insertions causing repeated
overflow. It also covers nonwrapping exhaustion and acknowledgment boundaries.
Evidence: `/tmp/cubit-close-request-policy.log` and
`build/close-request/obj/gnatprove/gnatprove.out`.

```sh
nix develop -c bash -c '
 gprbuild -p -P tests/compositor/close_request.gpr
 tests/compositor/build/close-request/close_request_tests
 gnatprove -P tests/compositor/close_request.gpr -u compositor_close_request.adb --level=2
'
```

The proof does not establish IPC authentication, Main's merge/dispatch glue,
client handling, surface destruction, reader retirement or process isolation.
Native integration and multi-window title-bar close tests are still pending.
The integration is installed in Main, the Ada protocol, UI input constants,
C envelope validator and input trace codec/checker. Session 42149 passed the
Ada/C codec regressions and protocol proof (229 checks, zero unproved or
justified), then stopped on one native runtime line-length style error.
After formatting that declaration, session 16970 built the runtime and default
Desktop successfully and passed the input publication validator and 1,000-batch
trace regression. The pure latch run was session 98090. The enabled-metrics
Desktop binary from earlier tests predates this change and must be rebuilt
before testing these features together.

Evidence: `/tmp/cubit-close-native.log`, `/tmp/cubit-close-native-retry.log`,
`/tmp/cubit-close-native-built.sha256`. No claim of a passing native title-bar
close test is made yet. Servo has the ABI handoff and owns its event handler
and multi-window acceptance test.

Actual Main dispatch checks are in `test-input-queue-integration.py`,
`test-input-recovery-integration.py` and `test-close-dispatch.py`. These extract
production routines into hosted harnesses with real policies/codecs and mocked
kernel, drawing and waiter calls. The title-bar harness passed both surface
slots for opt-in, legacy and internal windows, and rejected a mutation that
fell through into destructive cleanup after requesting graceful close (98763).

The expanded actual enqueue/dequeue test exposed a motion ordering defect
(58962): two ordinary pointer moves on opposite sides of the retained close
could coalesce because the close is outside the ordinary queue. The required
fix makes the retained serial a coalescing barrier until the ordinary queue's
newest serial is later than that close. Session 95864 applied the repair under the shared lock and completed with
exit 0. The expanded actual enqueue/dequeue test passed, including preservation
of normal coalescing after the barrier, stable retry before acknowledgment,
1,000-event overflow and isolation between channels. It also rejected the
existing incorrect-coalescing-kind mutation. The actual recovery test passed
1,000 cycles, the trace glue emitted 200 exact records in five batches, and the
native default Desktop rebuilt successfully. The updated latch proof has five
checks with zero unproved or justified checks. Evidence:
`/tmp/cubit-close-barrier-native.log` and
`/tmp/cubit-close-barrier-built.sha256`; all four recorded hashes matched at
verification. Recovery tests now
also require source resynchronization to preserve pending close and require
surface retirement to clear only the target channel's close state.


The input trace policy was re-proved after extending the admitted event-kind
range through close request (10). Session 24380 passed 26 checks, with seven
flow and nineteen prover results and zero unproved or justified checks.
Evidence: `/tmp/cubit-input-trace-close-proof.log` and
`build/input-trace/obj/gnatprove/gnatprove.out`. The earlier 1,000-batch
regression exercises all ten valid kinds and rejects kind 11.


Servo's v26 native gate subsequently passed its feature checks and final
fault scan (`/tmp/cubit-servo-browser-v26-run.log` and
`/tmp/cubit-servo-browser-v26-features.json`). The feature report records sixteen
live tabs, four simultaneous windows, capacity refusal and isolated close with
slot reuse. The Servo owner recorded a real fourth-window title-bar X with
Settings open, followed by fresh wheel/Esc interaction in the original window
and reuse of the closed slot. This supplies native browser lifecycle evidence
for the new opt-in path. It does not prove every client's close handling or
replace the compositor policy and hostile-input checks.
