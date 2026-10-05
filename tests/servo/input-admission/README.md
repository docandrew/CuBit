# Penny cached input admission

Penny bounds a batch to 32 polls. After its 1 ms deadline, it admits only events
already present in UI.App's local cache. No additional input fetch is allowed;
cache-validation failure ends that attempt without acknowledgment advancement.
A later ordinary poll can recover the server-retained events.

`Controls_Stale` remains a hard stop, even with cached input. The address-field
input-loss guard is unchanged. Individual event handlers may still block:
configure/resync application preserves its existing theme and buffer work.
Neither this policy nor the cache-only API promises a bound on total handler or
paint wall time. Server queue overflow during a long paint remains separate.

Use the pinned Nix shell, from the checkout root:

```sh
out=$(mktemp -d /tmp/penny-input-admission.XXXXXX)
gprbuild -p -P tests/servo/input-admission/admission.gpr -XADMISSION_OBJECT_DIR="$out"
"$out/admission_tests"
(cd kernel && alr exec -- gnatprove -P ../tests/servo/input-admission/admission.gpr \
  -XADMISSION_OBJECT_DIR="$out" -u servo_input_admission.ads --level=2 --report=all -j2)
python3 tests/compositor/test-input-batch-client.py
```

The policy tests exercise slow-fetch draining, stale-control rejection, the hard
32-poll ceiling, and deadline/clock boundaries. SPARK covers the pure admission
predicate's contract and termination, not the whole native bridge or IPC.

The adapter test extracts the actual UI.App cache-consumption, Receive_Input,
and Apply_Input_Result routines. It uses the real cache/protocol implementation,
with mocked input transport and theme/buffer operations. It checks empty and
ineligible windows, ordered acknowledgment, invalid cache identity/ack, no
extra input requests, later ordinary-poll recovery, and configure/resync routing.
Two deliberately broken variants must fail. This does not prove real theme IPC,
frame-buffer synchronization, or input loss elimination during rendering.

Native stress results must distinguish surviving a run from preserving input.
Use `audit-stress.py RUN --expected urls.json` to check the completed run and
all submitted addresses against a nonempty JSON list in exact order. Add
`--require-no-recovery` for the input-reliability gate: this also requires zero
recorded recovery attempts and zero reported input resync. Missing diagnostics
fail the evidence check. `--self-test` checks rejection of truncated/duplicate
addresses, faults, recovery, resync, and incomplete evidence. Run in Nix.

A 36-resize run can pass the survival gate while failing input reliability;
never report the former as proof of the latter. Paint timings are guest wall
time and can include scheduling delays, not CPU time or hardware benchmarks.


## Pointer cancellation and Servo event order

Native Settings/resynchronization cancels a page gesture explicitly. It must not
synthesize mouse-up (which can activate a link or button). MouseCancel travels
through the ordered paint/constellation input route, needs no hit test, and resets
documents belonging to that WebView. Script clears pressed/click/selection-drag
state and releases pointer capture after pointercancel. Retained gesture event
and target references are cleared on final release or cancellation.

`test-coalescing.py --servo SERVO_CHECKOUT --output OUTPUT_DIRECTORY` extracts
the actual patched document input handler and checks ten motion/wheel/button/
cancel ordering and acknowledgement cases. Removing the production coalescing
barrier must fail. This is a hosted test of that handler, not DOM integration.

`test-pointer-cancel.py --seed SEED_DIRECTORY --app PENNY_APP --desktop DESKTOP_SVC
--kernel KERNEL_ELF` runs in a disposable native VM. Run it through the private
build-workspace helper, or hold the shared build lock. The seed directory must
contain `init.ccl`, `desktop.img`, and `boot.iso` from the existing browser fixture.
The test copies those artifacts, overlays an offline page and explicit binaries,
and verifies the copied bytes before boot. It holds a page press while opening
Settings, requires pointercancel with no release/click, verifies capture loss,
and requires two normal subsequent clicks. Repeat with `--no-capture` and with
`--iframe`. An unpatched browser must fail the cancellation assertion.

These are functional VM regressions. They do not prove general crash freedom,
input reliability under every overload, or YouTube/video compatibility.
