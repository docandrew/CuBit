# Bounded pointer publication retention

`Input_Pending` retains up to 32 exact source packets before successful
nonblocking mailbox admission. Refusal leaves the head unchanged, including its
source sequence. Only successful admission acknowledges it. Opposite motion,
button edges and wheel events retain their original order. No pixel storage,
heap allocation or unbounded queue is introduced.

The policy is integrated into both PS/2 and xHCI pointer publication. When pending work remains, PS/2 waits for an IRQ
or a one-millisecond retry deadline, so a final refused packet does not depend on
further mouse movement. With no pending work it returns to interrupt-only wait.
Each publication drain attempts at most 32 sends and stops at the first refusal.
The existing controller-byte drain and keyboard path are unchanged.

Local overflow is explicit: discard the old 32-packet backlog, retain the latest
packet, and mark it RESYNCHRONIZE. Desktop receives the latest button snapshot
and detects the sequence discontinuity; this does **not** reconstruct discarded
motion, clicks or wheel history. Consumer replacement discards pending input and
marks the next report for recovery. xHCI preserves earlier optical-storage and
boot-log deadlines, adds a retry timer only while input is pending, and sends
a fresh button snapshot on the next HID report after consumer replacement.

## Evidence and limits

The native dual-output test previously recorded four mailbox refusals followed
by a source gap. Four lost -70 X reports matched the observed 280-pixel cursor
offset (`tests/compositor/build/pixel-routing-evidence/dual-workarea.serial.log`,
lines 1098 onward). The retention regression reconstructs that displacement
and verifies FIFO order, refusal stability, circular wrap, overflow's final
button state, consumer reset, and sequence rollover.

Run inside the Nix environment:

```sh
gprbuild -p -P tests/input-pending/pending.gpr
tests/input-pending/build/pending_tests
cd kernel
alr exec -- gnatprove -P ../tests/input-pending/pending.gpr -u input_pending.adb \
  --mode=all --level=2 --report=all --checks-as-errors=on -j1
```

The SPARK boundary is the pure queue policy: bounds, FIFO content preservation,
sequence advancement, removal and explicit recovery. Driver syscalls, hardware
parsing, scheduling and eventual mailbox availability are outside that proof.
Hosted tests do not establish native delivery or end-to-end latency. Hosted regression and all 28 SPARK analysis results pass (7 flow, 21 prover;
zero unproved or justified checks). At that pre-timestamp gate, queue storage was 792 bytes on the hosted
x86-64 build. Native PS/2 and xHCI compilation/linking have passed; the native CuBit dual-output gate also passes with the existing Mesa Desktop
and the new PS/2 driver (QEMU TCG, four CPUs, 1 GiB). Evidence is in `build/evidence/`.

The kernel's `event_drop` statistic counts failed mailbox admissions. With
retry, it is a backpressure-attempt count, not a count of irretrievably lost
source packets. A successfully retried packet must not create a source gap.
This fixes bounded transient refusal; long consumer stalls still require
scheduling and render-work improvements to meet the desktop latency goal.

## Actual PS/2 driver fault harness (hosted)

`driver.gpr` compiles the production PS/2 `main.adb`, boot probe, typed input
encoder/decoder and retained queue. Only the port/IPC boundary is replaced.
The fixture feeds real three-byte PS/2 packets, refuses every send until the
first timed retry, and then checks exact payloads, source sequences and flags.
It exercises four -70 motions followed by button-down/up, 40 reports overflowing
the 32-packet retention bound, and consumer replacement while backpressured.
All three cases pass. The final retry occurs without a new IRQ in the first two
cases. No test-only fault switch is compiled into the native driver.

```sh
gprbuild -p -P tests/input-pending/driver.gpr
for mode in retry overflow replace; do
  tests/input-pending/build/driver/driver_tests "$mode" || exit
done
```

This is an actual driver control-flow test with simulated external boundaries,
not a live CuBit or USB controller test. The fixture does not cover wheel-device
negotiation, IRQ races, kernel authority stamping or scheduler progress.

## Native compositor regression

`build/evidence/dual-final.log` ends with `headless: PASS desktop-dual-output`.
All four observer suites passed: primary migration, 125%/150% scaling and mixed
seams, output arrangements, split dragging/maximizing, and exact cursor/window
background restoration. The VM was stopped after all four suites passed; the
300-second argument was a deadline, not a completed 300-second soak.

This run recorded no mailbox refusals. Its initial source recovery flag follows
consumer registration. It therefore proves integration/regression behavior, not
native backpressure recovery; the separate deterministic CuBit loopback fixture
below supplies kernel-transport fault coverage. The hosted production-driver
fixture additionally verifies the real PS/2 driver control flow. Native USB runtime fault coverage and hardware latency/240 Hz
measurements remain outstanding.

## Native CuBit mailbox fault oracle

Run under Nix and the shared build lock:

```sh
flock --exclusive --timeout 600 coordination/build.lock \
  nix develop -c bash tests/input-pending/native/run.sh
```

The script builds a disposable bootstrap program and a private ISO under
`build/native.*`, reusing the existing kernel and recording its binary hash.
It never replaces production services or images. The oracle uses the inherited
self endpoint and real kernel event IPC, fills the 32-entry mailbox until
admission refuses, and then exercises the production typed protocol, retained
queue and timed activity wait. In the transient case six refused reports
(four -70 motions and a button down/up pair) all arrive exactly once and in
order after capacity is freed, with no new source report. In the overflow case
40 refused reports produce the eight newest reports and explicit recovery.
Both cases check exact mailbox rejection counts, kernel authority stamps,
source sequence/payload/button state, deadline wake and absence of duplicates.

Final run `build/native.06pF1E/serial.log` passes both cases; its `input.sha256`
records the reused kernel, new fixture and policy inputs. Driver-created HID
packets, independent publisher/consumer processes, capability isolation and
physical display latency are not covered by this loopback oracle. Native USB
controller/backpressure integration remains outstanding.

The first run (`build/native.SiyixK`) failed an incorrect fixture assumption:
a later minted endpoint was expected to supply the tag, but kernel destination
resolution correctly selected the earlier inherited self endpoint. The final
fixture uses that inherited endpoint directly; no production behavior or
kernel authorization was changed to make the test pass.

## Acquisition time survives renderer stalls

Pointer reports now retain the driver's acquisition time in milliseconds when
appended, rather than sampling time again during publication retries. PS/2 and
xHCI put that value in the high 56 bits of the relative-pointer snapshot as
`time + 1`; the low eight button bits and four-word source envelope remain
unchanged. Zero high bits mean unavailable. Zero boot time is representable;
unavailable/oversized clocks are left unstamped, never wrapped. Keyboard
snapshots are unchanged. This is driver packet acquisition, not a measurement
of physical switch closure or hardware interrupt arrival.

Desktop passes this acquisition time to the existing click recognizer. Legacy
unstamped events keep processing-time behavior; switching between time qualities
resets the click sequence so the two clocks cannot be paired. Ordering gaps,
consumer replacement and queue overflow retain their existing resynchronization
semantics. The queue is now 1,048 bytes on the hosted compiler (256 bytes more
than the original 792-byte queue), with the same 32-report bound.

`timing_tests.adb` checks 2,001 render/handling delays, with a negative control
showing processing-time double-click loss and a positive path using the retained
acquisition times. It also covers timestamp encoding boundaries, exact refused
send retention and overflow keeping the newest timestamp. Production PS/2
publication tests validate both unchanged buttons and the exact mock clock on
retry/overflow/consumer replacement. Final combined proof has 44 results
(17 flow, 27 prover), zero unproved or justified checks, including existing queue
and input-wire contracts. Logs: `build/pointer-time-policy-r2.log`.

Run the timing project in Nix (`kernel/alr exec -- gprbuild -P
../tests/input-pending/timing.gpr`) and execute `build/timing/timing_tests retry`.
Native PS/2, xHCI and Mesa Desktop rebuilds pass. The unchanged native CuBit
regression under QEMU passes primary display, mixed 125/150% scaling, arrangement
and Desktop interaction checks (`../compositor/build/pointer-time-native-r3.log`).
Its interaction fixture uses PS/2; physical USB timing remains unvalidated.
The motivating failure and accepted binary hash are documented in
`../compositor/desktop-frame-completion.md`. Millisecond timestamps fix gesture
semantics; they are not sufficient for sub-millisecond latency profiling.

## Agreed motion coalescing (Pointer_Pending, 2026-10-09)

Raw retention made the pending backlog grow with the device report rate.
The kernel takes 16 events per publisher (IPC-002 credits) and the driver
kept 32 more, so a desktop stall longer than 48 report periods (48 ms at
1 kHz, 384 ms at 125 Hz) overflowed: the backlog was discarded, desktop saw a
sequence gap and resynchronized (NUC: a resize drag cancelled mid-drag).
The backlog was also FIFO, so after a stall desktop replayed stale motion.

`Pointer_Pending` (userspace/lib/input) replaces raw pointer retention in
both xhci.drv and ps2.drv. RELATIVE_POINTER reports are already declared
ACCUMULABLE_DISPLACEMENT, so motion is merged by agreement into the newest
*unpublished* report when:
- that report is not itself a button/flag transition (its buttons equal the
  report before it), and the new report has the same buttons and flags;
- the newest report has no wheel steps, or the new report has no motion (a
  wheel step is never moved past later motion);
- the summed X/Y/wheel still fit the wire fields (12/12/8 bits).
Merging keeps the report's sequence number, recovery flag and acquisition
time (so desktop's source age is the oldest contained input), and consumes
no new sequence: no gap. Buttons and wheel are never lost or reordered; a
press or release lands at exactly the position it happened at. True overflow
now needs 32 unpublished *transitions* and stays explicit (Overflowed,
RESYNCHRONIZE, `xhci: pointer retention overflow`).

xhci.drv's stats line adds `coalesced=` (reports merged), `overflow=`
(retention losses) and `busy=` (kernel credit refusals; the report was kept
and retried). Desktop's `event_drop=` is renamed `event_busy=`: since IPC-002
step 3 it counts credit refusals that the publisher kept, not losses. Desktop
adds `src_age_max_ms=`/`src_age_avg_ms=`: driver acquisition to intake.

Proof (SPARK level 2, all of userspace/lib/input: Input_Pending,
Keyboard_Pending, Pointer_Pending): 142 checks, 0 unproved, 0 justified.
Proved: the pending tail is exactly the last report on the wire; a coalesce
keeps count and sequence, conserves displacement (tail = old tail + report),
and the merged tail still has the buttons of the report before it; an append
preserves FIFO content; overflow is flagged.

```sh
cd kernel
alr exec -- gnatprove -P ../tests/input-pending/prove.gpr --mode=all --level=2 \
  --report=fail --checks-as-errors=on
alr exec -- gprbuild -p -P ../tests/input-pending/pending.gpr
../tests/input-pending/build/pointer_tests
```

`pointer_tests` drives a model consumer that applies reports as desktop does
(move, then button change and wheel at the new position) and compares every
observation (transition or wheel turn, with its position) against the raw
stream: a 1000-report flood, a press-drag-release mid-flood, wheel between
motion, and 20,000 random reports with random partial drains. Mutation
checks: dropping the transition guard or the wheel guard each fails the
model (observation/position mismatch) with assertions off. A stall model
reproduces the NUC loss under the old policy (1 kHz, 100 ms stall: 2
overflows) and shows none with coalescing (peak 1 pending) for 125/500/1000 Hz
and stalls up to 1 s.

The PS/2 driver fixture (`driver.gpr`, real ps2 main.adb) now expects four
-70 packets to arrive as -70 then -210, button transitions unchanged, and
uses 40 button transitions (not motion) for its overflow case. The fixture
had drifted from the runtime API (No_Process, Registered_Driver, capSend
deadline); it compiles again.
