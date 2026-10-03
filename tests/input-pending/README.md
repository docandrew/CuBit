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
