# Bounded Desktop input batches

`Compositor_Input_Batches` is the selection policy for batched input delivery.
Desktop's IPC dispatcher and Penny use the eight-event grant-backed transport.
The bounded server queue now retains 128 events. It can still overflow if a
client stops consuming for long enough; overflow produces explicit resynchronization.

Each ordinary character produces three events. Ordinary polling returns one event per synchronous call; batched polling
returns up to eight. Batching reduces those calls without enlarging the queue
or dropping key transitions to make room. The existing four-word reply
cannot contain eight full event envelopes; transport requires a separate buffer.

The pure SPARK policy:

- Selects up to eight events, merging the separately retained close request in
  ascending serial order with ordinary input, exactly as current dequeue does.
- Returns unchanged event records, the last selected serial, and whether more
  events remain after that serial. No serial arithmetic can wrap.
- Leaves the source queue and close latch untouched. Repeating a selection from
  unchanged sources produces the same result. Publication and acknowledgment
  remain separate integration responsibilities.
- Uses fixed storage and at most eight scans of the 128-slot queue. It allocates
  no memory and performs no IPC or pointer access.

The snapshot contract proves each returned entry is the next eligible event,
all used entries are valid, unused entries are cleared, and the continuation
indicator is exact. `Next` proves provenance and minimal eligible serial. The
queue's existing serial allocator establishes uniqueness in live use; malformed
duplicate serials do not establish two independently deliverable events.

## Validation

Run in the Nix development shell:

```sh
gprbuild -q -p -P tests/compositor/input_batches.gpr
tests/compositor/build/input-batches/input_batches_tests
gnatprove -P tests/compositor/input_batches.gpr -u compositor_input_batches.adb \
  --level=2 --timeout=20 --checks-as-errors=on --report=all -j2
```

The independent oracle collects and sorts events, rather than calling the
production selection helper. It covers 110,904 combinations of queue occupancy,
slot permutations, acknowledgment positions, batch limits, close positions,
payloads and serial exhaustion. It also checks repeatable selection and no queue
mutation. The accepted run is `build/input-batches-r2.log`: all cases pass and
all 19 SPARK obligations are discharged without justification. Dependency and
source hashes are in `build/input-batches/source-hashes.json`. The proof relies
on the existing queue helper's contract; it is not an IPC or publication proof.

## Remaining integration

The new `Compositor_Input_Batch_Wire` codec defines a 320-byte native
little-endian snapshot: eight header words (version magic, request identity,
surface, acknowledged serial, count, through serial, more flag, reserved zero),
then eight four-word event records (kind, serial, payload0, payload1). Request
identities must not be reused within a live transport session. This is a format,
not a grant attachment or an IPC operation already implemented in Desktop.

Decode requires the expected surface, request identity and acknowledged serial.
It rejects unknown versions, bad bounds/flags, nonzero unused records, unordered
or replayed serials, and payloads rejected by the existing single-event codec.
A malformed final record rejects the entire result; no prefix is exposed.
Callers must provide a stable local copy after publication: this pure codec does
not establish atomic reads from concurrently changing shared memory.

`nix develop -c bash tests/compositor/test-input-batch-wire.sh` prepares private
portable runtime sources and runs the codec tests and proof. Accepted evidence:
`build/input-batch-wire-r3.log`, 769 codec/rejection cases and 246 SPARK checks
across the new codec and existing Desktop protocol, zero unproved/justified.
The protocol's existing input validator expression moved unchanged into its
visible declaration to make cross-unit proof usable; payload rules are shared.
The codec proof establishes successful-decoding validity and encoder field
correspondence, not the transport's memory ordering, identity allocation,
authentication or grant lifetime.

The transport must authenticate surface ownership before acknowledgment or
publication, validate the grant generation/access/extent, publish a bounded
snapshot, and distinguish failed publication from delivered input. A writable
client-owned grant can avoid Desktop-owned output allocations; acquisition and
release costs must be measured. Failed acquisition must not remove events, and
uncertain release must retain bounded ownership until confirmed. A mapped
pointer and a successful return from a copy are not retirement evidence.

The client must consume cached events in order under a bounded handling budget,
preserve resynchronization and close semantics, and avoid fetching another batch
while one is unconsumed. Integration needs native tests at the original typing
rate, failure injection, queue occupancy/age measurements and total input-to-frame
latency. No IPC reduction, overflow fix or hardware latency improvement is claimed
by these policy tests alone.

The unchanged input request/reply and C/Ada parity corpus from the existing
Desktop protocol regression passes separately (`build/input-validator-regression-r4.log`,
private workspace `build/input-validator-9kp8s_i8`). The complete protocol suite
is not accepted: it stopped earlier at its unrelated Display attachment
assertion that grant slot 4096 is invalid (`main.adb:96`), while the current
shared grant limit is larger. No display-test expectation was weakened for the
input change. The private extraction records its scope in `extraction.txt`.

## Bounded grant delivery policy

`Compositor_Input_Delivery` separates grant ownership from the resettable input
channel. A delivery attempts at most one acquisition, one fixed snapshot write
and one return. Ownership is recorded before the writer is invoked. A failed
return preserves the exact grant reference and returns `Quarantined`; another
delivery returns `Busy` without callbacks until `Retire` confirms cleanup.
A successful copy alone never produces `Published` while a loan remains held.
A null mapping reported as acquired still requires return and is never written.
No input queue is mutated by this layer.

`input_delivery.gpr` exercises all 16 acquire/write/return/null-mapping outcome
combinations, callback ordering/counts, blocked reuse, failed cleanup, eventual
cleanup and repeated retirement. `build/input-delivery-r2.log` passes, and the
proof report has 255 discharged checks across its analyzed units (including
inherited codec/protocol results), zero unproved or justified checks. Imported
callback termination is an explicit trusted boundary in the prover's diagnostics;
this establishes bounded callback counts, not a wall-clock execution bound.

The new `Desktop_Input_Transfer` adapter acquires exactly 320 writable bytes at
offset zero with the kernel-authenticated expected owner and generation-checked
grant reference. Its writer rejects null, unaligned and wrapping addresses,
then copies the fixed metadata snapshot. Pointer mapping validity, lack of alias
with service state, syscall semantics and publication ordering remain audited
non-SPARK boundaries. The adapter is not called by Desktop's dispatcher yet.
Persistent delivery-state storage must survive channel clearing and be reclaimed
only after confirmed return; native fault and client-death tests remain required.

Native component compilation passes against the frozen CuBit runtime in
`build/input-delivery-native-agm_6pjf` (`inputs.json`, `result.json`), with source
hashes rechecked against the checkout. The log is
`build/input-delivery-native-private-r2.log`. This is compile evidence only;
no native link, grant execution, client-death cleanup or Desktop boot is claimed.

## Desktop dispatcher integration

`Compositor_Input_Delivery_Pool` provides service-lifetime storage independent of
input channels. It refuses additional loans for an owner/surface already holding
one, preserves every uncertain reference, and polls exactly one slot per tick in
round-robin order. The hosted test covers full-pool refusal, repeated retries,
independent owners/surfaces, cleanup after channel loss, a permanently failing
loan beside a releasable one, and reuse with a new grant generation. Its proof
report has 272 discharged checks including inherited policy/codec checks.

`Compositor_Input_Protocol` reserves extension label `0x0823`. Requests contain
four words: surface, previous acknowledged serial, encoded grant reference, and
nonzero request identity. Success receipts contain status zero, request identity,
event count and through serial; the single flag bit means more input remains.
Errors contain one status word with zero unused words. The protocol test checks
malformed envelopes, grant bounds, identities and receipt limits; its proof report
has 263 discharged checks including inherited units.

`Compositor_Input_Acknowledgment.Apply` clears only entries at or before the
previous acknowledged serial, including the retained close latch. Its exact
postcondition is proved; 1,156 queue/close combinations plus exhaustion cases
check preservation of newer events and idempotent retry.

The pending dispatcher patch is preserved in `build/input-service-preview.patch`
and `build/input-service-wcgdmxnx/apply.py`. The latter is a guarded application
script accepting the target repository root. It compiled against both native
Desktop backends in that isolated snapshot, whose input hashes and result are
recorded. The tested patch is now applied to the live Desktop main, byte-identical to
the accepted preview. The runtime and legacy Desktop link pass, and the Mesa
Desktop component compile passes (`build/input-batch-publication.log` and
associated build logs). It authenticates the surface owner before
grant acquisition, excludes simultaneous async input waits, and applies the old
acknowledgment only after successful publication and confirmed grant return.
Eight cleanup slots survive input-channel resets; one is polled per maintenance
turn. The new batch itself remains queued until a later acknowledgment.

The extracted actual handler now passes authorization, malformed-source/request,
active-waiter exclusion, acquisition/write/return/null-mapping faults, retry and
four-batch draining tests. Three deliberate negative controls (ownership bypass,
premature acknowledgment, accepting failed publication) are rejected. Evidence:
`build/input-batch-service-o2orjvfy/result.json`; the mocked kernel/grant boundary
does not establish native grant behavior. Native grant/client-death tests, client
batch consumption, and the failing four-window browser workload remain outstanding. In particular,
the newer browser failure at one-second typing intervals is preserved in
`tests/servo/build/perf-tmp/nix-shell.llw377/penny-interaction-18107202`; pacing
does not establish that input overflow is fixed.

Native single-output regression now passes on the newly linked legacy Desktop:
`build/input-batch-desktop-boot.log` / `build/input-batch-desktop.serial.log`.
The runner checks boot/window readiness, keyboard/pointer/button input, source
rejection absence, and exactly one maximize/restore pair. It does not exercise
batch requests (clients still poll singly), mixed-output DPI, GPU execution or
physical latency. `desktop-dual-output` is required for the separate mixed-DPI
interaction gate; setting its environment flags on `desktop-display` is not
sufficient.

## Client metadata cache

`Client_Input_Batch_Cache` validates the complete page and matching receipt
before accepting any event. Count, through serial and more flag must agree;
surface/request/acknowledgment bindings must match. Load refuses to overwrite
unconsumed events. Take requires the bound surface and exact previous consumed
serial, then advances by exactly one event. Its More flag covers both locally
cached events and further server input. Clear discards metadata only and has no
grant-lifetime side effects. The discriminated output of Take must be an
unconstrained `DP.Input_Result`, as required by its proved precondition.

`client_input_batch_cache.gpr` passes hosted tests and SPARK proof in
`build/client-input-batch-cache-r2.log`; tests cover empty/full batches, all
lengths, continuation, wrong context, mismatched receipt fields, atomic
rejection, no overwrite, per-event acknowledgment and maximum serial values.
Hashes are recorded alongside the proof. The cache is now connected to opt-in synchronous `CuBit.UI.App` polling and
waiting. Native grant and browser consumption still require execution tests.

## Client process transfer page

`Client_Input_Channel_Policy` bounds setup to one attempt per process-lifetime
state and reserves nonzero, nonwrapping request identities. Failure or exhaustion
disables reuse. Hosted tests and all 69 SPARK checks pass in
`build/client-input-channel-policy-r1.log` (including analyzed queue dependencies).

`Client_Input_Channel` is the native boundary: one 4 KiB owned page, one writable
grant to the Desktop endpoint, one synchronous call per fetch and a stable local
copy of the returned 320-byte snapshot. An x86 atomic exchange with a compiler
memory clobber serializes page use; acquisition tries once without spinning.
Contention leaves the cache unchanged for ordinary-poll fallback. Any failed
receipt or invalid page disables future reuse and retains the bounded page until
process exit; window teardown cannot lose its ownership state. The one-time
allocation and grant may also remain retained after failed setup. This first
version intentionally has no in-process recovery/regrant after quarantine.

The actual native component compiles in frozen snapshot
`build/client-input-channel-ju18xujm`, with verified source/runtime hashes and
`result.json`; see `build/client-input-channel-native-private.log`. This does not
prove syscall behavior, grant execution, concurrency correctness or IPC memory
ordering. The atomic instructions reuse the native futex test's exchange pattern;
they and raw shared-memory reads remain audited non-SPARK boundaries.

UI wiring is now opt-in via `App.Open (..., batched_input => True)`, defaulting
to False. Observatory and CCL treat rejected async Submit_Input_Wait as fatal,
so async clients remain on the existing path. Submit_Input_Wait explicitly
rejects opted-in windows; these clients must use synchronous Poll_Input/Wait_Input.
Async local-completion support remains separate work.

## UI App integration evidence

The actual Receive_Input routine passes an extracted hosted fixture covering
cache-first polling and synchronous waits, one-event acknowledgment, empty
successful batches without an extra poll, fallback, opt-out, outstanding-wait
exclusion, closed windows and mismatched cache context. Two deliberate negative
controls (bypassing cached waits and advancing acknowledgment too far) fail.
Evidence: `build/input-batch-client-mo38ue3f/result.json`; syscall, transfer and
Apply_Input_Result effects are mocked, so this is routing evidence only.

The updated UI App compiles natively in both backend project configurations in
`build/input-client-app-h_g6aa88`. `build/input-client-ready.json` records published
source hashes. Open and successful Close clear only per-window cached metadata;
the transfer page remains process-owned. Failed Close keeps window/cache state.
Synchronous waits deliver cached events first and issue a real wait only after
that cache drains. Servo's App.Open call has not been changed by this agent.
The original four-window input/focus gates remain pending. Native batch evidence
and diagnostic integration are described below.


## Native delivery probe and diagnostics

`userspace/apps/desktop-check/desktop_input_batch.adb` queues twelve configure
notifications and requires two actual `Client_Input_Channel.Fetch` deliveries
of eight and four events. Ordinary polling cannot satisfy this check. It also
checks an empty acknowledgment fetch, cache-first UI wait, closing with cached
input, transfer-page reuse after window closure, and ordinary polling after a
rejected request permanently disables the channel. The headless desktop-protocol
runner requires `DESKTOP-INPUT-BATCH-CHECK: PASS batches=2 events=12 fallback=1`.
The first native run emitted that marker; its wider protocol suite failed a
preexisting stale fixture that called feature bit 256 unknown, despite that bit
now representing Graceful_Close. The fixture now uses the first bit above all
recognized features; the full-suite rerun is pending.

`App.Input_Statistics` exposes allocation-free, read-only per-window counters:
Successful_Fetches, Fetched_Events, Delivered_Events, Fallback_Polls and
Cache_Rejections. Successful_Fetches includes empty valid snapshots; only
Fetched_Events/Delivered_Events establish positive batch event delivery.
Fallback_Polls counts unsuccessful fetch attempts followed by ordinary polling,
including guard contention or a disabled channel. Cache_Rejections separately
counts invalid local cache context. Ordinary waits after cache exhaustion are
not fallback polls. Counters saturate at Unsigned_64'Last and reset on Open;
Close preserves their final values. The snapshot also reports the window's
Batch_Enabled selection and an atomic process-wide sticky Channel_Disabled flag.
Read it on the window's owning event thread; this is diagnostic state, never an
authority or synchronization mechanism. There is no per-event logging or IPC.
The counter instrumentation is non-SPARK UI code, checked by routing and native
regressions; it does not expand the proved policy boundary.

Diagnostic validation: `build/input-batch-client-qonkpewq/result.json` passes
actual routing plus saturation, successful/empty delivery, fallback and rejected
cache counter assertions; both deliberately broken routing variants still fail.
`build/input-client-app-9mfmpw0d/result.json` and
`build/client-input-channel-yf9ys0bj/result.json` confirm native component
compilation against frozen inputs. Published hashes are in
`build/input-client-diagnostics-ready.json`. The full diagnostic native rebuild
stopped in unrelated process-launch runtime source style checks before boot;
no diagnostic native execution or complete protocol-suite pass is claimed.

The complete diagnostic `desktop-check.app` subsequently linked in private
snapshot `build/input-batch-native-link-pynee0ti`. Its original launcher exited
nonzero because seven unrelated runtime sources changed in the shared checkout
after copying; this was not a compiler/link failure. The separate
`frozen-link-audit.json` verifies all 865 snapshot input hashes, a native x86-64
executable and zero undefined symbols, and lists the exact root drift. This
establishes a full link against frozen inputs, not a current-tree build or boot.

## Retained events must remain immutable after exposure

Desktop records a per-channel `exposedThrough` watermark before attempting to
publish a nonempty, validated snapshot. Adjacent motion may coalesce only when
its newest serial exceeds that watermark and the existing close barrier allows
it. A later motion therefore cannot overwrite an event a client already holds,
then disappear when that older event is acknowledged.

Freezing precedes the grant writer because bytes can become visible even when
a write or loan return reports failure. Freezing after acquisition failure is
conservative: it can use more queue slots, but never acknowledges or drops an
event. Empty snapshots cannot advance the watermark from a caller-supplied
`After`. Prior-`After` acknowledgment, explicit overflow resync, close ordering,
wire format and batch size remain unchanged. This exposure fix originally kept
the 32-entry queue; the bounded headroom change below raises it to 128. Channel teardown
resets the serial namespace and watermark together.

Run `python3 tests/compositor/test-input-batch-service.py` and
`python3 tests/compositor/test-input-queue-integration.py` inside Nix. The first
extracts actual enqueue, close, and batch-handler code, with mocked kernel/grant
boundaries. It checks exposed-motion retries/acknowledgments, partial-write and
return failure, failed acquisition, authorization, empty high-After requests,
close barriers, overflow, fresh-channel state, and continued coalescing of the
unexposed tail. Removing the exposure freeze must fail; the existing broken
owner, acknowledgment and publication controls must also fail. The second
retains ordinary dequeue, close, ordering and isolation regression coverage.

The candidate also passed native logstore startup/retained-software-text
retrieval and 36 Penny resize/load/menu/window cycles. Two address-entry
recoveries remained in that run: this correction fixes a distinct delivery bug,
not synchronous rendering latency or keyboard overflow. These are regression
results, not an end-to-end SPARK proof or a crash-freedom guarantee.


## Bounded retention during client painting

The queue capacity is 128 events per Desktop input channel. Batch size remains
8 events and the wire layout is unchanged. With 48-byte events, this increases
retention storage from 1,536 to 6,144 bytes per channel: 36 KiB more across eight
channels. GNAT reports Main's bounded frame rising from 56,528 to 93,392
bytes; ELF data/BSS and the existing 16 MiB stack reservation are unchanged.
Storage remains fixed; serial allocation, acknowledgment, exposed-event
immutability, close ordering and explicit overflow recovery retain their contracts.

The combined actual-handler regression drains the configured capacity in batches
of eight rather than assuming four batches. Integration fixtures derive overflow
and stale-ack serials from Capacity. Queue contracts and failure behavior remain
required at the larger bound. Headroom handles finite paint pauses; it does not
replace reliable loss handling or guarantee responsiveness during an indefinite stall.
