# Desktop protocol checks

From the repository root, using the Nix environment:

```sh
nix develop -c make -C kernel test-desktop-protocol prove-desktop-protocol
```

The test uses the production portable protocol and grant-reference sources,
staged under `build/` to avoid importing CuBit's freestanding Ada runtime into
the hosted test.
Assertions and overflow checks are enabled only in this hosted executable.

See [the protocol specification](../../docs/desktop-protocol.md) for the exact
proven properties, unproved round-trip properties covered by tests, and the
native `desktop-protocol` QEMU adversarial test. None of these proves the
whole compositor, grant lifetime, kernel IPC authentication or latency.

## Immutable publication codec

The `Publication` child package is a separate portable codec. Desktop implements
configuration, staging, publication and retirement queries; toolkit
integration remains outstanding. Its isolated hosted build avoids
sharing the existing protocol test's staged sources or object files:

```sh
nix develop -c bash -c '
  mkdir -p tests/desktop-protocol/build/publication/source
  cp userspace/runtime/gnat/cubit.ads \
     userspace/runtime/gnat/cubit-grant_references.ads \
     userspace/runtime/gnat/cubit-desktop_protocol.ads \
     userspace/runtime/gnat/cubit-desktop_protocol.adb \
     userspace/runtime/gnat/cubit-desktop_protocol-publication.ads \
     userspace/runtime/gnat/cubit-desktop_protocol-publication.adb \
     tests/desktop-protocol/build/publication/source/
  cd kernel
  alr exec -- gprbuild -p -P ../tests/desktop-protocol/publication.gpr
  ../tests/desktop-protocol/build/publication/publication_tests
  alr exec -- gnatprove -P ../tests/desktop-protocol/publication.gpr \
    -u cubit-desktop_protocol-publication.adb --level=2 -j1 --report=all
'
```

Read the proof summary, not just the exit status: GNATprove can return zero
with unproved contracts. The regression covers 28,074 admitted density/layout
configurations, identity limits, canonical failure replies, independent wire
examples and malformed headers. It cannot demonstrate actual reader retirement,
IPC authentication, client immutability or native-density toolkit rendering.
The native runtime build additionally checks the freestanding compiler's
language and mandatory style settings; it needs the shared build lock.

Verification checkpoint, 2026-10-01: the promoted production codec passes the
native runtime compiler (`/tmp/cubit-publication-promoted-native-final.log`) and
the persistent hosted tests/proof (`/tmp/cubit-publication-integrated-hosted.log`).
The proof summary in `build/publication/obj/gnatprove/gnatprove.out` reports
128 checks: 58 runtime, 9 assertions, 37 functional contracts and 24 termination,
with zero unproved or justified checks. All encoder round-trip contracts remain
intact. Ghost conversion lemmas and a materialized ghost decode result connect
packed-field recovery to record equality; no assumptions were added. Ghost code
is omitted from the native runtime. This proves codec properties, not service
ownership, actual reader retirement, or client write exclusion.

Earlier private manifests and incomplete proof logs are historical; the result
above is for staged copies of the shared runtime source, using the commands here.

The promoted codec also passes the rebuilt native `desktop-protocol` regression
(`/tmp/cubit-publication-integrated-native.log` and `.serial`), including the
configuration query checks and final runner fault scan. This native run uses
the unit-scale legacy backend. That earlier run predates staging and retirement
integration; see the newer grant-lifetime regression below.

## Native publication grant lifetime

The native `desktop-protocol` regression now executes 140 stage/resize/retire
cycles through real CuBit IPC and grants. It checks short-grant rejection,
foreign-owner rejection, malformed staging, monotonically increasing tickets,
duplicate candidate rejection, pending and repeatable exact retirement receipts,
stale generations, legacy attach/present exclusion and destruction with a held
candidate. The final grant revocation confirms no acquisition remains.
The rebuilt 90-second CuBit/QEMU run passes the entire protocol suite and final
fault scan: `/tmp/cubit-stage-grants-final.log` and `.serial` (grant marker 652,
final PASS 664). Source hashes are in `/tmp/cubit-stage-grants-source.json`.
The first run exposed undersized test storage after Desktop's existing minimum
size clamp; the corrected fixture respects that clamp without weakening service
validation. This is the legacy backend with unpublished candidates, not a test
of Mesa reader fences, displayed immutable client pixels or physical latency.

## Native visible publication

Native evidence, 2026-10-01: the rebuilt CuBit/QEMU `desktop-protocol` run
passes 12 visible two-buffer replacements, malformed/stale/duplicate/foreign
publish rejection, denial of visible-buffer retirement, resize ownership
preservation and destruction with both retained slots. Final grant revocation
confirms every loan was released. The independent monitor screenshot observer
checks the distinct final frame at 312×166: all 51,792 RGB pixels match, at
output position (120,130). Previous frames use different colors, so an old
visible frame cannot satisfy this check. The complete protocol suite and runner
fault scan pass; QEMU finishes its configured 90-second timeout normally.
Evidence: `/tmp/cubit-publish-native.log`, `.serial`,
`/tmp/cubit-publish-pixels.log`, and `/tmp/cubit-publish-source.json`.
This uses the legacy unit-scale backend. It proves neither mixed-output scaling,
Mesa import/fence behavior, physical scanout latency nor 240Hz operation.

`check-publication-pixels.py` observes only the monitor belonging to the supplied
serial-log path. Run it in the Nix environment alongside a locked native
`desktop-protocol` run using that same path, with a 90-second QEMU timeout.
It waits for `DESKTOP-PUBLICATION-VISIBLE: ready`, captures a PPM and checks both
color bands and their exact logical dimensions. The native fixture's four-second
sleep only permits screenshot observation; it does not establish retirement or
presentation completion. Ownership checks use explicit protocol receipts.

## Partial publication damage

The current fixture performs a 13th publication containing a 13×11 patch, after
repairing the candidate's unchanged content. The screenshot checker now checks
that patch as well as both background bands and the exact image dimensions.
The full 90-second native run and final fault scan pass in
`/tmp/cubit-partial-final.log`; `/tmp/cubit-partial-final-pixels.log` reports all
51,792 pixels matched. The earlier `publish-native` run above is historical.

The geometry test/proof is `tests/compositor/source_damage.gpr`: 191,731 interval
checks, 602,420 nearest-neighbor sample checks and 22 proved SPARK checks with
zero unproved/justified. Evidence: `/tmp/cubit-partial-integrated.log` and
`tests/compositor/build/source-damage/obj/gnatprove/gnatprove.out`. That initial
native attempt failed before boot on a shared catalog-capacity issue; the final
native log above follows its repair. These results cover unit-scale native
pixels and portable geometry, not native mixed-DPI rendering or hardware timing.

## Protected producer buffer integration

The visible fixture now uses `Client_Frame_Buffer`, backed by reclaimable owned
memory. The earlier low-level publication adversary remains as a separate test;
only the protected fixture emits the screenshot observer's ready marker. The
initial native run verified 13 frames, matching retirement before write restoration,
visible-buffer denial, failed reclamation while a reader remains, quarantine
allocation refusal, release after destruction and reuse of released handles.
The exact final image, full protocol suite and final fault scan pass in
`/tmp/cubit-client-frame-integrated.log`, `-native.serial` and `-pixels.log`.

The pure producer state project is `tests/compositor/client_frame.gpr`:
4,096 lifecycle iterations and eight proved checks, zero unproved/justified.
This is state-policy proof plus native adapter regression; actual kernel memory
protection/grant semantics and authenticated IPC remain trusted boundaries.
Ordinary toolkit applications have not yet migrated to the component.

The current fixture also uses `Client_Frame_Damage` to repaint only stale buffer
regions. Fifteen protected publications end with three 13x11 patch changes;
frame 15 must paint exactly 143 pixels while the final screenshot still matches
all 51,792 RGB pixels. The repair rectangle remains separate from publication
damage. Full protocol and final fault scan pass in
`/tmp/cubit-client-damage-integrated.log`, with serial `-native.serial` and pixel
oracle `-pixels.log`. The `DESKTOP-CLIENT-REPAINT-CHECK` marker records completion.
The persistent `tests/compositor/client_damage.gpr` has 16 proved checks and
4,096 independent pixel-model cycles, including failed painting/publication.
This validates selective repaint in the native fixture; ordinary toolkit clients
and native mixed-DPI canvas rendering remain separate integration work.

## Native fractional-density toolkit text

`desktop-check` now executes the actual UI font path offscreen at 5/4, 3/2,
2/1, 16/1 and 1/2 scale for Sans and Monospace. It compares every pixel against
fresh Rust masks and an independent integer blend oracle, including untouched
padding and clipping. It verifies that doubled text coverage differs from an
enlarged normal-size mask. Checks use explicit failure returns because native
assertions are disabled. The runner requires
`DESKTOP-DENSITY-TEXT-CHECK: PASS scales=5 faces=2` before accepting the suite.
The Makefile declares the native font-library dependency for this fixture.

2026-10-01 native run: marker662 and protocol PASS672 in
`/tmp/cubit-native-density.serial`; the complete 90-second run and final fault
scan pass in `/tmp/cubit-native-density.log`. Exact source hashes are in
`/tmp/cubit-native-density-source.json`. This establishes native offscreen text
execution, not output-scale negotiation, mixed-monitor presentation or latency.
The ordinary `UI.App` windows still need protected-buffer/configuration adoption.

## Bounded client frame owner

`Client_Frame_Pair` combines two noncopyable `Client_Frame_Buffer` owners with
`Client_Frame_Damage`. It obtains a canonical authenticated configuration,
accumulates logical repaint debt, returns only a writable candidate, and
publishes physical damage using the proved canvas edge mapping. New or resized
slots require full repaint from retained application state. There is no copy
from the visible frame. Resize reclaims only a retired writable candidate;
the other buffer remains visible until replacement. Cancelled/rejected paint
preserves debt. Close withdraws access and retains uncertain loans until
confirmed retirement. Allocation count is always at most two; each allocation
is capped by the existing 16 MiB protocol limit and allocated on demand.

The native fixture checks 12 frames, partial repair, a resize, cancellation,
incomplete-render rejection, every retained pixel, allocation bounds and
close while visible followed by destroy/reclaim. Required marker:
`DESKTOP-FRAME-PAIR-CHECK: PASS frames=12 resize=1`.

2026-10-01: native linking passed, and the explicit staged-service 90-second
protocol run passed all markers and final fault scan in
`/tmp/cubit-frame-pair-staged.log` and `.serial`. Exact kernel, initrd, plan,
fixture and owned source hashes are in `/tmp/cubit-frame-pair/staged-inputs.json`.
A fresh full build was blocked by unrelated missing `FUNCTION_VALUE` cases in
CCL Language/VM; it is not claimed here. The private staged runner skipped only
kernel/initrd regeneration, retaining all protocol and fault gates. Underlying
frame/debt/geometry/protocol policy is proved; this serialized IPC/allocation
adapter is an audited SPARK-off boundary, not a whole-adapter proof. Ordinary
`UI.App` adoption and raw-renderer density handling remain outstanding.


The separate native `files` regression now opts the real Files application into
this owner through `UI.App.Run`. Its first successful protected publication is
required by the runner, alongside all existing interaction/fault checks. The
fresh native 90-second run passed in `/tmp/cubit-app-frames-native.log` and
`/tmp/cubit-app-frames.serial`. Other applications remain on their existing
path pending migration; this does not establish configured mixed-DPI rendering
or hardware latency. See `docs/compositor-backends.md` for the current scope.


`managed-ui` is the native protected-window smoke test for Devices, Config
Inspector and Boot Logs. Build those three targets, then run:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/headless/run.sh --test managed-ui --accel tcg,thread=multi \
  --timeout 90 --serial /tmp/managed-ui.serial --keep-logs
```

It stages current binaries into a temporary disk, requires all three ready
markers and exactly three first successful protected publications, and applies
the normal final fault scan. The initial run passed in
`/tmp/cubit-managed-ui.log` and `.serial`; `/tmp/cubit-managed-ui.png` was visually
inspected. This is unit-density software presentation with overlapping windows,
not per-window exact pixels or a hardware performance measurement.


The native CCL Workbench now also paints directly into protected candidates.
The `ccl-workspace` regression passed with native first-frame/owner markers,
file dialogs, save/open, live labels and REPL results in
`/tmp/cubit-ccl-direct-verified.log` and `.serial` (120-second TCG run, final fault
scan passed). `ccl-workbench`, `ccl-workbench-virtio-vga` and `ccl-workspace` now
require the protected-publication marker. The clean one-frame SDL preview passed
in `/tmp/cubit-ccl-direct-host-final.log`; SDL rendering remains hosted evidence.

## Handled-input publication metadata (2026-10-01)

The publish request now packs its two bounded identities into word 1 and carries
the full 64-bit handled-input watermark in word 2. The permanent hosted test
and proof pass against the updated production sources: 129 analysis results
(58 runtime, 9 assertions, 38 functional contracts, 24 termination), zero
unproved or justified checks. The tests retain the 28,074 layout cases and add
576 individual watermark-bit round trips, an all-ones watermark, an independent
wire vector, invalid packed halves and explicit old-layout rejection. Evidence:
`/tmp/cubit-provenance-publication-permanent.log` and the proof report above.
Coordinated native rebuild and the 90-second `desktop-protocol` regression
pass, including visible publication, frame-pair checks and final fault scan
(`/tmp/cubit-provenance-native-complete.log` and
`/tmp/cubit-provenance-protocol.serial`). This metadata
does not establish visible response or input-to-photon latency.

## Native bounded input overflow

`desktop-check` now includes a deliberately stalled per-surface consumer:
32 alternating resize/configure events must remain ordered with exact payloads,
serials and `More_Pending` flags. Four batches of 33 unconsumed transitions must
each collapse to one explicit resynchronization event. It verifies the known
no-device-input snapshot (boot cursor at 80,80 minus the current client inset 4,30, giving
client-local 76,50; no held buttons/modifiers), no
stale replay after consumption, and immediate admission/delivery of a fresh
configure with the next serial. A second surface keeps its initial configure
through all four overflows, checking per-surface isolation through real IPC.
Synchronous resize acknowledgments establish admission without timed sleeps.

This is a deterministic queue-capacity and recovery gate, not a keyboard-rate,
source-mailbox overload, visual-response, throughput or physical-latency test.
The plain surfaces start at the origin and the headless protocol profile does
not inject device input; an interactive run violates the snapshot fixture.
Native session 74508 exited 0 after rebuilding the corrected fixture and running
90 seconds of four-vCPU TCG. Recovery serials 66,100,134,168 each carried local
position 76,50 and zero held state. The dedicated saturation marker, complete
protocol profile and final fault scan passed; nine source/staged/kernel hashes
were verified in `/tmp/cubit-input-overload-inputs.json`. The original failure
(36423) remains in `/tmp/cubit-input-overload-before-*`; it rejected an incorrect
desktop-coordinate expectation, corrected without weakening the snapshot check.
Logs: `/tmp/cubit-input-overload-coordinate-native.log` and `.serial`. This result
precedes the elapsed-time admission change to Desktop. The integrated rerun
(session 38145) also passes the full protocol/saturation/fault gate with the same
four recovery records. Evidence: `/tmp/cubit-dispatch-native.log`,
`/tmp/cubit-dispatch-protocol.serial` and `/tmp/cubit-dispatch-native-inputs.json`.

Build with
`make -C kernel desktop-check` in Nix, then run the existing `desktop-protocol`
headless profile under `coordination/build.lock`. Require both its dedicated
`TEST: PASS desktop native input saturation and recovery` marker and the overall
profile/final-fault-scan PASS; a source change or successful compile is insufficient.
