# ADL-N reset and GPU-address ownership

## NUC regression and candidate-profile correction (2026-10-08)

User reports the offered latest image reaches Desktop quickly but has severely
laggy pointer motion, redraw artifacts during window movement/resizing and
nearly unusable log scrolling. Photo shows `DESKTOP-OPTIONAL-RENDER: PASS empty
slot`, `setup unavailable stage=admission`, `startup=SOFTWARE`, and Intel viewer
target delivery/backing TRUE with runtime-fault FALSE. This does not establish
the cause of input latency or redraw corruption, or exonerate the display driver.

The offered frozen `intel-matched-stepped-nu4ho_n9` normal UEFI image profile
is `images/laptop-usb.ccl`, selecting `tests/hardware/init-usb-live.ccl`. Its
Desktop start omits `(render approve-declared)` while Desktop's manifest asks
for optional render. `CuBit.Render_Startup.Initial_Attempt` therefore selects
Software_Only. The photographed empty slot matches that deliberate policy;
it is not evidence of a GPU allocation/pipeline rejection. The earlier READY
request below was incorrect for this normal profile. The separate
`init-desktop-mesa-startup.ccl` approves render explicitly. A future hardware
candidate must verify its actual embedded startup policy as well as binaries.

The existing `--desktop-mesa-startup` wrapper option is also not a GPU-drawing
compositor profile: `verify-desktop-mesa-startup.py` deliberately requires
`gpu_drawing_enabled=False` for that historical initialization checkpoint.
Do not remove that guard or label its output accelerated. A drawing-enabled
candidate must pass `tools/verify_desktop_vulkan_compositor.py` (which requires
the drawing-enabled runtime-dispatch artifact and source/binary identities),
AND have explicit render approval in the startup actually embedded in the
image. Neither check establishes execution, successful admission, or hardware
presentation; those need separate runtime evidence. The live wrapper now has
`--desktop-vulkan-compositor`, requiring `CUBIT_DESKTOP_VULKAN_DIR` and producing
the separately named `cubit_live_desktop_vulkan_compositor.img`. It reuses the
explicit-approval startup profile (including its separate triangle-window demo)
but selects a drawing-enabled Desktop through the compositor identity guard.
This option has hosted routing/rejection coverage, not a newly built or
hardware-tested image. Keep the matched runtime/Mesa/driver checks and embedded
payload inspection when building a real candidate; a directory name alone is
not sufficient provenance.

Do not recommend this candidate as a responsiveness baseline. Do not claim
software fallback explains the magnitude or corruption: compositor is examining
the frozen damage/presentation/input path against newer fixes. IPC attribution
requires evidence. No new image or rollback has been made. The source findings
assume the user ran the offered SHA256 84b099392c19... candidate; confirm the
filename if their stick came from another image.

## Source progress after the frozen candidate (2026-10-08)

Public retirement quiescence now shares internal teardown's deferred-work
predicate. A pending buffer retirement, selection, preparation, public/private
allocation, VM update, in-place update or table-ledger operation conservatively
keeps the reply pending, including work belonging to another session. Previously
the query checked a narrower allocation/preparation subset. Existing GPU,
translation, CPU-grant and cleanup checks remain; OK still grants no physical
reuse authority. Exact-source hosted coverage passed all 256 combinations and
the coordinator lifecycle suite; native Intel compile/link/staging passed
(44621, terminal 0; hosted evidence `native-cleanup-j5c12_na`). No frozen NUC
image replacement or hardware-rendering claim.

### Remaining lifetime-admission bottleneck: source audit

Buffer metadata growth/reuse does not remove the render-session lifetime cap.
`Intel_GPU_Render_Sessions.Reserve` rejects at `Used = Capacity` (16), and
`Close` preserves both Used and the issued identity. Independently,
`Intel_GPU_Render_Control.Handle` stops reservation when Next_Recipient_Slot
reaches 56 after assigning slots 40 through 55. Aborted reservations consume
these resources too. This is a lifetime limit, not a concurrent-client quota.

Do not recycle either counter or interpret `Handle_Retirement_Query` OK as a
metadata-reuse certificate. That handler explicitly establishes quiescence,
not backing reuse. Its checks cover the session sweep, deferred allocation,
context preparation, GuC deregistration, image-address release and CPU grants;
the actual backing teardown paths maintain additional allocation tickets,
translation evidence and parent-slice retirement state.

The implementation boundary for removing this cap must include all of:

- Session identities remain monotonically issued and never reused; stale
  stamped requests must not resolve after storage is reassigned.
- Authenticated recipient capability retirement/replacement must be confirmed
  independently of CPU mapping-grant retirement; a new endpoint must match
  the new recorded incarnation before activation.
- Preserve per-session private context, images, registration/retirement flags,
  channels, submission state and cleanup cursors until their exact outstanding
  work and backing tickets finish. These currently share the registry index.
  Growing only the registry would leave the other arrays incorrectly bounded.
- Retained closed quota records cannot be treated as live-client capacity;
  their outstanding charges remain until exact ticket retirement refunds them.
- Stress more than 16 sequential admissions, overlapping live sessions,
  failed activation, failed delivery, pending GPU/TLB/CPU retirement and stale
  endpoint messages. An uncertain retirement must retain state, not free a slot.

Next implementation needs coordinated growable session-associated storage and
an explicit endpoint-slot lifecycle, rather than a larger fixed admission cap.
No recycling was enabled by this audit; the existing conservative boundary is
still present and the full driver goal remains incomplete.
Hosted `render_control_tests` passed under Nix (61064, terminal 0), including
authority, incarnation, activation, late replies, retirement and exhaustion.
This checks existing behavior; it does not demonstrate admission beyond 16.

### Verified buffer allocation and cleanup work

Native indexed-handle replacement regression passed (70879, terminal 0):
128 cycles reuse a stable allocation slot with advancing ticket generations
and fresh names, while a neighboring real self-grant remains acquired. Stale
names cannot map or close, retired grants cannot be reacquired, and retained
neighbor contents survive each replacement. The subsequent closed-admission
sweep visits 193 mapping records in thirteen bounded calls and still cannot
dismiss the two held readers. Evidence: `tests/intel-gpu/demand-backing.GrOPfF`
serial log and input hashes, using the existing kernel. This is CPU-only
native lifetime evidence, not GPU completion or physical arena release proof.
No production source, runtime, or NUC image was changed by this test extension.

The bounded name index is now integrated into `Intel_GPU_Buffer_Handles`.
Name lookup no longer scans all records. Each stable, growable buffer record
also stores an independent index node; index payload relocation cannot move
the buffer or its retained references. Replacement retains existing retirement
and ownership checks, consumes a fresh monotonically issued name, removes the
old key and preserves the current node payload at the buffer's slot.

Full handle/request/quota/stepped-cleanup suites passed (61739); an additional
129-buffer, 1,024-cross-owner-replacement regression passed (92957), checking
every neighbor and a retained pin after each replacement. Full sharing with
4,096 presentation cycles and the exact-source native coordinator passed
(12474). Native Intel compile/link and real-grant mapping tests passed (53432),
with evidence in `tests/intel-gpu/demand-backing.ZbJ9sn`. Explicit test dependency
lists were updated under the shared lock. No hardware image was replaced.
Backing-overlap admission and other observers still scan records; this is not
a claim that all allocation work is bounded or that GPU rendering is verified.

A bounded handle-name index primitive now passes hosted stress tests (34688):
2,048 caller-grown metadata slots, 8,192 replacements, stale/duplicate names,
sequential keys and malformed-link quarantine. Its 32-bit radix paths bound
lookup to33 node reads, insertion to35 and removal to68; removal relocates index
payloads, not named buffer records. This primitive-only result preceded the
integration above. The read-count bound is not a measured frame-rate speedup.

The session registry and controller endpoint slots also retain a separate
16-admission lifetime limit. Growable buffer metadata does not solve that
limit. Safely removing it needs session/context/endpoint lifecycle work, not a
larger fixed array or reuse based solely on the cleanup sweep's completion.

Native stepped CPU-grant teardown now has real-kernel evidence (28218).
`mapping_growth_check.adb` closes its trusted admission before taking buffer
and mapping snapshots, then advances the actual primitives while readers remain
held in inline and expanded metadata. The 65-record mapping prefix completes
in five calls (16/16/16/16/1); every call retains Outstanding status and prevents
buffer retirement. Only returning both readers and bounded polling makes the
CPU-grant observation Clear and the closed buffer eligible for the next gate.
Evidence: `tests/intel-gpu/demand-backing.ylx0OC/serial.log` and `input.sha256`.
This uses actual CuBit self-grants and the existing recorded kernel; it is not
cross-process isolation, a full Intel-service boot, physical reuse, GPU/TLB
retirement or rendering proof. No hardware image changed.

Teardown now verifies closed admission explicitly before taking its snapshots.
`Render_Control.Retired_Admission` uses the controller's recorded recipient
identity and accepts only a known retired session; reserved, active, unknown
and quarantined observations fail closed. A native caller that skipped or
failed its close cannot start a sweep: the coordinator quarantines instead.
Hosted lifecycle and exact-source coordinator tests55509 passed, including a
live fourth-session rejection while another cleanup is queued; the rejected
session's cursor/context stays untouched. Native compile/link14018 passed.
These are regression and compile results, not SPARK proof or hardware evidence.

Cleanup scheduling also tracks outstanding sweeps so the native loop does not
insert its 10 ms idle wait between runnable cleanup steps. The count increments
once per session and decrements only after its mapping sweep; duplicate closes
cannot double-count. Completed sweeps with outstanding grants return to normal
polling, and a runtime fault cannot make this scheduling predicate busy-spin.
Final hosted coordinator regression93673 passed, including a third queued
session during fault injection (`.vm-intel-o4CEuh/native-cleanup-uiz9hajw`).
Native compilation/link71614 passed against runtime publication162. The native
two-process accounting test18514 also passed with real IPC and retained quota
checks (`tests/intel-gpu/demand-backing.OUA9vD/serial.log`, `input.sha256`), using
the existing recorded kernel binary. Neither test validates Intel rendering or
the full service's teardown on hardware. The matched Mesa/Desktop stack has not
been rebuilt into a new image.

Native `Retire_Application_Resources` now begins limited allocation/name and
mapping sweeps instead of draining them in the close handler. The serialized
loop visits one existing stable session slot per turn, advancing at most one
32-name, 32-allocation-record, or 16-mapping chunk. Duplicate notifications do
not restart the cursor; an unknown nonzero session quarantines admission.
Retirement queries and physical teardown checks additionally require sweep
completion, without replacing GPU, translation, CPU-grant or backing receipts.

Native compilation/link/staging passed (16582). The hosted exact-source test
`tests/intel-gpu/test-native-cleanup.py` passed (14518), compiling coordinator
blocks extracted from `main.adb` with real session/allocation registries and
mock grants. It covers 65 names, 49 retained views, two-session progress,
duplicate closure, late completion, retained charges and unknown identities.
Generated fixture/output: `.vm-intel-o4CEuh/native-cleanup-nhxl60t0`.
This is not a native boot or hardware-retirement test. The native build preceded
compositor runtime publication162; the next matched stack must rebuild against
that runtime. No NUC image was replaced.

Late allocation completion no longer synchronously closes all names when
`Begin_Retire_Session` has already cancelled that ticket. It returns Denied
without publishing a handle; the existing retirement cursor remains responsible
for the name sweep. A new 65-name hosted regression verifies no early closures,
32/32/1 stepped closure, and retention of all 66 allocation charges, including
the cancelled allocation. The full buffer-request and client-quota suites passed
(45182); native Intel compilation/link/staging passed (49293). The separate
authority-loss path without begun retirement still closes synchronously.
The coordinator integration above uses these steps. This does not make every
service path bounded or prove GPU retirement.

The candidate below is unchanged. Later source changes add own-account query
0A30, its Mesa client adapter, and role-specific reusable-slot lists for
application buffers, private page tables and context parents. These lists
replace allocation-time capacity sweeps; only the existing exact confirmed
retirement paths can insert entries. Quota refusal leaves the list intact,
and reused tickets still advance generation and bind to the new owner.
This is not a larger fixed slot limit or permission to recycle unretired memory.

Hosted buffer-request and client-quota suites passed (10479), including multiple
retired nodes, stale/duplicate acknowledgements, cross-session reuse, quota
refusal, growth and existing repeated lifecycle regressions. Native Intel
compile/link passed against the newly published metrics runtime (7493).
The two-process accounting fixture also passed with real IPC and DMA backing:
`tests/intel-gpu/demand-backing.aiQJL1/serial.log` and `input.sha256`.
That disposable test uses the production adapter and accounting handler, not
the full Intel service or GPU; it does not establish GPU retirement or rendering.
The existing hashed kernel was reused, not rebuilt.

Session teardown still contains capacity-sized work. Complete bounded teardown,
GPU/TLB/CPU/grant/display retirement evidence and the next physical NUC run remain
outstanding. Future image packaging must rebuild matched Mesa/desktop components
for the updated runtime; the current staged Intel binary alone is not a matched
image. No commit, push or replacement of the offered NUC image was performed.

Teardown audit: `Retire_Application_Resources` invokes context retirement, then
`Buffer_Requests.Retire_Session` (handle and allocation sweeps), then
`Sharing.Retire_Session` (mapping-grant retirement requests). The own-close path
closes render admission before entering this sequence. Yielding requires each
phase to retain progress and must not report completed GPU/grant retirement
merely because all name records have been visited.

The first primitive, `Buffer_Handles.Close_Session_Step`, now visits at most32
records per call using a caller-retained count snapshot/cursor. Hosted95127
passed65 records in32/32/1 visits, foreign-owner preservation, invalid snapshot
refusal and retained-reader lifetime checks. Its caller must already deny new
admission/issuance for that session. The existing synchronous close drains this
same primitive; native event-loop yielding is not implemented yet. This result
is regression-tested behavior, not a SPARK proof or hardware-retirement result.

`Buffer_Requests.Begin_Retire_Session` / `Retire_Session_Step` now retain the
service identity and captured handle/allocation bounds. Begin closes the client
budget and cancels pending allocation immediately; each step visits at most32
names or allocation records. Existing synchronous retirement drains the same
core. Hosted63811 passed66 allocation records, active-restart/foreign-root
refusal, immediate cancellation, other-session preservation and retained charges,
plus the existing request/quota suites. This was the primitive-only milestone;
the native coordinator and cancelled-completion integration are recorded above.

Mapping teardown now also has a root-bound snapshot and16-entry steps. Hosted
20039 passed49 views in16/16/16/1 visits and verified that sweep completion is
still Outstanding until grant retirement is confirmed. The full sharing suite,
including4096 presentation/recycling cycles and metadata growth, passed; tests
that assumed one Poll swept the whole table were corrected to use bounded full
passes and explicitly check incomplete drainage. Native Intel compile/link
passed with all new teardown primitives. Existing synchronous wrappers still
drain them for explicit synchronous callers; the native close coordinator now
uses the stepped interfaces as recorded above.

## Matched stepped-allocation candidate (2026-10-08)

New, separately retained image:
`.build-workspaces/intel-matched-stepped-nu4ho_n9/kernel/cubit_live_uefi.img`.
SHA256: `84b099392c19f08503d2cf2fbd9f1a6e4df35415f671400d4f1167d4db883dcb`.
The previous `build-graphics-tests/cubit_intel_matched_deadline_candidate2_20261007.img`
is unchanged. No commit or push was made.

This frozen snapshot contains the resumable GPU VM-growth preparation, hidden
adoption and rearm work, with ownership checks and stable metadata identities;
it does not replace dynamic allocation with a larger fixed slot array. It also
contains the matched explicit-deadline runtime and compositor startup-stage
diagnostics. Runtime/libc, Intel driver, combined Mesa, desktop, kernel and the
normal live-image service targets were rebuilt. The snapshot manifest records
seeded artifacts separately; this is not a claim that every bundled application
was rebuilt (notably the seeded browser). It predates the compositor's later UI
hit-map/minimum-window changes. The only private build-script correction uses
an explicit Nix `path:` input in `userspace/libc/build.sh`; no Git mutation was
used to make the snapshot visible to Nix.

Build session16833 completed successfully. Image audit91425 verified firmware,
notices and bootstrap membership, and compared the embedded binaries byte for
byte with the private builds:

- Intel driver: `00a99eaa0e278b1a21c8d892d063f774ac580c5889e4ca275f8a47a72b91a866`.
- Desktop: `ba9e3bd471132ef2bfc843159ba3b3f48015d680b21f769e5b0e16cc2d56aa72`.

Exact-image QEMU/KVM test73316 passed with UEFI, four CPUs, i8042 disabled,
USB keyboard/mouse behind a hub, quiet xHCI, boot-log delivery and Mesa testing.
Evidence is retained under
`.vm-intel-o4CEuh/nix-shell.1CCWzn/cubit-usb-live.k2rtu6t8/`.
The pointer opened/closed Apps twice; keyboard-driven application launches
worked; the software cube passed194673 geometric pixels, animated retired-buffer
reuse, pause and Escape/exit. Mesa and DOOM screenshots were visually inspected;
DOOM shows its rendered scene/menu, not a sustained-gameplay benchmark. The
image hash was unchanged after the test. USB harness fixes remove dependence
on disabled statistics/enumeration traces, not functional acceptance checks.

This run explicitly reported `setup unavailable stage=admission` and
`startup=SOFTWARE`. It validates native CuBit integration and software fallback
under QEMU, NOT physical Intel rendering, GPU retirement, modesetting or audio
quality. The full Intel goal remains incomplete.

### Exact next NUC request

1. Boot this exact image at1080p or lower; record elapsed time to a responsive
   desktop. Leave the existing known image available for recovery.
2. Test keyboard navigation and an Apps click through the usual Das Keyboard
   USB hub. Cursor movement alone is not input acceptance.
3. This normal profile selects software-only (see correction above). Photograph
   `DESKTOP-VULKAN: startup=SOFTWARE`, and the
   first `setup unavailable stage=...` or `pipeline failure stage=...` line.
4. Capture any allocation-denial reason, backing stage, quota/bytes and metadata
   growth records. Do not infer successful hardware rendering from growth alone.
5. If READY, exercise window movement/overlap and opening/closing applications;
   report corruption, hangs and stutter cadence. This image's menu cube is
   explicitly software-rendered; it is not a hardware teapot test.

Independent remaining work includes bounded removal/reclamation, complete
GPU/TLB/CPU/grant/display retirement before physical reuse, owner-death and
exhaustion tests, and native Intel modesetting/EDID. Hosted metadata test28291
passed optimized typed initialization and old-record preservation across sparse
growth; that regression is not a lifetime proof or hardware result.

## Render permissions / application setup image (2026-09-30)

Private image: `.build-workspaces/graphics-permissions-ef2lftob/kernel/cubit_live_permissions.img`.
SHA256: `9f43c07daf990dc53fa19004f2b719d9390000c142ba48a2a40d4a2ae33d3dc8`.
Extracted `/apps/intel-gpu.drv` matches the freshly built driver byte-for-byte:
`cfda78abba12c41cf5b13227509f3c6f854821764192694d914577843245fa04`.
Snapshot records sources and reused service seed binaries; kernel/runtime and
Intel driver were rebuilt, not all services. Existing frozen images preserved.

Includes explicit twelve-slot render permission initialization and the
application setup/marker/disable handler. Application admission remains closed,
so the latter is compiled but not exercised by this image's ordinary boot.
No writes to the eight additional slots listed in the TGL PRM are enabled.

Build5550, packaging14554 and extracted-driver audit95893 passed. QEMU15535
passed USB-flash/UEFI/four-CPU Desktop and native Mesa software rendering:
194673 geometric pixels, animation and close. Logs: `/tmp/cubit-usb-live.j4fow66h`.
The first test49197 terminated before testing because the private TMPDIR made
the Unix socket path too long; the successful rerun used `env TMPDIR=/tmp`.
This is native CuBit software rendering under QEMU, not Intel acceleration.

NUC checkpoints: `render engine settings READY`, draw completion/pixel match,
`native TLB invalidation COMPLETE`, then `updated-VM batch published=TRUE
completion=COMPLETE disable=COMPLETE; alias=00208000`. Capture the first failing
stage if one is absent. No physical result is recorded for this image yet.

## CT roundtrip NUC image (2026-09-28)

Published `kernel/cubit_n95_ct_roundtrip.img`, SHA256
`5410f00bef02792ea17955cc4f09d0aa02f799d8b272d3f27b935ba74b4e5c4e`.
Job95357 exit0: native Intel/devmgr link, devices regression, live image
firmware/Mesa payload audits and QEMU USB/UEFI4CPU with hub/noPS2 passed.
Boot diagnostics automatic launch and Apps-menu relaunch/replay passed;
native Mesa SOFTWARE pixels/animation/close passed (194673 geometric pixels).
Logs `/tmp/cubit-ct-roundtrip-{linked-build,devices,image-build,image-test}.log`;
QEMU evidence `kernel/build/tmp/nix-shell.LtVTic/cubit-usb-live.1q73x4ri`.

This image adds PAT setup, corrected four-word SELF_CFG envelopes, native CT
send/receive and the logging-control roundtrip. On NUC open Apps -> Boot
diagnostics and report PAT setup, CT registration and CT roundtrip lines,
including result/reply. Expected final exchange success is COMPLETE with
reply F0000000. Failure retains backing and disables further runtime access.
QEMU does not test the Intel path; no NUC roundtrip is confirmed. The PPGTT
encoder/initial VM builder remain offline and this is NOT accelerated3D.

## Offline initial GPU VM (2026-09-28)

`Intel_GPU_Initial_VM.Build` prepares a sparse four-level tree for one
2MiB-aligned window of1..512 4KiB pages. It requires four distinct admitted
table DMA pages and rejects every table/data alias before populating entries.
Unspecified entries remain zero; rejected plans contain no usable root or
entries. The caller still must authorize and retain all backing, copy tables
to device-visible storage, establish visibility and register an engine context.
No live mapping, publication, invalidation or general VA allocator is supplied.

Hosted tests8472 exit0 cover all512 window lengths at the highest raw48bit
window, exact sparse entries, all table-pair aliases and every table/data
alias position. SPARK proves runtime checks, termination and the invalid-plan
zeroing/root postcondition. Follow-up92975 exit0 additionally proves validity
iff the exact admission predicate holds, including DMA validity, window
alignment/length, access mode and all table/data nonaliasing requirements.
Exact sparse contents remain regression-tested, not fully functionally proved.

## Initial PPGTT encoder (2026-09-28)

`Intel_GPU_ADLN_PPGTT` encodes4KiB system-memory leaves and cached directory
entries, with the existing below4GiB DMA admission policy (not a hardware
limit). Cache choices match PAT indices0..3. It decomposes raw48bit GPU
addresses into four512-entry levels plus page offset. No table is allocated
or published yet; ownership/lifetime, context registration, invalidation and
cross-context isolation remain separate requirements.

References: [Intel entry definitions](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gtt.h.html)
and [Linux encoder/RO restriction](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/gen8_ppgtt.c.html).
Linux disables read-only support for Gen11/12 due to an unresolved fault issue.
The CuBit encoder therefore rejects RO instead of upgrading it to RW. Do not
use that bit as an application isolation guarantee.

Hosted tests cover every admitted DMA page under all four cache policies,
unaligned/out-of-policy rejection, RO rejection and every index at each level.
SPARK proves entry postconditions, bounded indices, exact address reconstruction,
arithmetic safety and termination (29653 exit0). Earlier modular decomposition
proofs were incomplete; non-wrapping signed integer decomposition retains the
same reconstruction contract and proves it. This proves software encoding,
not hardware behavior. No live image change.

## Native CT logging-control roundtrip (2026-09-28)

Additional integrated hosted test11681 exit0 connects both native memory
adapters, CT framing and roundtrip logic to a simulated firmware notification
handler. It checks literal request words, simultaneous ring wrap, interleaved
events, success/wrong-fence/error replies, retained event independence after
ring reuse and one-shot behavior. This is CPU-mapped-memory integration, not
a concurrent device/coherency test. Shared build remains pending.

Main now wires the native H2G adapter and the bounded roundtrip helper after
successful registration and initial receive capture. One request sends
logging-control `[0x40, 0]` with fence42; this disables debug logging rather
than inventing an unsupported ping. Success requires one DWORD `0xF0000000`
with the matching fence. At most8 unsolicited events are retained as owned
messages, not dispatched. BUSY stays within the fixed1s deadline; RETRY is
reported without replay. A poll cap also bounds a stalled clock. Any failure
latches the runtime fault and retains backing. There is still no ongoing
event pump, engine submission or GPU rendering.

Hosted helper tests (17 scenarios) passed in93809; private native compilation
passed in38374. The shared link/devices attempt was lock-unavailable (exit1),
so this is source/private-compile evidence only, not a new image or hardware
roundtrip. Next shared link, QEMU regression and distinct NUC image.

## SELF_CFG envelope correction (2026-09-28)

Both address and size KLVs now use the fixed four-DWORD SELF_CFG request.
Previously size requests incorrectly used three words; KLV_LEN=1 describes
the value, not the message envelope. The fourth word remains zero.
References: [Intel ABI](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/abi/guc_actions_abi.h.html)
and [Linux sender](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#859).
Isolated hosted setup/registration tests and setup SPARK checks passed (job
56328, exit0). This is not evidence of hardware acceptance. Published image
has not been rebuilt and still contains the old size-request length.

Native CT send adapter also has passing hosted mapped-memory tests, but is
not instantiated in main yet. For the first CT exchange, investigate the
documented logging-control action (0x40, one control word), which Linux sends
over CT in [guc_action_control_log](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_log.c.html#204).
Do not use SELF_CFG as a CT ping: it is explicitly MMIO-only. GET_HWCONFIG
also uses MMIO upstream. Response matching, bounded event handling, deadlines
and response credits remain necessary before wiring a live request.

## Initial native receive capture (2026-09-28)

Main now instantiates the native G2H adapter and framing receiver after CT
registration succeeds. Its ownership gate checks the exact retained CPU base,
32KiB extent and registration/runtime readiness (including PAT). A bounded
initial pass captures at most8 messages into owned retained copies. Each
successful frame publishes only the host head cursor. Corrupt/quarantined
results latch runtime failure. No payload is dispatched or interpreted as
an address/completion. Captured length/fence/first HXG word are diagnostic only.

An empty ring is logged explicitly as NOT a firmware round-trip; we have not
sent a request that obliges a response yet. Frames arriving after this initial
pass need the forthcoming service receive loop/interrupt integration. The
bounded capture does not replace that production receive pump.

Private native compilation29086 PASS (`/tmp/cubit-ct-receive-native-main.log`).
Shared link/device attempt exited1 because the network test lock is occupied;
no shared build/test launched. Published CT image remains unchanged and lacks
both native PAT setup and initial receive capture.

## Native G2H memory adapter (2026-09-28)

`Intel_GPU_Native_CT_Receive` supplies the existing CT receive parser's
callbacks over the fixed CT layout. It reads aligned volatile32-bit fields,
validates all13 descriptor reserved words, orders payload reads after tail
observation, and publishes only the host-owned head DWORD. Coherent x86 memory
barriers replace no cache-policy setup: the caller must attest the registered
retained mapping's coherency and ownership. No CLFLUSH is performed while
firmware may be writing, and no whole-descriptor store can overwrite its tail.

Hosted6169/2247 PASS (`/tmp/cubit-native-ct-receive.log`): every4096 ring index,
invalid indices, all416 reserved-bit corruptions, actual parser consumption
and head update on mapped host RAM, wraparound, and ownership loss before/
after stores. These are not GPU cache-coherence tests. The adapter is not yet
instantiated by main; native receive dispatch/credits still need integration.
Published CT-registration image is unchanged.

## Native PAT setup integration (2026-09-28)

Fixed control-page policy now includes4000 at index9/slot34/CPU61209000.
`Intel_GPU_Native_PAT` permits only eight aligned registers4800..481C and
two-bit values0..3, with volatile32-bit accesses and ownership checks before
and after. Fenced stores are not device acknowledgement; setup reads back.
Whole-page capability authority remains broader than the software selector.

Main configures PAT after successful reset and retained display/DC ownership,
after GGTT control mapping, but before any new GPU buffer publication. It
admits only46D2 and an active one-shot setup window with no firmware/ADS/log/CT
mapped. Publication and subsequent startup/runtime ownership now require
PAT_Ready. Failure leaves buffers retained and prevents publication. Diagnostic
`PAT setup` reports result/index/raw. Existing firmware scanout is retained;
this initial-boot path is not a runtime cache-policy transition API.

14916 private native Intel+devmgr compilation and page/PAT tests PASS
(`/tmp/cubit-native-pat.log`).82559 exhaustive native-adapter offset tests,
host stores, denied values/ownership loss, and page-policy SPARK checks PASS
(`/tmp/cubit-native-pat-access.log`). These do not prove GPU coherency.
Published CT-registration image predates this integration and is unchanged.

## ADL-N private PAT setup helper (2026-09-28)

Upstream Gen12/IP12.0 selects `tgl_setup_private_ppat`: registers4800..481C
are programmed3,1,2,0,3,3,3,3 (WB,WC,WT,UC,WB,WB,WB,WB). CuBit currently
has no explicit corresponding native initialization. Its address|present
GGTT encoding agrees with upstream `gen8_ggtt_pte_encode`; adding PPGTT or
Meteor Lake PAT bits to these PTEs is not the fix.

`Intel_GPU_ADLN_PAT` now implements a bounded one-attempt setup helper, reads
before writing, skips matching values, checks posting readback, and stops on
access/ownership/readback failure without rollback. Caller must provide
ADL-N main-GT identity, retained forcewake and quiesced/pre-publication state.
This is not a live cache-policy migration operation. No native instantiation
yet: next grant/map page4000, snapshot PAT, integrate before GuC publication,
and establish fresh post-reset ordering while preserving display scanout.

Hosted72441 PASS (`/tmp/cubit-adln-pat.log`): exact addresses/values, every
read failure and ownership loss, each write failure/lost write, matching policy
without writes, and no retry after admission. No SPARK/hardware-coherence claim.
References:
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gtt.c.html#540
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gt_regs.h.html#364
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gtt.h.html#145
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html#281

## CT receive coherency audit (2026-09-28)

Before native receive integration, inspected actual allocation and upstream
mapping contracts. CuBit `Syscall.IPC.allocateDma` maps the retained allocation
with `PG_USERDATA`; the retention argument does not select an uncached mapping.
`Intel_GPU_DMA_Cache.Flush_Range` explicitly requires exclusive CPU ownership
and cannot simply be reused on descriptors concurrently modified by firmware.

Linux `intel_guc_allocate_and_map_vma` calls `intel_gt_coherent_map_type` with
always_coherent=true. The latter selects WB except local memory / Media13.0
workaround cases, so normal cached CPU RAM is not itself a demonstrated bug
for this ADL-N main-GT target. Need verify the matching GPU/PAT/snoop policy,
not infer coherence merely from the CPU mapping or an mfence. References:
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#834
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gt.c.html#998

The CT ABI descriptor is64bytes: head, tail, status, then13 reserved DWORDs
which must be zero. Host owns G2H head and H2G tail, firmware the opposite
cursors. This shares cache lines between writers; do not implement a whole
descriptor read-modify-write or use a stale snapshot to publish one cursor.
Reference:
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/abi/guc_communication_ctb_abi.h.html

Next native receive adapter must perform individual aligned volatile32-bit
accesses, reserved-word validation, acquire ordering after tail observation,
read completion before head publication and device-visible release ordering.
It must not flush the entire allocation after registration (firmware may also
be writing log storage). Published CT-registration image unchanged: no host
ring accesses are enabled yet. Cache visibility remains hardware-unverified.

## CT-registration hardware test image (2026-09-28)

`kernel/cubit_n95_ct_registration.img`, SHA256
`e1e8c1d300b41158d4ad80c9a3fca30fd819d80bcd73392243438fc56cd94ec8`.
Build/payload audit and QEMU USB/UEFI four-CPU, hub, no-PS2, BootLogs automatic
and menu launch, Mesa software pixels/animation/close all passed (31498).
Logs `/tmp/cubit-ct-image-build.log`, `/tmp/cubit-ct-image-test.log`.
Previous images preserved. No Intel hardware is emulated by this QEMU test.

On NUC inspect Boot diagnostics for GuC startup phase/raw result, then CT
registration result, step and reply/transport. Successful registration logs
`CT enabled (NOT engine submission-ready)`. If startup fails, CT must not run;
capture the earlier startup/transfer evidence instead. This image executes
native firmware upload and CT registration when prerequisites pass; it does
not yet submit engine contexts or hardware rendering commands.

## Native CT registration wiring (2026-09-28)

Main now instantiates the runtime mailbox adapter, bounded MMIO HXG transport
and seven-step CT registration executor. It admits this path only after native
GuC startup reports Firmware_Ready and CT publication succeeded. Ownership
checks retain existing display/reset/forcewake gates, compare the current CT
CPU/DMA/size view to the captured published view, check ADS initialization, and
read/decode fresh GuC status before mailbox accesses. A runtime fault is sticky.

The mailbox has a one-million-poll cap per exchange in addition to its elapsed
time limits. Registration failures retain backing and mapping claims without
retry. Diagnostics expose phase, step, reply and transport status. CT-enabled
is explicitly NOT engine-submission-ready: receive handling, contexts and
rendering commands are still absent from the native path.

Private native compilation and all three focused mailbox/registration tests
pass (26871, `/tmp/cubit-ct-native-registration.log`). No Intel hardware reply
has yet verified this wiring; the previously published image is unchanged.
Shared Intel/devmgr linking and the QEMU devices regression also pass
(24215, `/tmp/cubit-ct-linked-build.log`, `/tmp/cubit-ct-devices.log`).

## Native CT backing and publication (2026-09-28)

The firmware allocation now reserves a disjoint32KiB CT slice at offset0x84000,
after the512KiB firmware and16KiB log regions. The existing whole1MiB zero,
readback and cache-flush pass initializes both descriptors and rings, including
padding. Compile-time checks require page alignment, sufficient CT layout size,
disjoint regions and containment. `Prepared_CT` exposes only retained CPU/DMA
backing once that preparation succeeds; it never reinitializes live memory.

Native main publishes this slice through the same runtime GGTT reservation
ledger used for ADS/log, with its own one-attempt publication object and exact
backing checks. Protected scanout ranges and existing mappings remain excluded.
Its local write binding is revoked after publication, but backing/reservations
remain retained, including partial-publication failures. A diagnostic reports
CT mapping status/address explicitly as NOT registered.

Private native compile54763 passed (`/tmp/cubit-ct-backing-native.log`). This
is compilation evidence only; no new image or hardware test yet. Next connect
the native mailbox and registration executor after firmware startup succeeds.

## CT registration executor (2026-09-28)

`Intel_GPU_GuC_CT_Register` now executes the existing codec's six buffer KLV
registrations followed by CT enable. It constructs the plan internally from
validated numeric inputs; the caller must independently attest initialized,
flushed, GPU-published backing and a running authenticated GuC through the
ownership callback. Transport success alone is insufficient: registration
requires the exact recognized-key response, and enable its exact success.

The limited registration object records admission, last step and last reply.
Once admitted, it cannot be retried even after a negative response. No rollback,
disable or freeing is attempted: firmware may retain partially registered
addresses, and a failed enable response may conceal an enabled channel.
The exchange callback must be bounded (the intended adapter is GuC_MMIO).

Hosted test83265 passes: ordered seven-message success, transport/refusal/
owner-loss failures at every exchange, loss at every admission/before/after
ownership check, invalid-address rejection, and repeated-call rejection.
Log: `/tmp/cubit-ct-register.log`. This is not a SPARK proof or hardware test.
Native backing publication and transport instantiation remain pending.

## Native runtime mailbox access (2026-09-28)

`Intel_GPU_Native_GuC_Mailbox` admits only the four main-GT runtime words at
0x190240..0x19024C and the write-only notification trigger at0x1901F0 (value1).
The fixed control-page policy adds0x190000 at index8, slot33, CPU alias61208000;
existing page indices and grants are unchanged. This is page-granularity
authority, not hardware-enforced register isolation. Device-manager and driver
loops already consume this common policy. The startup adapter remains separate.

Reference: Linux i915 `intel_guc_reg.h` definitions and `intel_guc.c`
`intel_guc_notify` / main-GT `intel_guc_init_early`:
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h.html
https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html

The adapter requires serialized ownership, running authenticated firmware,
retained forcewake and UC/NX mapping from its caller. Invalid indices are
rejected before address arithmetic. Volatile32-bit accesses check ownership
before and after; stores are fenced. A failed post-store check is ambiguous,
not a rollback. Posting read before notification remains the transport's job.

Hosted tests exhaust all byte offsets in2MiB, check literal CPU aliases,
exercise real host-mapped stores and ownership loss. These tests and existing
startup adapter/page-policy regression tests pass (73331, log
`/tmp/cubit-native-guc-mailbox.log`). No GPU execution is simulated by these
tests. Native CT allocation, channel instantiation and registration are still
pending; the published startup image is unchanged.
Private native compilation of Intel GPU and devmgr plus the page-policy
SPARK checks also pass (60792, `/tmp/cubit-guc-mailbox-native.log`). This does
not prove the MMIO device behavior or establish a native firmware response.

## Bounded MMIO HXG transport (2026-09-28)

`Intel_GPU_GuC_MMIO` implements serialized short request/single-word-response
exchanges using generic native callbacks, separate from startup scratch writes.
It writes the request, performs a posting read, then notifies firmware. Polls
validate origin/type, distinguish BUSY/RETRY/failure/success, reject ownership
loss or invalid MMIO, and check unavailable/backward clocks. The initial10ms
deadline extends to at most1s from the original start on BUSY; a total poll
budget and at most three firmware-requested resends prevent unbounded loops.
Only explicit DROPPED/RETRY permits retransmission. Other admitted failures
break the channel, preventing stale/in-flight mailbox reuse. Complete means
HXG success; callers must still validate action-specific response data.

Twenty hosted scenarios pass, including posting ordering, retry exhaustion,
BUSY losing GuC origin, deadline and poll exhaustion, clock failure/regression,
owner loss, and attempted reuse after failure. This is not native integration,
Intel hardware validation or a SPARK proof. References:
[MMIO exchange](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#511)
and [HXG ABI](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/abi/guc_messages_abi.h.html).

## CT registration encoding (2026-09-28)

`Intel_GPU_GuC_CT_Setup` plans a retained28KiB mapping with separate descriptor
pages, a4KiB H2G ring and16KiB G2H ring. It rejects unaligned, undersized,
below-bias and above-runtime-limit extents. It emits descriptor/address/size
KLVs for G2H first, then H2G, and a separate enable request. SELF_CFG must
return recognized-key success(DATA0=1), while enable requires DATA0=0; the
complete HXG success header is checked, not just the payload. It does not
allocate, zero, publish, register or own memory and is not native transport.

Literal message/order tests, allocation/end-boundary tests and response-bit
mutation tests pass, as do SPARK checks with explicit address helper bounds.
Source: [i915 CT registration](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_ct.c.html#219)
and [GuC action/KLV ABI](https://www.kernel.org/doc/html/v6.4/gpu/i915.html).
Run in Nix using `gprbuild -p -P tests/intel-gpu/guc_upload.gpr guc_ct_setup_tests.adb`.

## Native one-shot GuC execution wiring (2026-09-28)

Startup now instantiates the restricted register adapter and composed GuC
upload helper after successful reset, retained power ownership, all three
published mappings, ADS initialization and valid startup parameters. It retains
the validated WOPCM layout and reads the CSS/signature bytes from the same
prepared CPU/DMA descriptor that backs the published firmware extent. Each
source access rechecks descriptor identity and content bounds. Header bytes
are captured before Execute; the existing helper snapshots RSA before its
first signature write. Firmware backing remains immutable during this attempt.

MMIO/source failures latch a local fault; later accesses cannot recover and
continue silently. Execute is one-shot, with the existing100ms DMA/3s startup
deadlines plus a1,000,000-poll bound and unavailable/backward-clock rejection.
Startup disables its local access gate afterward, retains resources on every
outcome, and logs explicit phase/raw status and transfer detail. Firmware READY
is not CT/submission readiness and is not accelerated rendering.

Private native compilation, native Intel/device-manager links and Devices
KVM smoke passed (`guc-startup-native-{build,devices}.log`). Hardware results
remain pending. The published runtime-mapping image is unchanged and does
not contain this execution wiring. The separate `cubit_n95_guc_startup.img`
contains it; payload audits and USB/UEFI four-CPU/hub/no-PS2 validation passed,
including Boot Logs auto/menu replay and Mesa software cube animation/close.
SHA256: `4e9d22fb10d63c20f4484e7057e1faadb1edf77f94bfad142549348f58f386d2`.
Evidence: `tests/mesa-software/target/guc-startup-image-test.log`.
NUC testing should capture the `native GuC upload beginning`, `GuC startup`
phase/raw status and `GuC transfer` lines, or the first failed prerequisite.
`FIRMWARE-READY` does not yet mean CT communication or hardware3D works.

## Native GuC upload register adapter (2026-09-28)

`Intel_GPU_Native_GuC_IO` restricts the fixed control-page mappings to six
read offsets and89 aligned write offsets used by the existing upload helper.
It denies status-register writes, unrelated registers, unaligned offsets and
all addresses outside its explicit list. It resolves aliases through the
shared page selector; no caller-provided pointer is accepted. Writes use
32-bit volatile accesses and fences; ownership is checked before and after
access. Post-store ownership loss reports failure without claiming the store
was undone. The caller must retain/quarantine backing on this result.

Hosted tests enumerate all2MiB byte offsets and check literal expected aliases,
then exercise real CPU volatile accesses against anonymous fixture pages.
They pass, including denied ownership before mapping and loss after a store.
This adapter is not yet instantiated by native startup, and these tests are
not Intel hardware evidence or a SPARK proof of MMIO.
Run in Nix: `gprbuild -p -P tests/intel-gpu/guc_upload.gpr native_guc_io_tests.adb`,
then `tests/intel-gpu/build-guc-upload/native_guc_io_tests`.

## GuC shim control-page authority (2026-09-28)

The fixed reset/control page list now includes BAR-relative0x138000 for
the upload helper's0x13816C shim register. The existing authenticated,
frozen-claim devmgr grant loop handles index7; no caller-selected physical
address or target slot is accepted. Slot32 avoids logging23/27, display24/25/28,
GGTT26 and PHY29..31. Earlier control pages keep their slots16..22.
This is whole-page authority, not hardware-enforced per-register isolation.
The forthcoming native upload adapter must restrict its own accesses.
Page coverage/uniqueness and PHY separation regressions pass, along with
the selector's SPARK checks. Native consumer compilation is tracked separately;
no firmware execution or new hardware validation is claimed.

## Native startup parameter construction (2026-09-28)

Startup retains PCI device/revision immediately after authenticated bootstrap
admission. Only successful firmware/ADS/log mappings, retained power ownership
and ADS initialization at the published address allow parameter construction.
The block uses those actual GPU addresses and retained backing sizes, the
WOPCM-biased runtime lower bound, scheduler enabled, SLPC/PXP disabled, and
the minimal16KiB log layout without half-full interrupt notification.
It is retained in memory; there are still no parameter MMIO writes or execution.

For the currently selected ADL-N/IP12.0 and metadata-admitted70.49.4 firmware,
workaround bits14/18/22 select PRE_PARSER, POLLCS and TSC_CHECK_ON_RC6.
This is platform/firmware selection, not a PCI-revision heuristic. References:
[i915 flags](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#292)
and [reset workaround selection](https://kernel.googlesource.com/pub/scm/linux/kernel/git/next/linux-next/+/f9b5aeed37bc9023d700c9c8ff186f1e98692bc8/drivers/gpu/drm/i915/gt/intel_reset.c).
Private native compilation passed. The hosted codec regression now checks
literal words for the native16MiB ADS/16KiB log shape; neither check establishes
hardware startup. The published runtime-mapping image predates this wiring.

## GuC identity encoding correction (2026-09-28)

The startup parameter codec now requires the actual PCI device ID alongside
revision instead of hard-coding46D2. It admits encoding only for46D0..46D4;
this does not change the stricter native reset/inventory admission. Tests
exhaust all65536 device IDs and all256 revisions for each accepted ID,
checking exact device/revision packing and zero output on rejection.
The hosted upload regression and codec SPARK checks passed. Those proofs
cover the codec's existing contracts/runtime checks, not authenticated identity
provenance or successful hardware startup. Native parameter construction still
needs to bind the authenticated bootstrap identity and published mappings.
Reference: [i915 guc_ctl_devid](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#348).

## Runtime-mapping hardware test image (2026-09-28)

`kernel/cubit_n95_runtime_mapping.img` has SHA256
`0f1c142c706de9f9b9cc1affaf481fff8c6387dc9fe65d04d5c7430a2db03355`.
Native link and Devices smoke passed. The image payload audits and QEMU
UEFI four-CPU USB-flash/hub/no-PS2 boot passed, including Boot Logs startup
and menu replay, and the Mesa software cube's pixels, animation and close.
Evidence: `tests/mesa-software/target/runtime-mapping-image-test.log`.

On the NUC, inspect Boot Logs for complete scanout inventory, followed by
firmware mapping, ADS mapping and log mapping results and GPU addresses.
All three should report `PUBLISHED`; record the first rejection if not.
This image intentionally does not start GuC. Mapping success is not firmware
authentication or hardware-rendering success. Check desktop/input remain
usable, since preserving firmware scanout is part of the mapping contract.
The previous `cubit_n95_four_pipe_inventory.img` remains available.

## Native ADS/log runtime publication (2026-09-28)

After successful firmware mapping, startup admits the separate WOPCM-biased
runtime interval into one retained ledger. It initializes/publishes ADS first,
then the16KiB log slice from the retained firmware allocation. Both use that
same ledger, scanout predicate, exact descriptor/DMA binding, native PTE adapter
and ordered invalidation; the upload ledger cannot allocate runtime objects.
ADS initialization uses the selected GPU address, never a speculative or CPU
address. Published GPU addresses are retained separately for later startup
parameter construction. Each transaction's local write binding is disabled
when it ends; this does not release the ledger claim, backing or actual mapping.
Failures retain partial publications and stop progression to the next buffer.
GuC transfer/execution remains absent. Private native compilation, native
link and the QEMU devices smoke passed (runtime-mapping-native-build.log,
runtime-mapping-native-devices.log in tests/mesa-software/target). The smoke
verifies desktop/Devices startup, not Intel MMIO. Intel hardware evidence
remains pending.

## Native one-shot firmware mapping integration (2026-09-28)

Startup now connects the publisher to the retained firmware descriptor and
native PTE adapter. It maps only the512KiB firmware slice, not the separate
log slice. Admission requires the frozen initial-boot display claim, successful
reset with retained forcewake, completed DC transition, all four held pipe
references, validated WOPCM/upload layout, complete scanout inventory, and the
scoped UC GGTT mapping. This is a single initial-boot owner; no hot rebind or
concurrent modesetter/submission owner is permitted by this contract.

The top upload interval is reserved under that owner, not inferred from zero
PTEs. Search additionally skips every occupied PTE and scanout exclusion.
After reserving, preparation rechecks the retained descriptor, flushes the
firmware slice, and binds the exact selected GPU interval to its DMA pages.
Native stores refuse nonzero prior entries. Readback precedes the existing
Gen12 pre-CT invalidation command. Failures retain backing and ledger claims.
`PUBLISHED` means mapped only: no firmware transfer/authentication/execution,
CT setup or submission is invoked. Hardware validation remains pending.

Private native compilation passed. The native compiler crashed on the ghost
claim model's derived array; a record wrapper around the same array avoids
the crash without dropping contracts. The reservation SPARK checks and hosted
reservation/publication/ADS/scanout-composition regressions were rerun and pass.

## Native PTE access adapter (2026-09-28)

`Intel_GPU_Native_GGTT` uses aligned64-bit volatile accesses to the fixed UC
alias0x64000000. Retained-owner and mapping-size callbacks bound every index;
writes additionally require the exact retained-allocation/PTE predicate,
the conservative below4GiB system-page encoding, and a zero prior entry.
Stores are fenced; invalidation remains a separate publication step. No clear,
overwrite or release operation is exposed. Loss of ownership after a store
returns failure even though the store has happened: callers must quarantine.

Nix host tests execute actual adapter loads/stores against private anonymous
pages, including ownership loss before/after the store, invalid sizes/indices,
sentinel reads, denied bindings and overwrite refusal. This is not hardware
validation or a SPARK proof of MMIO. Native startup now instantiates it for
the firmware, ADS and log publication transactions described above.
Reference: [Gen8+ GGTT 64-bit access](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html#442).

## Mandatory publication exclusion gate (2026-09-28)

`Intel_GPU_GGTT_Publish` now requires an owner-local `Range_Allowed` callback;
there is no default-allow instantiation. Search combines it with ledger claims
before reading candidate pages. Publication checks the entire extent before
reserving it, then checks again after backing preparation and before any PTE
write. Rejection after preparation retains the claim/backing and returns
`Protected_Range` without stores or invalidation. Serialized ownership must
still prevent mutation during the subsequent writes; this is not a lock.

Hosted GGTT and16MiB ADS publication regressions pass. The composed
`scanout_publish.gpr` test binds the actual inventory predicate and verifies
protected-page skipping, missing inventory rejection, and preparation-time
inventory invalidation with retained claim/no writes. These transaction tests
are regression evidence, not a SPARK proof of hardware coherency or native GPU
execution. Native instantiation remains pending.

## Runtime/upload partition policy (2026-09-28)

`Intel_GPU_GGTT_Layout.Plan` partitions the validated2/4/8MiB table's GPU
address range into a page-rounded WOPCM-biased runtime interval, a top18MiB
firmware-upload reservation, and the final4KiB guard page (excluded from
upload allocations). The runtime interval ends at the upload reservation,
including on1/2GiB address spaces; it never reaches above0xFEE00000.
Invalid table sizes, overflowing/out-of-range biases, and empty rounded
runtime intervals reject. This does not discover unallocated addresses or
authorize clearing firmware mappings. Native wiring remains pending.

Nix hosted tests check independent literal boundaries for all three sizes,
8194 low biases per size, and4096 near-split rejection cases per size.
SPARK proves the returned partition's postcondition, including ordering,
alignment, runtime ceiling and end-guard preservation. The same fields must
bound the native reservation ledgers, with scanout/platform exclusions and
retained ownership separately established before publication.

Reference: Linux [GGTT initialization and GuC top reservation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html#826).

## Four-pipe scanout collection (2026-09-28)

Native source now admits C/D only with their PW1/PW2/DC-off ancestors retained;
DC-off comes from the full native DC/PHY transition. All four pipe adapters
retain their references. Five planes and one cursor per pipe feed a combined
inventory. Missing reads, changing state and unsupported layouts prevent a
complete inventory; partial observations never authorize an allocation.

Hosted tests cover all120 plane register selections,16308 rejected pipe-power
combinations, real volatile adapter loads for20 planes/four cursors against
anonymous host mappings, and65536 independent interval comparisons. SPARK
proves selector bounds and the inventory non-overlap postcondition. These are
software properties; no physical C/D power-up has yet been validated. The
inventory is not a free-space allocator or proof of GGTT ownership. Firmware
reservations and publication-time stability remain separate requirements.

This supersedes the A/B-only collection limitations in the historical sections
below. No native GPU PTE publication is enabled by this change.

## Read-only WOPCM layout admission (2026-09-28)

After successful reset, native startup samples C050/C340 twice and logs
stability plus the selected layout's validity, lock state and pin bias.
The upload size comes from the selected, prepared firmware's CSS+code extent.
`Intel_GPU_ADLN_WOPCM.Select_Layout` rejects all-ones/unstable samples, partial
lock state, a HuC-loading flag, oversized firmware and layouts beyond the
explicit2MiB ADL-N policy. An unlocked layout proposes base16KiB and size
2MiB-16KiB-36KiB; a locked layout must fit the same supported capacity.
Pin bias is the validated GuC WOPCM size, following the runtime reservation
rule. No WOPCM register is programmed by this observation/selection step.

Linux can accept larger firmware-programmed layouts on deprivileged devices;
CuBit does not yet treat that as independent evidence of physical capacity.
Such a case is reported invalid and requires extending the platform policy,
not silently accepting up to8MiB. Native MMIO behavior remains unverified.
Hosted tests cover default/locked layouts and rejection cases. SPARK discharges
all6 checks (5flow,1prover), including the selected layout extent/pin-bias
contract; it does not prove device identity, register authenticity or ownership.

Reference: [upstream WOPCM sizing and locked-layout handling](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_wopcm.c).

## Gen12 pre-CT invalidation adapter (2026-09-28)

The fixed control-page list now includes BAR+0xC000 at index6/slot22,
separate from publisher23, display24/25, GGTT26 and observer27. The broker
uses the list's upper bound rather than a duplicated literal. All seven
pages must be granted before reset authorization; no extra caller-selected
address is accepted. The shared CPU mapping base is0x61200000.

`Intel_GPU_GGTT_Invalidate.Issue` requires mapped control pages, successful
native reset and a ready UC GGTT alias. It orders prior PTE stores and writes
the32-bit value1 to GEN12_GUC_TLB_INV_CR at0xCEE8. Native object inspection
confirms `mfence; movl $1,0x61206ee8; mfence` after the guards. The operation
is compiled but uncalled: no PTE publication has been enabled.

Linuxv6.16's pre-CT Gen12 path issues this write without a status poll;
CuBit does not invent an acknowledgment bit. A True result means the command
was issued, not firmware execution/authentication or completion of queued GPU
work. This boundary is for initial bring-up; future CT-enabled operation must
use the corresponding GuC invalidation protocol.

References: [invalidation sequence](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_ggtt.c),
[register definition](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h).

## Cursor footprint decoding (hosted, 2026-09-28)

`Intel_GPU_Cursor_Decode` checks two samples of CURCNTR, CURBASE,
CURSURFLIVE and CUR_FBC_CTL. It uses the new-style mode mask, not the legacy
bit31 cursor-enable flag. Only exact square ARGB64/128/256 modes currently
produce a footprint (16KiB, 64KiB, 256KiB respectively). Unknown control bits,
shortened-height/FBC configuration, sentinel reads, changing samples,
pending/live mismatch and invalid aperture geometry cannot produce a valid
extent. Disabled state does not authorize reclamation of earlier backing.

`Intel_GPU_Cursor_Collect` reads two four-field samples and guarantees one End
callback after every successful Begin, including failed reads. Tests cover 72
failure combinations and a changing snapshot. The new `Native_Cursor` adapter
binds those reads to A/B registers under retained power and local ownership;
main now invokes A/B observers after successful reset and pipe-power acquisition,
and publishes cursor status/control/pending/live records. A Linux-hosted fixture executes its actual
volatile loads against anonymous pages reserved with MAP_FIXED_NOREPLACE at
the driver's expected virtual addresses. Distinct A/B values check pipe
selection, live/pending mismatch, sentinel rejection, and access-scope cleanup.
It refuses to overwrite any existing mapping. This validates address selection
and loads in the adapter, not GPU register semantics, native power management
or device-memory ordering. The native CuBit driver build passes; real hardware
validation of the expanded observations remains pending.

The decoder's hosted tests cover
all 65,536 low control-word values and malformed/boundary samples. The SPARK
contract establishes exact footprint sizes on accepted samples, not hardware
snapshot atomicity or ownership. The native observer must retain pipe power
and collect both samples before relying on this result.

Sources: pinned Linux v6.16 [cursor registers](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/intel_cursor_regs.h)
and [cursor programming](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/intel_cursor.c).

## Native A/B five-plane observations (2026-09-28)

The native observer now selects hardware planes 1 through 5 on each of A/B,
under the existing retained display-power references. Each plane uses the
same two six-register samples and conservative linear-footprint decoder as
the former primary-only observer. New boot records identify both pipe and
plane number; no new register writes or GGTT publication are enabled.

The bounded `Intel_GPU_Plane_Registers` selector rejects C/D and admits only
CTL, STRIDE, SIZE, OFFSET, SURF and SURFLIVE. Its 120 possible selections have
hosted regression coverage, and its bounds/termination contracts have three
discharged SPARK checks. Native rejection tests exercise all five planes with
missing ownership, power or table geometry. A Linux-hosted fixture also maps
anonymous pages without replacing existing mappings and executes the actual
adapter loads for all ten A/B planes. Distinct geometry and surfaces check
register selection; pending/live mismatch and failed-read recovery are covered.
This is positive host-memory execution, not positive GPU MMIO evidence.

This is still incomplete display-memory admission: cursors and C/D remain
unobserved, supported active footprints remain deliberately narrow, and two
matching samples are not an atomic hardware snapshot. It must not be used to
declare all unreported GPU memory free. Hardware validation of these expanded
reads is pending; the previously published `pipe_planes` image predates them.

Sources: Linux v6.16 [display platform/plane counts](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/intel_display_device.c)
and [universal-plane registers](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/skl_universal_plane_regs.h).
ADL-N selects the ADL-P display description; display version 13 uses four
sprite planes in addition to the primary. Registers step by 0x100 per plane
and 0x1000 between A/B; hardware numbering here is one-based.

## Publication search and remaining native admission (2026-09-28)

`Publish_Available` now searches hardware PTEs inside the ledger's admitted
aperture, skipping retained software claims before MMIO reads. `Publish` then
reserves the selected interval and rechecks all PTEs before preparing backing
or writing. Search failure consumes the attempt without claiming memory or
writing; later failures retain any acquired claim. Hosted tests cover an
occupied first candidate, unpublished reservations, and real 16MiB ADS pointer
preparation at the selected address. These are not native GPU publication tests.

Native admission/selection must account for inherited PTE ranges as well as
live/pending planes, cursors and platform reservations. A bounded hardware
search may propose an interval only inside an independently admitted range;
zero entries alone do not establish ownership. Preserve the one-shot write
and retained-backing rules; do not add blind retries with replacement ledgers.

Rechecked Linux v6.16 `intel_ggtt.c` and `intel_uc_fw.c`: upload staging uses
the dedicated top reservation, while runtime resources use the WOPCM-derived
lower bias and GuC-accessible ceiling. They must not be collapsed into one
unqualified "free GGTT" pool. This is an integration requirement, not a claim
that native CuBit publication or scanout adoption is complete.

References: [GGTT initialization](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_ggtt.c),
[firmware GGTT placement](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c).

## GGTT write-grant validator (2026-09-28)

`Intel_GPU_GGTT_Access.Plan_Write` prepares the0233/slot26 grant from
one configuration snapshot plus broker-retained BAR/table and owner/executor
state. It requires8086:46D2, D0, an admitted non-prefetchable register BAR,
unchanged table size, and currently disabled INTx/MSI/MSI-X (with MSI-X masked
when present). A historical successful disable cannot substitute for current
interrupt-control readback. Invalid plans have zero extent; valid plans cover
only the discovered2/4/8MiB table at BAR+8MiB. Hosted tests cover all table
sizes, owner/completion gates, malformed/changed configuration and all eight
MSI/MSI-X enable/mask combinations. The device manager now binds it to an
exact zero-payload request from the designated Intel PID with badge4947.
It retains BAR/table size after the initial read-only grant, requires the
exclusive display owner and completed PCI-disable executor, and reads fresh
configuration through checked PCI I/O. The attempt is consumed before minting
or replying, including failures. Success grants only the discovered table in
fixed slot26 and returns its physical extent. Startup returns the existing
busy reply without consuming the attempt. The native driver requests
this grant only after successful native reset, using retained ownership and
the originally inspected BAR/table size. The client checks the exact reply,
uses fixed virtual address0x64000000 and512-page UC/NX mapping chunks, and
retains partial mappings on failure. Its21 hosted fixture cases cover client
control flow, not kernel enforcement. This is not admission of free GPU VA or proof that existing
scanout/platform reservations can be overwritten. No PTE stores are enabled.
SPARK level1 now discharges all21 generated checks for this unit, including
the valid-plan contract (owner/disable inputs true, exact table extent and
nonwrapping physical range), initialization, call preconditions and termination.
Hosted regressions pass again with that stronger contract. This uses callee
contracts and trusted broker inputs; it does not prove ownership authenticity,
PCI serialization, capability minting, or physical hardware behavior.

## Intel grant-slot separation (2026-09-28)

Display-power grants now use slots24/25. Their former22/23 layout collided
with `CuBit.Log_Protocol.Publisher_Slot=23`; the kernel's mint-at-slot operation
replaces an existing entry. Thus granting the second power page could remove
the driver's logging endpoint. The hosted display-claim regression imports the
actual logging protocol and checks both display slots against publisher and
observer slots, reset-page slots and each other. Native intel-gpu/devmgr builds
pass. This correction is staged, not in the previously published device_map
image; no claim of a hardware-tested grant sequence is made.

## Main-checkout device mapping integration (2026-09-28)

The private boot-debug workspace's read-only MAP_DEVICE support was missing
from the main syscall dispatch/handler. The main capability check required RW
even for inspection, and the handler ignored arg3 and always chose writable
PTEs. Thus a strictly RO grant failed; an overlapping RW grant could silently
make a requested inspection mapping writable. The main path now forwards and
validates arg3 (0=RW,1=RO), uses the existing Device_Memory_Admission predicate
for capability containment/access, and omits PG_WRITABLE for RO. UC and NX
remain set. Newer owned-memory guards are preserved, and unaligned, wrapping,
null/non-user virtual ranges are rejected. Hosted admission and capability
consumer tests pass; current native build passes. The historical native
write-fault test in tests/device-memory/README.md belongs to the private kernel,
not fresh native enforcement evidence for this merged kernel. Follow-up run
`b_e2rf6m` now verifies the merged kernel using those historical RAM fixture
binaries: writable/unknown-mode denial, RO read success, exact-address write
protection fault, process stop and Desktop survival. See the test README for
input provenance and limitations. The previously
published pci_irq image predates this fix; do not use it to validate the fixed
read-only path.

## Native IRQ lifecycle audit (2026-09-28)

The current kernel interface is registration plus a persistent, coalesced
process doorbell, not a device-specific interrupt fence:

- `Syscall.Admin.handleEnableIrq` registers an owner and optionally enables an
  IOAPIC route; MSI registration skips IOAPIC unmasking.
- `Interrupts.dispatchDeviceIRQ` reads each vector subscriber and calls
  `Process.IPC.notifyIRQ`. That call sets `irqNotificationPending` under the
  recipient mailbox lock. It carries no device identity or registration epoch.
- `Capabilities.IRQ.unregisterIRQ` removes table entries. Its implementation
  does not wait for a dispatcher which has already read a recipient;
  `unregisterAllByPID` is also used by process teardown. Neither procedure is
  an interrupt-drain completion interface for a live driver.

Consequently neither clearing a process doorbell nor unregistering a recipient
is evidence for the pipe helper's `Delivery_Blocked` prerequisite. A shared
INTx vector must not be globally masked to quiesce one GPU: other registered
devices may still need it. Device PCI controls, kernel notification delivery,
and userspace handler completion are separate states.

For initial Intel bring-up, the inspected devmgr path does not yet register an
Intel GPU IRQ recipient. This can simplify the initial software-handler case,
but it does not prove inherited hardware routing or pending CPU vectors are
absent. Before native pipe quiescence, explicitly establish which writers can
touch GPU IRQ registers, stop this device's interrupt generation, and determine
whether any pending vector can invoke such a writer. Do not require an invented
global “all interrupts drained” condition when only device-handler exclusion
is necessary, and do not infer that exclusion from PCI readback alone.

Runtime reset/restart will need a separate lifecycle protocol: stop device
generation, exclude new device handler work, synchronize already admitted
handlers, then quiesce source registers while retaining resources. A future
kernel fence must define its linearization point against dispatch and preserve
other subscribers; per-process coalescing alone cannot supply that contract.
This is a source audit and integration requirement, not a proved SMP protocol
or hardware-tested implementation. Coordinate kernel IRQ changes with the
networking owner before editing the shared dispatch/mailbox machinery.

## PCI interrupt disable plan (2026-09-28)

The generic `Intel_GPU_PCI_IRQ_Disable` executor now consumes that plan under
caller-held exclusive PCI ownership. It admits only8086:46D2 in D0, takes a
fresh full snapshot before each selected word write, and verifies a final
snapshot. All bytes must match the expected baseline except ordinary PCI
status; capability-list presence must still match. This conservatively rejects
unexplained BAR/control/layout changes rather than adapting an old plan.
At most five snapshots and three writes occur. The phase becomes `Uncertain`
before the first write and stays so on partial failure; no rollback/retry exists.
A no-op plan still requires final verification. `PCI_Disabled` does not mean
the CPU's pending vectors or handlers have been drained. The helper is tested
with modeled callbacks and is now instantiated in devmgr's owner-checked0232
IPC path. The general frozen-configuration guard remains in place; the private
callback permits only the freshly planned interrupt-control word/value pairs.
The native driver requests this once after reset authorization, before GT
reset. It requires the exact successful completion and otherwise skips reset,
retaining resources. Busy replies can be retried only within the bounded
startup request; lost/ambiguous replies do not trigger a new attempt. Its
authorization polling rejects unavailable/regressing clocks and has both an
elapsed deadline and a30000-iteration cap. Kernel wakeup/scheduling remains an
external assumption; the poll count is not a proof against a stalled kernel.
Native driver/devmgr compilation passed under the shared lock; physical PCI
execution and pending-vector behavior remain unverified. No new NUC image yet.
The non-Intel QEMU `devices` regression also passes after a forced devmgr
rebuild; it checks general device startup, not execution of the Intel0232 path.

Native transport prerequisite fixed: devmgr's existing `pciWriteConfig16`
performed a DWORD read/modify/write, which could write back adjacent W1C status
bits. It now selects CF8 once and uses `portOutp16` on CFC/CFE. The frozen-Intel
configuration guard is unchanged; this shared helper does not bypass it.
The executor uses the separately admitted private callback. The syscall path
checks two-byte port authority and emits x86
`out16`. Native object inspection confirms no configuration-data read and no
DWORD data write in this helper. This also affects its existing non-Intel
callers; no networking setup policy was changed.

`Intel_GPU_PCI_Interrupts.Plan_Disable` uses the validated conventional
capability chain to produce at most three exact 16-bit changes: set command
INTx-disable, clear MSI enable, and clear MSI-X enable while setting its
function mask. It preserves other bits, omits unchanged words, and rejects
malformed/overlapping/duplicate capability records and all-ones control reads.
It never includes PCI status (which contains W1C bits), message addresses/data,
or the MSI-X table. Definitions are from the
[PCI register constants](https://github.com/torvalds/linux/blob/v6.16/include/uapi/linux/pci_regs.h).

This is a plan, not a native configuration write or interrupt-drain operation.
The executor must retain owner/PCI serialization, revalidate identity/D0 and
capability layout, compare fresh baselines, use actual 16-bit PCI writes and
verify results. Pending CPU vectors and handlers require separate treatment.
Do not set the pipe helper's `Delivery_Blocked` merely because this plan exists.
The hosted regression checks256 control combinations, exact changed words,
preservation of all other bytes, no-op replay and malformed inputs; both
native decoder consumers build with the extension.

## Pipe interrupt handoff helper (2026-09-28)

`Intel_GPU_Pipe_IRQ` implements a bounded, one-shot quiescence sequence for a
single admitted pipe: mask IMR and verify, disable IER and verify, clear IIR
twice with posting reads, require the second result to be zero, then recheck
IMR/IER. It never enables a source or restores firmware state. A failed write
may have reached hardware; any failure after starting leaves `Uncertain` and
the caller must retain ownership/power and upstream delivery blocking.

Offsets follow [Linux v6.16 register definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_reg.h):
`44404/44408/4440c + pipe*16`. The double-clear ordering follows the
[display IRQ reset routine](https://github.com/torvalds/linux/blob/master/drivers/gpu/drm/i915/display/intel_display_irq.c).
CuBit additionally verifies readback and explicitly reports callback failures.
The all-ones IMR is a legitimate mask, not itself a failed-read sentinel;
the transport callback must report access failure separately.

This is NOT yet the native post-enable callback: it requires an exclusive
owner, a held pipe-power reference, and independently blocked upstream
delivery with in-flight handlers drained. A PCI snapshot cannot supply those
conditions. Source masks for other interrupt groups, native grant/mapping,
delivery handoff, DC-off work and plane/cursor collection remain outstanding.
The hosted regression covers all four pipes, exact ten-callback ordering,
each callback failure, changed verification values, admission and reuse.
No new MMIO writes are enabled in the current native startup or user image.

## Native parent PW1 adapter (2026-09-28)

Follow-up: the implementation is now `Intel_GPU_Native_Parent`, restricted to
PW1 and PW2. Main acquires PW2 only with the local PW1 `Held` result. The second
instance can add only request bit3 in45404, not PW1's workaround, and retains
the reference without release. Unsupported wells reject before MMIO. Both
parent descriptors have zero pipe IRQ mask; individual pipe callbacks remain
unimplemented. The original PW1-only source was replaced, not kept as a
compatibility alias. Native build and existing156/84 helper regressions pass;
this refactor/PW2 addition is not yet in `cubit_n95_pw1.img` and has no hardware
validation.

`Intel_GPU_Native_PW1` binds the existing enable state machine to the native
fixed-page mapping. It reads only45404/42000/46430 and writes only45404/46430,
requiring the proposed value to preserve the freshly read baseline and add
only bit1 or bit15 respectively. Invalid access is latched. It uses the
microsecond monotonic API and the helper's finite polling limits. Main invokes
it after display-page mapping and reports an explicit textual result.

The reference PW1 descriptor has no pipe IRQ mask; its parent-only post-enable
callback therefore performs no pipe IRQ programming. This exception must NOT
be reused for pipe wells. The adapter retains PW1 for the device lifetime and
exports no release. It does not acquire DC-off or any pipe reference, inspect
planes, or admit GGTT space. Partial write failures remain unrecovered.

References: Linux [power-well map, icl_power_wells_pw_1](https://raw.githubusercontent.com/torvalds/linux/master/drivers/gpu/drm/i915/display/intel_display_power_map.c)
and [hsw_power_well_enable/post_enable](https://raw.githubusercontent.com/torvalds/linux/master/drivers/gpu/drm/i915/display/intel_display_power_well.c).
The inherited no-VGA/IRQ concern below still applies to the separate pipe-well
handoff; it is not a reason to invent a pipe IRQ callback for PW1.

Native compilation passed and the existing generic enable/release fault suites
passed156/84 cases. Those tests do not execute the native MMIO adapter. No NUC
validation or new image yet; the next packaged image WILL add these PW1 writes.

## Native integration audit (2026-09-28)

Follow-up: devmgr now accepts authenticated request0231 with a fixed page
index0..1, granting pages45000/46000 at fixed slots22/23 only to the retained
display owner. The shared page handler rechecks D0/PCI/BAR and freezes each
successful grant before replying. Startup busy replies cover this request.
This is page-level authority (other registers on those pages are accessible),
not per-register isolation. Native devmgr compilation and hosted owner/layout
tests pass; the IPC denial branches are not yet exercised on a live Intel
device. The driver does not request these pages or perform power writes yet.
The current `.img` is unchanged.

Driver follow-up: `Intel_GPU_Display_Mapping` now requests both fixed grants
and maps them at61400000/61401000 before the existing reset attempt. It accepts
only the expected completion token/tag/zero payload, handles the startup busy
reply, and bounds polling by count and elapsed time while rejecting unavailable
or regressing clocks. An attempt is consumed even after partial grant/map
failure; successful mappings remain retained. The driver logs `display power
pages ready (NO power writes/reference)` on success. Native compilation passed;
hardware grant/map execution and power acquisition remain unverified. No new
image was packaged for this step. The adapter performs no register reads or
writes; mapped authority alone must not be used as a power-reference predicate.

The helper tests are not the native startup path. Current native blockers,
in dependency order:

1. `main.adb` requests 022F and devmgr records a unique display-power owner,
   but the reply grants no writable display MMIO or hardware reference.
   Connect the display-power callbacks (including platform post-enable and
   DC-off work) before using plane collection to admit scanout exclusions.
2. 022B maps GGTT read-only. A separate owner-checked writable grant must
   revalidate device identity/D0/BAR and be scoped to the designated driver.
   Reset-page grants intentionally do not confer this authority.
3. Native aperture admission must protect inherited live/pending planes,
   cursors and platform reservations before allocating. The existing present
   count cannot establish that an interval is free or owned.
4. Choose upload and runtime addresses separately. The existing GuC parameter
   encoder already checks caller-supplied pin bias, runtime ceiling, complete
   ADS/log extents and their nonoverlap. The native caller must derive the bias
   from the actual WOPCM configuration; a numerically valid ADS composition
   alone is not enough. The retained firmware allocation's log slice needs
   its own GuC-accessible mapping, even when upload staging uses the top range.
5. Bind prepare/flush, ordered PTE writes and translation invalidation to
   actual native operations before invoking publication/upload. Do not stub
   `Invalidate` as successful merely because PTE readback matched.

Reference rechecked against Linux
[intel_ggtt.c](https://raw.githubusercontent.com/torvalds/linux/master/drivers/gpu/drm/i915/gt/intel_ggtt.c):
`init_ggtt` sets a WOPCM-derived pin bias and protects the end guard;
`ggtt_reserve_guc_top` separates upload staging from runtime addressing.
`needs_wc_ggtt_mapping` requires uncached mappings on ICL+ because larger
write-combining bursts can be dropped. `guc_ggtt_invalidate` uses a distinct
Gen12 GuC invalidation path before its message-based facility is available.
These are reference requirements, not claims that CuBit implements them.

The next native change is the display-power MMIO adapter and its scoped grant,
not turning on GGTT writes based on the inspection count. The current user
image remains the combined diagnostics/Mesa checkpoint.

## Native retained log storage

The native firmware buffer now partitions its existing 1 MiB retained DMA
allocation: firmware occupies at most the first512 KiB; the next16 KiB is
reserved for the log state page plus three4 KiB sections. Compile-time checks
keep these slices aligned, disjoint and inside the allocation. Preparation
rejects larger firmware before allocation. The pinned335360-byte blob fits.
Existing whole-allocation zero padding, readback and cache flush cover this
log region before `Prepared_Log` can return a ready descriptor. The descriptor
contains CPU/DMA addresses, never a GGTT address, and shares allocation lifetime.
Main publishes a separate zeroed-retained log-storage diagnostic.

No GGTT mapping, startup-parameter construction from these buffers, firmware
execution or independent freeing is enabled. The private v31 image is unchanged.
The hardware devmgr bootstrap/grant implementation has now been selectively
reconciled into the main tree. Normal USB image build/catalog integration now
packages the driver, pinned firmware and original Intel license. The private
v31 image remains the hardware-test artifact; other private boot changes have
not all been reconciled into the normal image.

The normal UEFI image passed the four-CPU QEMU USB live-boot regression
(`kernel/build/tmp/cubit-usb-live.uwuyqt0w`). The image audit checked firmware
and license hashes, and all 18 hosted CCL image tests pass, including explicit
firmware/license optical placement. These checks do not exercise Intel MMIO,
forcewake or firmware execution. The image reuses existing staging binaries
apart from the rebuilt devmgr and Intel driver; it is not a fresh world build.

## Main-tree devmgr reconciliation

Reviewed hardware-only additions now cover PCI discovery, frozen configuration,
the read-scoped firmware file, authenticated driver bootstrap/log publication,
read-only GGTT inspection, retained DMA storage, fixed reset-page grants and
one-shot reset permission. Requests remain tied to the launched Intel process
and endpoint tag; writable grants recheck identity, power state and BAR.
While another service is starting, Intel resource requests receive a busy
reply rather than being mistaken for that service's readiness notification.

The private RAM/map-check/DMA-retention fixtures were not imported. Neither
were unrelated xHCI/procmgr changes or the private tree's older, smaller
virtio-net allocation. This remains static bring-up ownership, not a complete
hotplug/rebind or process-lifetime recovery protocol.

## NUC v30: forcewake gate, not a completed reset

The current native GGTT diagnostic now rejects an all-ones entry anywhere in
the scanned table and reports its index. Previously only entry zero was checked,
so a later failed read could inflate the present-entry count. Ordinary entries
still require only the existing low-word read; the high word is checked only
for the all-ones sentinel. This remains non-atomic diagnostic sampling, not an
ownership map. Native service compilation passed; this change has not been
tested on the NUC or incorporated into the private v31 image.

Rechecked Linux's [GGTT initialization](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html#861):
it tracks reservations, including the firmware-upload region and end guard,
before clearing unallocated ranges. CuBit's present-count scan does not supply
the equivalent ownership evidence. Keep aperture admission separate from
finding zero PTEs; active scanout and platform reservations must be accounted
for before enabling native publication.

Physical feedback was `native reset 1 engine=0`. The native runtime's
`Discard_Names` turns this enumeration image into ordinal 1, which is
`Forcewake_Failed`. The handoff returns before stopping engines or writing GT
reset; zero is the absence of an engine-specific failure. Explicit diagnostic
labels now replace the enumeration image in source.

The combined forcewake helper formerly allowed only 100 polls per phase even
though its elapsed-time budget is 50 ms. A hosted delayed-ack regression shows
an acknowledgement after 200 pauses failing that bound while succeeding with
the new 100,000-poll bound. The 50 ms budget and quarantine behavior remain.
This establishes a premature-poll-exhaustion risk, **not** the physical root
cause. First-failure domain, reason and last sampled ACK are retained across
cleanup for the next hardware run; diagnostics perform no additional MMIO.
These follow-up changes are now packaged in private
`kernel/cubit_n95_forcewake_v31.img` (SHA-256
`2a402d12da0cb66a46fc8412977327273e29041e0763bced215f1412b7fe28a4`).
Four-CPU UEFI USB-flash/hub/log-viewer QEMU regression passed; QEMU does not
exercise the physical Intel handshake. Hardware validation remains pending.

## Firmware-startup observation (hosted only)

`Intel_GPU_GuC_Status` decodes C000 status, preserving the separation between
authentication rejection, boot-ROM failure and firmware initialization errors.
Ready requires microkernel F0, authentication GOOD, MIA out of reset, and no
recognized boot-ROM failure. Unknown microkernel codes remain pending. This
is deliberately stricter than checking F0 alone; physical compatibility still
needs validation. Sources: [status fields](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h.html#16)
and [Intel error ABI](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/abi/guc_errors_abi.h).

`Intel_GPU_GuC_Wait` observes without writes, retaining last raw status and
decoded category. It bounds both elapsed time (3s) and polls, rejects invalid
or regressing clocks, and does not reset/retry/free on failure. Call only after
successful transfer under exclusive ownership; stale READY cannot establish
a new firmware boot. Hosted status tests enumerate262144 field combinations
plus all-ones rejection and8 bounded-wait scenarios. SPARK-mode decoder source
has not yet been proved. No native startup wait is wired in v30.

## Composed upload (hosted only)

The composed uploader retains `Last_Transfer_Detail` as explicit text rather
than enumeration images (the native runtime discards enumeration names).
It distinguishes not-attempted, rejected, busy, invalid MMIO/clock, timeout,
cleanup failure, write failure and completed transfer. Reading this diagnostic
does not perform MMIO or clock reads; a rejected retry preserves it. Completed
transfer is not authenticated firmware readiness. Hosted tests now include
eight DMA failure scenarios, including unavailable/regressing clocks, with a
stale READY startup fixture that must never be read after failed transfer.

The hosted executable accepts an optional firmware path. Against v30 staging's
actual tgl_guc_70.bin (SHA256
`2f1f57a1b23d186f2592318d1e07a1365968932841ccb3e7177c516ba006e2f6`),
it passes header admission and checks all64 RSA write values against the binary's
signature bytes through the composed transfer sequence. This complements the81
synthetic fault cases; it does not execute/authenticate that firmware on a GPU.
Current Linux still explicitly redirects ADL-N to ADL-S firmware selection:
[selection override](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c.html#293).

`Intel_GPU_GuC_Upload` composes WOPCM configuration, ADL-N transfer
preparation, RSA delivery and DMA.
It preflights selected firmware metadata, the entire below-4GiB mapped blob,
WOPCM layout and clock availability before configuration writes. Any failed
stage prevents later stages. A failed DMA write latches an error, suppresses
further writes and forces failure rather than accepting a stale clear START
bit. No retry or resource release is exposed. Caller must still establish
reset, forcewake, immutable source and coherent retained GGTT
mapping. Authentication/readiness are separate unfinished stages.

For ADL-N graphics IP12.0, preparation writes SHIM_CONTROL C064=8607
and GT_PM_CONFIG13816C=1, checking the required bits on readback. It does not
apply Gen9 or IP12.50+ sequences. References:
[Linux transfer preparation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_fw.c.html#22),
[register definitions](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h.html#75),
[ADL-N device mapping](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/i915_pci.c.html#862).
This is explicitly ADL-N-only; caller must authenticate that device identity.

The composed upload now invokes the bounded startup observer after successful
DMA only. Its terminal success is `Firmware_Ready`, not merely `Transferred`.
Raw/decoded startup evidence is retained through read-only queries. Tests verify
no startup-status read after any preceding write failure, and reject bad
authentication, firmware initialization failure, unreadable status and stalled
startup. This is still a callback-based hosted composition, not native startup.

Hosted `guc_upload.gpr` passes101 cases, including failure at each of90 writes,
missing/unreadable preparation readbacks,
no-I/O invalid-address rejection and no-I/O repeat calls. This composes real
helpers against register callbacks, not physical GPU execution. No native
instantiation or upload is enabled in the v30 reset checkpoint.

### Startup parameter transport

The composed loader now requires an explicit 14-word startup parameter block.
After admission and the clock check, it clears SOFT_SCRATCH(0) and writes
SOFT_SCRATCH(1..14), before WOPCM programming, signature and DMA. A failure at
any of these 15 writes stops the sequence and consumes the attempt. The tests
check distinct word values and exact offsets, including reserved zeroes; their
parameter fixture is deliberately not a bootable device configuration.

`Intel_GPU_GuC_Parameters` now constructs a private parameter block from named
fields. GPU page addresses require explicit construction and reject zero,
unaligned and >=4 GiB offsets. ADS uses the page-number encoding; log address
and section-size fields are packed separately. Reserved startup words stay
zero, device ID is fixed to the selected 46D2, and revision is preserved.
Unsupported ADL-N workaround bits reject. The uploader rejects a default or
invalid block before any callback. Tests cover all256 revisions, each workaround
bit, maximal log fields, feature/debug flags, address bounds and zero reserved
words. The encoder now also requires a claimed mapped log extent: it must be
page-aligned, contain the state page plus all three encoded sections, and end
at or below 4 GiB. The section counts encode one less than their unit count;
zero does not mean an absent section. Crash/debug share the 4 KiB/1 MiB unit
choice while capture has its own. All1024 size/unit combinations pass exact-fit
and one-page-short tests, alongside end-of-address-space rejection. See the
[Intel log sizing implementation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_log.c.html#146).
This checks consistency with the supplied extent size, not evidence that an
address is GPU-owned, that the caller actually mapped that extent, or that ADS
contains valid data.

References: Intel's [parameter initialization and transport in i915](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.c.html#383),
[startup ABI](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_fwif.h.html#82),
and [scratch register offsets](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h.html#35).
This is not the Gen11 runtime message-register bank. Native integration must
still construct firmware-compatible device/revision, workaround and feature
words and retained ADS/log mappings. Encoding a parameter block does not validate
those objects or their ownership. The live reset-only image is unchanged.

## WOPCM programming (hosted only)

`Intel_GPU_WOPCM` applies the shared ADL-N layout admission before any MMIO,
then programs size before base and verifies each hardware-asserted lock bit.
A fully locked matching configuration needs no writes; conflicting or partial
locks reject without attempting repair. The HuC loading-agent bit must be zero
for this GuC-only path. All-ones reads reject, and failures after writes retain
quarantine with no retry. An attempt object does not establish exclusive access.

Capacity must come from trusted platform knowledge, not these possibly stale
registers. Reset/forcewake/exclusive ownership remain caller prerequisites.
Reference: [Linux uc_init_wopcm](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_uc.c.html#363).
Hosted `wopcm.gpr` covers 14 cases including matching locks, partial/conflicting
locks, incompatible HuC agent, failed writes, ignored locks, unreadable
readbacks, invalid layout, ordered writes and no-I/O retries. No native binding
or physical WOPCM programming has been enabled.

## RSA signature delivery (hosted only)

`Intel_GPU_GuC_RSA` supplies the selected ADL-N firmware's 256-byte signature
as 64 little-endian words at C200..C2FC. It snapshots all source bytes before
performing any device write, rejects incompatible/truncated layouts, and
retains a quarantined one-shot state if a write fails. Supplied means callbacks
accepted the writes, not that the GPU authenticated the image. The hardware
boot ROM performs verification later. There is no native binding yet.

The source must remain immutable and match the supplied CSS header; callbacks
must be bounded/nonraising, and caller must hold exclusive device/forcewake
ownership. Metadata allowlisting is not signature verification. Reference:
[Linux guc_xfer_rsa_mmio](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_fw.c.html#61).

Hosted `guc_rsa.gpr` checks all256 possible source-read failures and all64
register-write failures, little-endian register values/order, malformed size
and signature metadata, and zero-I/O retry rejection (324 cases). This is
regression coverage, not a formal proof or physical register validation.

## GuC DMA transfer helper (hosted only)

`Intel_GPU_GuC_DMA` implements the bounded CSS+code transfer stage, with a
limited one-shot attempt. It validates a page-aligned, below-4GiB GPU virtual
source and transfer bounds against the admitted GuC WOPCM region. These checks
do not prove ownership. It uses destination offset0x2000, excludes the signature,
and clears UOS_MOVE after completion/failure without claiming to cancel DMA.
Any attempt that writes remains quarantined unless transfer and cleanup both
complete. Resources are never freed by this helper, including on success.

Reference sequencing and register definitions:
[Linux uc_fw_xfer](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c.html#1084),
[GuC registers](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h.html#45),
[GuC upload](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_fw.c.html#297).

The caller must establish exclusive device ownership, reset, forcewake,
WOPCM configuration, GuC preparation, RSA delivery and coherent GGTT publication
before invoking it. Callbacks must be ordered, bounded and nonraising. A new
Attempt object is not an authority token; serialization is still external.
Completion means only the DMA START bit cleared and UOS_MOVE cleanup read back;
it does not establish firmware authentication, readiness, or working submission.

Hosted `guc_dma.gpr` tests 22 cases: exact register order/data, range boundaries,
busy/unreadable control, missing/regressing clock, deadline and stalled-clock
poll bounds, cleanup failure, quarantine and no-I/O repeated attempts. This is
regression evidence, not a SPARK proof or physical DMA validation. There is no
native instantiation yet, and the v29 hardware image is unchanged.

## Firmware address reservation audit (next integration)

The runtime parameter encoder now rejects bases at/above `0xFEE00000` and
log backing extents crossing that exclusive ceiling. Previously it accepted
up to4GiB, which incorrectly included the GuC-inaccessible upper region.
Reference: [intel_guc.h runtime address validation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.h.html#400).
Firmware upload DMA addressing is deliberately unchanged: uploading a firmware
image and giving running GuC a log/ADS pointer have different address rules.
The encoder now also requires an explicit nonzero page-aligned pin bias and
ADS backing length. Both complete extents must fit between that lower bound
and the runtime ceiling, and ADS/log intervals must not overlap. These are
numeric checks on supplied claims: deriving the correct pin bias, validating
actual allocations/mappings and constructing valid ADS contents remain native
admission obligations. Hosted tests cover 512 ADS/log page-set comparisons,
invalid lower bounds, ADS zero/unaligned/oversized lengths, exact ceiling fit,
one-page crossing and forbidden base addresses, alongside the composed upload
and actual packaged firmware fixture.

`Intel_GPU_GGTT_Reservations` now provides a limited, default-deny range ledger
for a single trusted aperture. Admission is one-shot; up to64 exact page-aligned
claims reject overlaps, out-of-aperture ranges and exhaustion. No release or
reset is exposed, so failed publications can retain their claims. The caller
must serialize use and establish the entire aperture's ownership first. Multiple
independent ledgers cannot confer independent ownership of the same hardware.
The publication helper now takes this ledger, derives table geometry from it,
and claims the exact range before invoking any callback. All acquired claims
survive every result. A fresh attempt with different backing cannot bypass
an earlier claim in that ledger. The raw table-size argument was removed.
Hosted fault-injection tests exercise this composition across successful,
pre-write-failed and ambiguously published attempts, plus default-deny and
outside-aperture cases. It is not yet wired to native MMIO; firmware/display
exclusions and correct device identity remain external admission obligations.

Hosted tests compare1296 interval pairs to a page-set intersection oracle and
cover capacity, re-admission and4GiB boundaries. Focused SPARK proves9 checks
(5 runtime,3 initialization,1 termination), with none unproved and no Assume.
Non-overlap semantics are regression-tested, not yet functionally proved.

Linux reserves a top-of-GGTT region for upload images, distinct from ordinary
GuC-shared allocations. See [ggtt_reserve_guc_top](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html#826).
Its [uc_fw_ggtt_offset and binding path](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c.html#1010)
assign firmware-specific offsets and prepare CPU cache visibility before binding.
These are reference implementation observations, not evidence that CuBit owns
those ranges on the NUC. Neither zero PTEs nor a successful GT reset establishes
that firmware display fetches exclude a proposed range.

CuBit's next integration must therefore:

1. Establish a single serialized GGTT owner after the reset handoff; retain
   active display mappings and account for firmware/platform exclusions.
2. Reserve upload storage separately from runtime GuC-visible communication
   buffers. Do not derive ownership by searching for zero PTEs.
3. Use the validated retained DMA allocation, not a reconstructed address or
   the firmware file's CPU address. `Intel_GPU_Firmware_Buffer.Prepared` now
   returns a discriminated descriptor only after complete copy/pad/readback;
   unsuccessful preparation exposes no ready descriptor. This is process-local,
   single-owner state, not a transferable capability or concurrency primitive.
4. Establish cache visibility before table publication, then invalidate the
   relevant translations. CPU readback alone is insufficient.
5. Keep ambiguous publications quarantined. Later DMA completion and GuC
   authentication are distinct milestones, neither implied by PTE readback.

The new descriptor is not wired to native GGTT writes. It records DMA and CPU
addresses separately, allocation capacity and actual content size, and does not
authorize unmapping or freeing the kernel-retained allocation. The published
v27 image remains unchanged and mapping-only.

Native descriptor validation: four-CPU UEFI RAM-GPU fixture `ron3uccy` passed,
including driver-side address/size invariant checks and the descriptor's
335360-byte content/1048576-byte capacity record reaching the desktop log.
This validates the success path in CuBit; the new getter's preparation-failure
paths have not yet been runtime fault-injected. No GPU publication occurred.

The retained-buffer preparation now performs an x86 CPU-cache writeback step
after copy/pad/readback and before publishing its descriptor: CPUID leaf1 checks
CLFLUSH support and obtains line size, rejects unusable geometry, then MFENCE,
one CLFLUSH per line across the entire owned 1MiB allocation, and MFENCE.
The inline assembly has compiler memory barriers. See Intel's
[instruction reference](https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-vol-2a-manual.pdf).
Unsupported cache flushing leaves the allocation retained and descriptor unready.
This adapter assumes compatible CPU features across scheduling migration and
exclusive CPU writes to the retained pages. It does not establish GPU-side cache
or TLB invalidation, IOMMU mappings, or correct GGTT/PAT cache attributes. Future
buffer modification requires another visibility operation before device use.

Native failure evidence: `run-live.py --intel-forcewake-fixture --boot-logs
--without-clflush` masks CLFLUSH in the emulated CPU model. Four-CPU UEFI
`e6_bvig2` reported retained cache-flush failure and an unavailable descriptor
through the desktop log, with no ready descriptor. Desktop continued to run.
This checks unsupported-feature handling, not cache flush effectiveness.

## Reference audit

Primary reference: [Linux v6.16 intel_reset.c](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_reset.c).
Register reference: [Intel GT register definitions](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_gt_regs.h.html).

The modern reset entry point acquires FORCEWAKE_ALL. Engine preparation uses
RING_RESET_CTL, handles catastrophic errors separately, and waits for reset
readiness. Requests are cancelled on exit, including preparation failure.
The first failed preparation does not proceed to reset. Linux's later retry
can force a reset, with explicit corruption/hang caveats; CuBit should not
adopt that fallback for initial handoff.

For Gen11+, full reset and GuC-only reset use different masks. GDRST is at
0x941c; full is bit0, GuC is bit3. GuC-only reset does not stop every engine.
The domain helper repeats successful reset on pre-12.70 hardware and delays
50 microseconds afterward because acknowledgment can precede settled state.
Individual media resets also have SFC preparation/cleanup requirements.

These are reference-driver observations, not proof of Intel silicon behavior.
Implementing only the GDRST write/poll would omit essential surrounding work.

## CuBit state and next implementation boundary

- Current native forcewake tests acquire/release only GT. They do not establish
  FORCEWAKE_ALL, engine readiness, engine stop, or exclusive reset ownership.
- Current v24 has retained firmware backing and read-only full-table GGTT
  inspection. It does not write PTEs, reset hardware or upload firmware.
- The hosted GGTT publication helper assumes a reserved range and exclusive
  writers. Its zero-entry preflight cannot supply those assumptions.
- Firmware scanout remains active. Retiring unknown render work must not
  discard its display mappings; display and render ownership stay distinct.

Before enabling the native publication adapter:

1. Inventory the supported ADL-N engines and required forcewake domains from
   validated device/fuse data. Do not probe nonexistent engines by assumption.
2. Provide a bounded, serialized multi-domain forcewake lease; partial
   acquisition failures must track which requests need cleanup.
3. Implement engine stop/preparation and cancellation with fault-injected
   adapters before issuing a full reset. No unconditional forced-reset retry.
4. Separate reset acknowledgment from settling and from broader handoff
   completion. A clock failure or ambiguous hardware response blocks writes.
5. Establish the GPU-VA reservation under that same ownership epoch, retaining
   firmware display ranges. Only then instantiate native PTE publication.

Until those conditions hold, keep published images on the existing inspection
and CPU-buffer-preparation path. Successful reset alone would still not prove
IOMMU isolation, shader safety, firmware authenticity or safe buffer reclaim.

## Hardware evidence: v24

The user reported the N95 boot with `ggtt=8388608 first=7C800001
present=532141 scanned=1048576` and firmware buffer
`prepared-retained (NOT GPU-published)`. This confirms completion of the
full-table inspection and CPU buffer preparation on this machine. The scan
is not an atomic snapshot; present entries do not establish allocation
ownership, free capacity, or a safe publication range.

## Multi-domain coordinator

`Intel_GPU_Domain_Lease` coordinates a caller-validated selection with bounded,
non-raising handshake callbacks. It acquires in enumeration order and releases
in reverse order, including after a partial acquisition failure. Cleanup
continues after a reported release failure. Failed acquisition and failed
release domains remain uncertain; a faulted lease cannot be reused. The failed
acquisition callback owns its own attempted request cleanup, as in the current
single-domain helper; the coordinator does not blindly repeat it.

This is not yet an ADL-N domain inventory or native FORCEWAKE_ALL adapter.
The hosted test's GT/Render/Media names are synthetic test cases, not a hardware
domain list. Native reset and publication remain disabled.

## ADL-N inventory decoder

Pinned reference files (Linux v6.16):
[platform table](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/i915_pci.c),
[media fuse handling](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_engine_cs.c),
[register definitions](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/gt/intel_gt_regs.h),
and [forcewake setup](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/intel_uncore.c).

ADL-N selects the ADL-P platform engine mask: RCS0, BCS0, VCS0, VCS2,
VECS0. Media disable bits at 0x9140 prune that mask; they do not add other
engines. This platform predates the newer enable-bit semantics and shared
media-slice forcewake exception. GT and render domains remain, with separate
domains for the surviving media engines.

`Intel_GPU_ADLN_Inventory` encodes this policy for 8086:46d2 only, rejects
all-ones MMIO, and returns an empty invalid inventory for other devices.
It is pure decoding, not a hardware probe: the caller must establish device
identity, power, successful ordered register access and stable ownership.
Register offsets are not capability grants. The native driver now reads the
fuse twice under its GT lease and admits the decoded inventory only after
matching reads and successful release. It logs the raw fuse and selected
domains. This compiled in the private native workspace, but is not yet in a
published image or hardware-tested. Additional domain handshake bindings and
the NUC's actual fuse value remain pending.

The subsequent native probe now binds the combined ADL-N helper to the existing
A000 writable alias. Reads/writes are restricted to the five known register
pairs and request bit patterns. After successful fuse capture and GT release,
it acquires the decoded selection and releases it again. Success logs
`domains-release-ready`; either phase's failure is explicit. This does not
hold forcewake across later operations or authorize reset/publication. These
changes are packaged in the private `cubit_n95_intel_domains_v25.img`.
Normal UEFI QEMU boot/log-viewer regression passed (run `zf_0kw37`); QEMU
does not emulate the Intel domains. NUC validation remains pending.

## Engine preparation helper (not enabled natively)

`Intel_GPU_Reset_Prepare` models one admitted engine's reset-control handshake:
already-ready, request-ready, and catastrophic-error hardware-clear branches.
Both elapsed microseconds and sample counts bound waiting; all-ones reads and
clock regression reject progress. Cancel issues the masked request-clear write
only: no claim that cancellation is acknowledged or the engine is quiescent.
The caller must cancel every selected engine even after failed preparation.

The Linux reset source also identifies Wa_22011802037 for pre-12.70 graphics:
engine reset requires exclusion of executing MI_FORCE_WAKE commands. The
forcewake lease and ready-to-reset bit do not alone establish this. Native
engine-stop/workaround handling remains a prerequisite, not an optional retry.

### Stop and pending-forcewake drain

Reference: Linux v6.16 `intel_engine_cs.c`, `__intel_engine_stop_cs`,
`__cs_pending_mi_force_wakes`, and `__gpm_wait_for_fw_complete` (linked above).
The stop path sets STOP_RING and disables prefetch before waiting for idle.
Pending MI_FORCE_WAKE fields are masked by their upper-half enable bits;
nonzero pending requests require a power-status acknowledgment and settling
delays before and after that acknowledgment.

`Intel_GPU_Engine_Stop` now implements that sequence through callbacks, with
elapsed and poll bounds and explicit invalid-MMIO/clock rejection. It refuses
the empty-ring fallback on an idle timeout. Failures leave requests asserted
and require quarantine, not automatic resumption of unknown firmware work.
It is hosted-only; no native engine-control register grant or reset is enabled.
Successful completion is not a proof of cache flushing, DMA isolation, or
correctness of the hardware/timebase assumptions.

### Full-domain reset handshake

`Intel_GPU_GT_Reset` implements the ADL-N two-write reset handshake with a
2,000us acknowledgment budget for each write and at least 50us settling after
the second acknowledgment. It uses the fixed full-GT mask, not PCI reset or a
caller-selected engine mask. An attempt is quarantined before any callback;
failure cannot be retried through that object. Neither a stalled clock nor
an all-ones MMIO read produces success. This helper is hosted-only, and its
external obligations still include held forcewake, stopped/drained engines,
successful preparation, exclusive ownership and preservation of scanout.
Failure leaves ownership/backing retained; cancellation is a separate caller
obligation. It cannot be invoked as a substitute for the complete handoff.

### Initial handoff coordinator

`Intel_GPU_Handoff` sequences held forcewake, all-engine stop, all-engine
preparation, reset/settle and cancellation. Preparation failure skips reset;
cancellation still visits every selected engine and continues after failure.
An uncertain result is terminal for the attempt. Success leaves forcewake
held for initialization; failure also retains ownership/backing rather than
resuming unknown work or allowing power-state transitions to save bad context.
No implicit release or resource reclamation occurs.

The hosted coordinator tests use Boolean stage callbacks, not the register
helpers or live MMIO. Binding the actual helpers, verifying the native
microsecond timebase and bounded register grants, and NUC testing are still
required. This is not yet a native reset path or hardware correctness proof.

### Register policy and timebase

The ADL-N inventory now supplies engine bases and pending-message registers
from the reference `intel_engine_cs.c`, `i915_reg.h` and `intel_gt_regs.h`.
`Engine_Write_Allowed` admits only selected engines' stop, prefetch-disable,
reset-preparation and cancellation patterns. It rejects restart, arbitrary
register values and GDRST (which requires its own guarded adapter).
This is a software allowlist inside the driver, not sub-page kernel isolation.

Native `SYSCALL_GETTIME` currently exposes `Time.msTicks`. The benchmark TSC
helper explicitly assumes cross-CPU agreement and uses millisecond calibration;
it must not silently become the reset deadline source. Sub-millisecond clock
binding remains work before these helpers can execute natively.

The native audit found a calibrated invariant-TSC path and a BSP epoch, but no
cross-CPU synchronization validation or configured TSC_AUX identity suitable
for the proposed driver clock. The deadline adapter must not assume those.
Also, floor-rounded microsecond timestamps can overstate a duration by almost
one microsecond, before hardware error. Minimum settling tests now require
differences of at least 3 or 52us respectively, budgeting 2us total short-interval
overstatement. Scaling msTicks by 1000 does not qualify.

HPET is a candidate shared-counter alternative. Boot already maps and disables
it. A counter-only source must mask interrupt and FSB outputs on every
comparator before enabling counting without legacy routing. The new pure
`HPET_Counter` helper supplies these transformations, 64-bit-counter admission
and split femtosecond-period conversion without a direct overflowing product.
Reference: [Intel HPET specification 1.0a](https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/software-developers-hpet-spec-1-0a.pdf).
It is not enabled at boot yet. Counter phase, reported period accuracy and
conversion quantization must be included in the delay error budget; the
earlier <=1us callback contract could not simply be assumed for this adapter;
it has been replaced by the explicit 2us short-interval contract below.

`HPET_Clock` now provides the one-shot startup/readback sequence through ordered
MMIO callbacks. It requires a 64-bit counter, masks every advertised comparator,
verifies configuration, and bounds the forward-progress check. Invalid reads,
counter regression, ignored configuration writes or no progress do not publish
an available clock. After an enable-stage failure it attempts to disable counting;
faulty hardware can ignore that write, so unavailable is not proof of quiescence.
Initialization must finish before concurrent readers and other timer owners are
excluded. It is now bound into kernel boot through `Platform_Monotonic` and
`Time.Read_Monotonic`; no userspace high-resolution syscall is exposed yet.
Four-CPU UEFI QEMU boot 3fw63ri_ passed with the counter-only backend ready.

The next binding uses the [common kernel timing boundary](kernel-monotonic-timing.md),
not GPU-specific HPET access. `Monotonic_Wait` now tests conservative minimum
delays with an explicit elapsed-time overstatement budget. Backend accuracy and
the native high-resolution interface remain integration obligations.

`Intel_GPU_ADLN_Reset` now composes the actual register-level stop, preparation
and GT reset helpers with the handoff coordinator. It binds the per-engine
offsets and shared `GEN9_PWRGT_DOMAIN_STATUS` at A2A0, and verifies each cancel
request bit is clear after its write. This is not a proof of DMA quiescence.
The instance is single-use after an admitted attempt and retains forcewake.
Hosted `adln_reset.gpr` now tests1152 combinations: all8 media-fuse subsets,
each/no engine stop failure, each/no preparation failure, reset failure and
cleanup failure. It asserts selected-only register accesses, no reset after
stop/preparation failure, cleanup of all selected engines after preparation/
reset, and no writes on rejected reuse. These use mocked registers, not an
Intel emulator; broader component fault tests remain separate.

The private bring-up device manager now implements request022D with a single
fixed-table index (0..5), not an arbitrary BAR offset, size or destination slot.
It revalidates the frozen8086:46D2 identity, D0 state and BAR and requires the
existing forcewake grant before minting one4KiB read/write page at slot16..21.
Duplicates are rejected; partial grants remain owned by the static claim.
There is no RAM-fixture grant bypass, runtime rebind or transaction rollback.
The six offsets are9000,2000,22000,1C0000,1D0000,1C8000. These grant entire
pages, including other registers in those pages; the driver write allowlist is
not a security boundary against a compromised driver. The physical adapter
must still limit operations and preserve display state. The driver now calls
022D after a valid inventory and successful domain probe, with high-resolution
clock availability required. All six grants/maps share a30s deadline and use
distinct aliases at61200000 + index*4096. Only canonical busy replies are
retried. Partial grants/maps stay retained; no reset starts on partial success.
This build reports ready (NOT reset), without writing through those aliases.
Main-checkout devmgr has not been overwritten with the divergent private
bring-up implementation; reconciliation remains explicit pending work.

`Intel_GPU_Native_Reset` now binds the composed sequence to the approved
aliases and common high-resolution syscall. It checks read/write offsets,
write values and admitted engines/domains, latches access rejection, and keeps
its forcewake instance and single-attempt state alive after return. The driver
requests canonical022E permission from its bound manager after all six mappings
exist and preparation/logging completes. The private device manager revalidates
the frozen identity, D0 state, BAR and issued grants, consuming the one-shot
permit before replying. This replaces the unused0230 push command, which could
arrive during startup IPC. It is trusted-driver sequencing, not register-level
isolation once writable pages are granted. Native build97173 passed; no hardware
reset or native adapter execution is claimed. Publishedv26 deliberately predates
this adapter.

NUC v26 feedback: `reset pages clock-unavailable`. The driver rejected mapping
before requesting any022D grants. Earlier inventory and domain probing gates
passed, but the common microsecond read was unavailable. The precise HPET
failure is not yet known; do not substitute scheduler milliseconds or remove
the clock guard. New retained startup-stage diagnostics have hosted regression
coverage but are not yet in a published image.

## Linear scanout exclusion geometry (2026-09-28)

`Intel_GPU_Scanout_Range.Linear` now calculates a conservative page-rounded
GGTT extent for an already-decoded linear packed-pixel surface. It includes
leading source-offset rows and row padding, validates row width against pitch,
and rejects ranges outside the discovered table aperture. This is a building
block for exclusion accounting, not permission to admit an aperture. No native
scanout register reads or publication are enabled by this helper.

The native inventory must distinguish programmed from currently scanned-out
addresses: Intel's register definitions expose both `PLANE_SURF` and
`PLANE_SURFLIVE`. Until ownership and transition state are established, retain
both, across every enabled plane and cursor. Never feed tiled/compressed/planar
register state into the linear helper. Register decoding, display power-domain
access, stable sampling, platform reservations and hardware prefetch rules
remain separate obligations.

Reference: [Intel/Linux plane register definitions](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/skl_universal_plane_regs.h.html).

`Intel_GPU_Plane_Decode` now translates paired ADL-N universal-plane samples
into that geometry for basic, unrotated linear RGB8888. It explicitly accepts
only control values 0x84000000 and 0x84100000; other control features, reserved
stride/address bits and all-ones inputs are rejected. Linear stride units are
64 bytes and size fields encode dimensions minus one, following
[Intel's Linux plane programming code](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/skl_universal_plane.c.html).
Matching samples alone do not establish atomicity. A programmed/live address
mismatch is rejected even if both samples match: pending geometry cannot be
assumed to describe the previous live allocation. `Disabled` is an observation,
not authorization to free the old buffer. Native collection, power access and
ownership admission remain unimplemented; this decoder performs no MMIO.

`Intel_GPU_Plane_Collect` now supplies the bounded read sequence around this
decoder. Its adapter must acquire/hold a display power reference and serialize
display changes for Begin..End, selecting one admitted plane's six read-only
registers. The collector makes at most twelve reads, stops on errors/sentinels,
and always attempts End after a successful Begin. An End failure suppresses
the decoded extent too. The generic has no register-write callback. The native
power/register adapter is still missing; do not substitute GT forcewake for a
display-power reference or bind this directly to unqualified BAR reads.

### ADL-N power-map audit and native diagnostics

Linux identifies ADL-N as an ADL-P subplatform with Xe-LPD display information:
[platform mapping](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/intel_display_device.c.html).
The [Xe-LPD power tree](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/intel_display_power_map.c.html)
places PWA below PW1; PWB/C/D additionally depend on PW2. It is not the older
TGL sequential well chain. Request/state pairs use well indices 0, 1 and
5/6/7/8 for PW1, PW2 and PWA/B/C/D respectively:
[register definitions](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/intel_display_regs.h.html).

The existing native read-only snapshot now logs `display PW request+state
ABCD=... (snapshot only)`. Both request and acknowledgment bits must be set
for each pipe's well and ancestors; all-ones is rejected. The previously
reported driver value 0xFC0F decodes to YYY-, not a claim all four pipes are
powered or present. This diagnostic introduces no additional MMIO reads.

This is not a lease: ownership of inherited requests, fuse distribution,
DC-state handling and serialization still need implementation. Do not use the
diagnostic Boolean as Begin_Access for the plane collector. The native
collector remains unbound and power-register writes remain disabled here.

### Authority audit: inherited requests are not a managed reference

The current `devmgr/main.adb` static claim freezes PCI configuration through
`pciWriteConfig16/32`, remembers BAR/identity, and rejects suspend/rebind by
policy. The 0x022E one-shot authorization additionally rechecks D0, identity,
BAR and reset-page grants. Its scope is the GT reset sequence, not acquisition
of a display-power reference. The broad inspection mapping is read-only;
subsequent writable grants cover forcewake/reset pages, not 0x45404.

A source audit found no current CuBit writer of the display power-well request
register. That absence is not a maintainable ownership contract: adding a
future power manager must not silently invalidate an inherited-request lease.
Do not implement Begin_Access as a Boolean test of the snapshot or reuse the
reset authorization as a display-power lease.

The next native boundary must explicitly designate the sole power-request
owner for this device generation and serialize all future request changes.
It must distinguish inherited requests from requests added by CuBit, retain
ancestor wells across every inspection, and never clear inherited bits during
cleanup. Failure after a possible request write must retain/quarantine the
reference state rather than report a successful release. Device restart and
rebind must not create a second owner while old mappings or references exist.
Actual enable paths additionally require the documented DC/fuse/workaround
sequence; this audit does not authorize skipping those steps for firmware-on
wells. No native register collection was enabled by this audit.

### One-shot display-power owner designation

The native broker now implements request 0x022F, with an exact empty envelope
and the Intel inspection endpoint badge. Only the designated Intel PID may
request it. The broker rechecks frozen configuration, N95 identity, D0 and
unchanged BAR, then consumes `Intel_GPU_Display_Claim` before replying.
Startup wait loops defer the request rather than consuming it. Retry after
success or lost reply is denied; no release/rebind API exists. This follows
the current static device-lifetime policy, not a general hot-restart design.

The Intel driver requests designation with bounded completion waiting and
logs the result. Reset authorization 0x022E remains distinct. Designation
grants no writable page and does not itself hold a hardware power reference;
the native collector therefore remains disabled. Future request-register
grants and power lifecycle operations must require this same retained owner.

Hosted tests cover 36 admission combinations and 612 retry/rebind rejections.
SPARK proves the one-shot state transition and owner preservation on denial.
Authentication, fresh PCI evidence and reply ordering in the native broker
are integration obligations outside that pure proof. This is not evidence
that the IPC exchange has run on the NUC.

### Forcewake failure budget diagnostics

Forcewake now distinguishes `deadline-expired` (the elapsed-time check reached
the deadline) from `poll-budget-exhausted` (the finite sample cap ended while
the last time sample was still in budget). Both retain the existing faulted
ownership/cleanup behavior; neither authorizes a retry or extends the wait.
The native reset detail retains the first failed domain, acquire/release phase
and last ACK, including when later cleanup fails. A poll-budget report alone
does not establish that the hardware failed to acknowledge within 50 ms.

Hosted tests cover stopped clocks, delayed ACK, elapsed deadlines, final-sample
success and deadline precedence on the final sample. These are regression
tests, not proof of real-time scheduling or hardware completion. The normal
live image must be repackaged to include changes to the staged driver.

### Display reference lifetime coordinator (hosted, not native-bound)

`Intel_GPU_Display_Lease` separates display dependency cleanup from the GT
domain lease. Its generic well enumeration is ancestor-first; the backend
validates an ancestor-closed selection before any hardware callback. One
package instance belongs to one serialized, designated owner. Acquisition
records uncertainty before each callback. On failure it retains the acquired
ancestors and permanently faults the instance, rather than attempting a
rollback that might power down a partially enabled descendant.

Successful acquisition distinguishes added from inherited requests. Release
walks descendants first and invokes software reference release for every held
well, passing whether its request was added. Hardware request removal is only
permitted for added requests. Release stops immediately on failure. Released descendants remain
released; the failed well and its ancestors remain retained. No callback is
allowed after quarantine; a complete successful release permits reuse.

This does not implement power register writes. Hardware callbacks must still
hold runtime power, handle DC/fuse/workarounds, validate the inherited baseline
and ensure removal of an added request cannot deprive an inherited consumer
of power. The coordinator cannot derive those facts from request bits.
Firmware baseline ownership is not discarded when a temporary reference ends.
There is no native binding and no SPARK proof of this callback implementation.
Hosted tests cover 128 inherited/acquisition/release combinations, invalid
selections, ordering, quarantine, no inherited clears, and successful reuse.

### ADL-N pipe-access power topology

`Intel_GPU_Display_Topology` supplies the platform selection validator for
the lifetime coordinator. Pipe A selects PW1/PWA; B selects PW1/PW2/PWB;
C and D also select DC-off before their pipe wells. This is the pipe-access
subset, not an assertion that DDI, AUX, audio or every transcoder shares that
selection. Unsupported bits and selections missing ancestors are rejected.
DC-off is explicitly not represented as a bit in the request register.
The native observation diagnostic now derives its request/state mask from
this topology instead of duplicating four numeric masks. Its existing hosted
test retains an independent register-index oracle, so a topology mistake is
not hidden by computing expected values with the same helper. This integration
adds no reads and does not change the snapshot-only status of that diagnostic.

Reference: Linux v6.16
[Xe-LPD map](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_power_map.c),
[power-well enable sequence](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_power_well.c),
and [register definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_reg.h).
The native backend must still implement PW1's ADLP FLR-source workaround,
PG0 pre-enable fuse wait, request/state handshake, target fuse wait and
post-enable work. The source also distinguishes clearing our request from
observing a well powered off: other BIOS/debug/KVM requesters may keep it on.
Do not treat that as a release timeout without checking requester ownership.
No power-register backend or native plane collection was enabled here.

### Per-well enable transaction (simulated MMIO)

`Intel_GPU_Display_Enable` implements the request-well portion of that sequence:
PW1's FLR-source workaround, PG0 fuse wait, request read/modify/write,
request-plus-state acknowledgment and the target well's fuse distribution wait.
It refuses all-ones reads, changed inherited request state, unavailable or
regressing clocks, and failed writes. Each wait has a 1 ms deadline plus a
sample cap; a late successful read cannot override an expired deadline.
Failure does not attempt rollback. Failed attempts permanently quarantine
the instance, including callback exceptions; `Added` is set before attempting
a new request write. A successful matched release permits reuse.

The read/modify/write preserves unrelated bits. An already-set inherited
request is not rewritten, but still requires state and fuse confirmation.
Post-enable IRQ/VGA handling remains a mandatory callback. This unit does
not acquire DC-off, authenticate ownership, validate ancestor references or
establish firmware baseline ownership; those are prerequisites for its trusted
caller. There is no native instantiation
or additional MMIO grant, so the live image remains unchanged.

Hosted tests cover 156 combinations over six request wells, both inherited
states and 13 success/failure modes. Exact per-well fuse bits are supplied
independently in the model; broad all-ready values are not used. This is
regression evidence, not SPARK proof or physical NUC validation.

The matching `Release` now uses the instance's recorded successful acquisition,
not a caller-supplied claim that a request was added. Inherited requests cause
no callback/MMIO activity. Added requests run Pre_Disable, read/modify/write
only their own request bit and verify that bit cleared. The STATE bit may stay
set because other requesters retain the well; release does not claim physical
power-off. Failures after Pre_Disable or a possible write permanently quarantine
the instance, leaving ancestor retention to the coordinating display lease.
The caller still owes descendant ordering and baseline ownership. Eighty-four
release combinations test inherited handling, pre-disable/read/write/readback
failure, unexpected request changes and successful reacquisition. Native IRQ/VGA
callbacks, DC-off release policy and integration remain unfinished.

Composition regression: the coordinator originally skipped inherited release
callbacks entirely. That preserved register bits but left the per-well
transaction's software `Held` flag set, causing the next acquire to fail.
The callback now receives an explicit `Added` flag and always retires the
software reference; the per-well implementation performs no hardware access
for an inherited release. Tests compose the real coordinator and per-well
transaction for all six request wells, both inheritance states and three
consecutive acquire/release cycles (36 cycles), asserting zero inherited-release
MMIO/hook calls and preserved inherited request bits.

### Composed pipe-level power backend

`Intel_GPU_Display_Power` now connects the six request-well transactions,
Xe-LPD topology and dependency-preserving lease. One designated serialized
instance admits one pipe reference at a time. An unready trusted authority
gate rejects before callbacks; re-acquire while held cannot alter the polling
budget or touch registers. Successful release supports another pipe selection
on the same instance. Faults retain the affected ancestors and forbid reuse.

DC-off remains an explicit complete-reference callback, separate from the
low-level DC write verifier. IRQ/VGA enable/disable callbacks are also explicit.
This avoids falsely marking those platform obligations implemented when only
the request-bit handshake is available. No native instantiation or new mapping
grants are enabled. Concurrent multi-pipe references are not implemented by
this serialized inspection-oriented backend.

Hosted shared-register tests cover 512 combinations of pipe, inherited
baseline, acquisition failure and release failure, plus 32 cross-pipe reuse
transitions. They check ordering, DC-off selection, ancestor retention,
authority rejection, unchanged unrelated control bits and fault quarantine.
These are integration regressions for the components, not hardware validation
or a proof of native power lifetime correctness.

### DC-state write verification boundary

`Intel_GPU_DC_State` provides a pure retained-configuration check for ADL-N's
CDCLK_CTL, PLL enable/ratio/reference and four DBUF slices. Before/after
snapshots must contain no sentinel values or unknown reference-clock selector.
CDCLK control, reference selection and PLL enable/ratio must stay unchanged;
the post-exit PLL lock must match enable, with no frequency-crawl request/ack
in flight. DBUF configuration/request bits must be preserved, while post-exit
power acknowledgments must match those requests. Pre-exit acknowledgments
need not already be settled. Tests cover all 256 request/state mask pairs,
sentinels, clock changes and unstable/invalid clocks; the preservation contract
is discharged by SPARK. Native snapshot collection and PHY restoration remain
incomplete, so this is not yet a complete DC-off acquisition.

`Intel_GPU_Native_DC_State` now implements read-only collection of these seven
registers twice, checking retained PW1 before every read and after collection.
It exposes a baseline only when all fourteen reads succeed and both samples
match. Sentinel values, power loss and changing samples return no baseline.
A hosted fixture executes the actual adapter loads at independent literal
addresses and tests every first-pass sentinel and every power-check failure,
plus a sample changed between passes. This remains host-memory evidence:
the adapter is not yet connected to startup or built for native CuBit. Matching
samples still do not constitute an atomic hardware snapshot.

The DBUF registers are explicitly listed, not a uniform stride: 0x45008,
0x44FE8, 0x44300 and 0x44304. ADL-N's Xe-LPD descriptor has all four slices.
This path assumes the ADL-N descriptor without CDCLK squashing; it must not be
used as a generic future-GPU validator. Sources: Linux v6.16
[clock readout](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/intel_cdclk.c),
[DBUF registers](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/skl_watermark_regs.h),
and [platform descriptions](https://raw.githubusercontent.com/torvalds/linux/v6.16/drivers/gpu/drm/i915/display/intel_display_device.c).

`Intel_GPU_DC_Exit` now composes that primitive for the ADL-N boot path:
validate ownership/PW1, capture the control value, check clock availability
before DC3CO writes, clear its status bit, verify the disabled target through
`DC_Write`, then wait at least 200us from a fresh post-write timestamp. The
wait has an independent sample limit and rejects unavailable/backward clocks.
The DC-enable mask is 0x4000000B; unrelated bits, including PHY latch bits,
are preserved. A required callback then validates/restores CDCLK, DBUF and
combo-PHY state before a final DC-mask read. Only then is `Ready` returned.

The callback contract is an outstanding native implementation obligation,
not a stub that may return success unconditionally. Inherited DMC/PSR state
must be coordinated by the caller. Fourteen hosted cases cover DC3CO/non-DC3CO
ordering, latch preservation, sentinel reads, write failure, unavailable or
stalled clocks, restoration failure, re-enabled DC detection and no replay.
This does not yet enable C/D or prove physical exit behavior; there is no native
binding, and the current published image contains no DC-exit changes.

The v6.16 `gen9_write_dc_state` implementation above documents DC6 writes
that sometimes revert and checks repeated readback. `Intel_GPU_DC_Write`
implements a separately bounded low-level counterpart: an initial write,
seven consecutive exact matches, reset/rewrite on mismatch, independent
read/write limits, rejection of all-ones reads, and no unverified final rewrite
when the read budget is exhausted. The write limit includes the first write.
The caller constructs the full register value safely; this primitive does not
choose a low-power policy or silently overwrite unrelated fields.

`Stable_Register` is only readback evidence, never a DC-off reference. Native
integration still owes DMC coordination, the DC3CO status-clear/200us exit path
when applicable, clock and DBUF validation, and combo-PHY restoration. The
upstream code notes that the MMIO disabling write itself can block during
state restoration: a software sample limit cannot preempt that bus operation.
No native binding/write grant is enabled. Tests cover 7290 combinations plus
two periodic-glitch cases; no SPARK or physical-hardware claim is made.

### ADL-N DC baseline evidence

The newer DMC wake-lock protocol is not an ADL-N requirement: Linux v6.16's
`HAS_DMC_WAKELOCK` selects display version 20 and newer; ADL-N uses Xe-LPD
display version 13. See the pinned
[device definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_device.h)
and [wake-lock gating](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_dmc_wl.c).
This does not remove DMC-managed low-power states or the other DC-exit work.

The native read-only observation now also captures DC_STATE_EN (0x45504) and
display fuse status (0x42000), publishing `display DC=... fuses=...` to logstore.
The admission predicate requires all four registers to fit before any read.
Raw all-ones values remain visible as diagnostic evidence, not accepted power
state. A zero DC enable mask alone does not hold a power reference or prove
that firmware cannot change state. These values will establish whether the
NUC needs an actual DC exit before inherited-plane inspection.

### IRQ/VGA handoff audit (2026-09-28)

Linux v6.16's power-well post-enable hook restores pipe IRQ registers only
when its interrupt subsystem is enabled; pre-disable resets those registers
and synchronizes its handler. CuBit has no Intel IRQ handler/grant yet, but
that does NOT prove firmware disabled MSI, MSI-X, INTx or GPU IRQ sources.
The broker freezes PCI configuration without establishing that transition.
Do not replace the power-well callbacks with unconditional success.

Devmgr now logs a read-only PCI interrupt snapshot before freezing config:
INTx disable, MSI presence/enable, MSI-X presence/enable/function-mask, and
validity. Invalid results clear all observations. The bounded walk rejects
cycles, duplicate recognized records, truncated recognized records and links
into their payloads. Unknown capabilities are treated as headers only, not
fully validated. Bootstrap v4 carries the snapshot to Intel's logstore
publisher so the desktop diagnostic viewer can show it without serial output.
The old v3 message is rejected; no undeployed compatibility path is retained.
All 256 encoded bytes are checked against an independent validity oracle and
round-tripped where valid. Reserved bits and contradictory presence/enable
states reject the bootstrap; invalid acquisition is represented only by zero.
A valid snapshot is neither atomic nor a quiescence
guarantee: no GPU source masks, pending CPU vectors, MSI-X table or routing
are inspected. Native hooks still need an explicit broker-owned mask/drain
policy; disabling PCI delivery alone does not drain in-flight interrupts.

Linux's VGA helper touches Misc Output to avoid unclaimed-register interrupts
on later legacy VGA access. CuBit's linear-framebuffer path avoids VGA status
polling, but kernel text-mode fallback still exists. Omitting that hook needs
an explicit no-legacy-access handoff rule, not a blanket no-VGA assumption.

Pinned references: Linux v6.16
[power-well hooks](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_power_well.c),
[IRQ lifecycle](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_irq.c),
[VGA helper](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_vga.c),
[PCI definitions](https://github.com/torvalds/linux/blob/v6.16/include/uapi/linux/pci_regs.h).

### Forcewake failure: missing fallback handshake (2026-09-28)

Audit of pinned Linux v6.16 `intel_uncore.c` found that graphics versions 11+
select `uncore_get_fallback`, including ADL-N. CuBit's current handshake only
waits for kernel ACK clear, sets request bit 0, and waits for ACK set. It has
no fallback. Upstream documents a collision between hardware wake requests
and the driver's request which can suppress the driver's acknowledgement
(HSDES 1604254524). This is a plausible explanation for a reacquisition failure
after the earlier probe succeeded, NOT a confirmed explanation of the NUC log.

The upstream fallback waits for fallback ACK clear, sets request bit 15,
delays 10 * pass microseconds, waits for fallback ACK set, samples the original
kernel ACK, and clears bit 15. It makes up to ten passes. Register definitions
confirm kernel bit 0 and fallback bit 15; masked fallback writes are therefore
0x80008000 (set) and 0x80000000 (clear). Both current native adapters explicitly
reject those writes. Merely adding a retry in the generic helper would fail
at that boundary. Normal multi-domain acquisition also differs: Linux issues
all domain requests before waiting for all set acknowledgements; CuBit waits
per domain. Neither difference has yet been established as the failure cause.

Next implementation must combine the fallback with the existing authenticated
domain/register allowlist, a real microsecond delay, bounded total time and
sample budgets, and preserved original failure evidence. Require valid MMIO
and a valid monotonic clock; an all-ones register must never satisfy ACK-set.
Keep bit-15 cleanup explicit on every attempted fallback and quarantine on
uncertain cleanup; never promote a failed or timed-out handshake to ownership.
Tests must cover both clear/set recovery, delayed original ACK, fallback ACK
failure, late completion, stalled/regressing clocks and cleanup uncertainty.
Do not copy Linux's best-effort continuation after failed waits as a success.

Linux additionally resets forcewake request bits during initial domain setup
(Gen12 mask 0xefff, preserving bit 12). CuBit does not currently do this; do not
add a broad reset speculatively because it changes inherited ownership.

Sources: [v6.16 forcewake implementation](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/intel_uncore.c)
and [GT register definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt_regs.h).

#### Bounded recovery implementation

`intel_gpu_forcewake_fallback` now implements up to ten bit15 toggles with
10*pass microsecond settling delays starting after the set write returns.
One 50ms budget spans a recovery invocation, with finite poll limits for
each wait/delay. It rejects all-ones MMIO, unavailable/regressing time, late
reads, unacknowledged fallback transitions and an original ACK that disappears
during cleanup. Success requires fallback clear and the desired original ACK
in the same final sample. Failures after asserting fallback attempt its clear
but do not assert cleanup completion or ownership. Callbacks must remain
bounded and non-raising; unexpected exceptions leave outer leases quarantined.

The single-domain lease invokes optional recovery only for Timed_Out or
Poll_Exhausted. Normal waits retain their original shared deadline; each
recovery invocation has its own additional bounded budget. Thus an acquire
can invoke recovery twice (clear and set), not spend an unbounded retry period.
The default callback does nothing. Only the native-reset adapter currently
binds recovery and admits the two masked bit15 writes on inventory-selected
domain registers; initial forcewake probing is unchanged. No broad inherited
request reset or multi-domain batching change was made.

Hosted tests exercise twenty clear/set recovery cases plus real lease acquire
and release with suppressed original acknowledgements. Existing five-domain
handshake and 288 composed-failure/two delayed-ACK cases still pass. These are
simulated hardware tests, not SPARK proof or confirmation of the NUC cause.
The native reset report appends a literal `fallback=` outcome when attempted.
The first failing fallback outcome is retained even if coordinator cleanup
later recovers another domain. Three additional admission regressions ensure
invalid MMIO and clock regression cannot invoke recovery, and unsuccessful
recovery leaves the lease faulted with no reusable ownership.

For the next NUC run, capture the entire `native reset` line. `fallback=recovered`
only confirms this wake handshake; it does not mean engine reset or GuC upload
succeeded. The leading reset outcome remains authoritative. Retain the PCI IRQ
and DC/fuse snapshots as well. No change to the initial probe means a failure
before `native reset beginning` still uses the normal forcewake path.

### Next firmware integration gate: ADS/private data

Pinned v6.16 `uc_css_header` places `private_data_size` at byte120, in bytes
(not DWORDs). The current selected-firmware admission already checks that
word equals0x00801000: **8MiB +4KiB**. This is not the335360-byte firmware file
size or WOPCM upload extent. The native1MiB staging allocation holds firmware
and logs; it cannot also back the required private-data area.

Linux places the page-rounded private area at the end of its ADS allocation,
after the ADS header, policies, system info, usage records, register lists,
golden contexts, workaround KLVs and capture lists. Consequently the full ADS
allocation must exceed8MiB+4KiB, with its size derived from those sections.
Do not substitute a zero page for ADS or guess a fixed8MiB allocation. The
existing startup parameter encoder validates address/extent representation,
not initialization of the data behind the ADS pointer.

Native startup still needs an owned retained mapping for that whole allocation
below `GUC_GGTT_TOP`, initialized absolute GPU pointers and engine information,
and coherency before publishing startup words. Firmware-image mapping, ADS
mapping and log mapping must be separate non-overlapping reservations. Memory
allocation alone cannot close the currently missing GGTT/scanout-ownership
boundary. No new allocations or GPU publication are enabled by this audit.

There is a useful initialization ordering distinction: upstream reserves and
describes golden contexts before firmware startup, but fills their saved engine
state after engines are operational. Do not create a circular dependency by
requiring executed golden-context capture before GuC startup; equally, do not
advertise watchdog recovery until that late initialization is complete.

Sources: [CSS ABI](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_uc_fw_abi.h),
[firmware parser](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c),
[ADS layout and early/late initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_ads.c).

`intel_gpu_ads_layout` now plans the packed storage layout. Pinned ABI sizes
are ADS4572 bytes, policies96, system info640 and engine usage16384; register
records begin at21692. These derive from16 engine classes/32 instances,
8-byte register-set descriptors and32-byte usage records, not native Ada
record alignment. Golden-context, workaround, capture and private sections
begin at4KiB boundaries, and total backing rounds up to4KiB. Register lists
must contain whole16-byte entries; workaround storage is DWORD-sized.
Zero-sized optional sections are allowed by this numeric planner, not evidence
that omitting them is valid for the selected firmware configuration.

The planner checks each addition before performing it, rejects exceeding the
runtime GGTT ceiling or actual backing capacity, and validates ordered disjoint
extents before returning a valid layout. Tests independently compute offsets
and exact-capacity admission over1089 layouts plus hostile sizes. GPU base plus
extent validation, firmware metadata admission, section contents, ownership,
coherency and native allocation are still required separately.
SPARK level2 proves runtime checks, termination and the valid-result extent
postcondition (backed by the final `Sound` validation). Exact ABI constants and
layout equivalence to upstream remain reviewed/tested assumptions, not proved
firmware compatibility.

#### Native ADS backing (not initialized ADS)

After a successful native reset only, Intel requests a separate16MiB retained
allocation through supervisor opcode0x0230. Devmgr fixes order12, CPU address
0x62000000 and a4GiB physical ceiling; the driver cannot choose size/address.
The broker authenticates the designated PID/badge and empty envelope, requires
reset authorization/frozen claim/GGTT inspection grant, and consumes the single
attempt before allocation. Repeated requests return the same retained backing
or denial, never allocate repeatedly. Reset success is checked in the driver;
broker authorization alone is not evidence of hardware reset completion.

The driver validates the exact reply, bounds its completion wait, zeroes and
reads back the full allocation, and publishes a backing descriptor only after
success. It does not treat that descriptor as initialized ADS: no section data,
device coherency operation, GGTT PTE or startup parameter is published. Failure
retains backing. The contiguous below4GiB requirement can fail under pressure;
future scattered backing must preserve the same lifetime/ownership contracts.
The existing kernel retained-DMA quota is64MiB globally; this allocation uses
16MiB of that budget and does not change the kernel API or quota.

Native services compile, and hosted tests cover8192 physical alignment/ceiling
cases plus ADS capacity checks. Actual16MiB allocation/readback remains untested
on the Intel hardware path; QEMU's virtio GPU does not exercise it.

Review found and fixed an admission bug: allocator `alloc` and `allocBelow`
both search through order12
inclusively; it is not a sentinel. DMA admission now rejects only orders above
that bound, and the kernel builds. The native quota fixture now requests four
16MiB allocations (still64MiB total), retaining its disjointness/exit/loan and
ceiling tests, in isolated current-source workspace
`.build-workspaces/dma-max-order-r37_25jh`.

Native validation update: the four16MiB fixture passed under four-CPU UEFI
QEMU in that workspace (`cubit-usb-live.yxpwf9ka`). It covers constrained
allocation, retained disjoint ranges after exit, deferred CPU loan return,
quota rollback on failed requests and full64MiB quota remaining consumed.
This establishes native DMA allocation admission/retention, not ADS section
initialization, full ADS-buffer readback, GPU coherency or device DMA access.

#### ADS scheduling policy encoding

`intel_gpu_ads_policies` encodes the packed96-byte policy section explicitly
in little-endian bytes, independent of Ada record layout. Following pinned
v6.16 `guc_policies_init`, queue-depth entries/reserved words stay zero,
DPC promotion is500000 microseconds, max work items15 and `is_valid` is1.
The caller must explicitly choose whether engine reset is permitted; otherwise
bit0 disables firmware engine reset. This is not permission to enable recovery
before golden contexts and the recovery path are ready. The encoded `is_valid`
field is part of offline construction, not a publication fence or ownership
claim. No native ADS initialization or GPU publication is enabled by this helper.

#### ADS engine information

`intel_gpu_ads_engines` encodes the first576 bytes of `guc_gt_system_info`:
512 mapping bytes and16 little-endian physical engine masks. All unused mapping
entries are32 (the ABI invalid instance), not zero. GuC class IDs differ from
CuBit's engine enum: render0, video1, enhancement2, blitter3. The ADL-N video
mapping compacts enabled physical instances0 and2 into logical indices0..1;
with only VCS2 enabled, logical0 maps to physical2 and the video mask is4.
The reported NUC fuse0x000E00FE enables only VCS0 of that pair.

This follows pinned v6.16
[mapping and mask initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_ads.c)
and [logical ID assignment](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_engine_cs.c).
The helper rejects invalid inventories or missing ADL-N render/copy engines.
It does not encode the remaining64 bytes of generic system information:
slice count, SFC availability and doorbell count must come from validated
hardware observations, not fabricated defaults. No native publication yet.

#### Register-save list construction

Pinned `guc_mmio_regset_init` builds each engine's list from ring mode (masked),
hardware-status-page address, interrupt mask, engine workarounds, nonprivileged
register whitelist slots, local MOCS and seven EU performance-control registers.
The RCU-mode condition additionally depends on compute engines. Workaround and
EU performance entries require explicit valid MCR steering; GuC does not inherit
the driver's default steering. This is a prerequisite for a complete ADS list,
not permission to publish only the three basic ring registers.

`intel_gpu_ads_regset` provides a bounded256-entry per-engine construction
buffer, sorted by offset when built through `Add` from its empty default.
Exactly identical duplicates are accepted; conflicting masked/steering flags
are rejected without mutation (stricter than upstream's first-offset-wins).
Full capacity fails explicitly instead of silently truncating. Register offsets
must be DWORD-aligned and fully inside the caller-supplied MMIO extent; that
extent is not a substitute for a register allowlist or device authority.

The16-byte wire encoder uses explicit little-endian offset/value/flags/mask
fields. Only ordinary save/restore, masked and steered forms are expressible;
value and mask remain zero. Explicit-value and restore-only forms are not yet
admitted. Steering IDs fit their four-bit ABI fields, but the caller must still
validate that the selected group/instance is present and nonterminated.
Complete engine workaround/whitelist/MOCS configuration, register allowlisting,
steering selection and native ADS integration remain required.

`intel_gpu_adln_regset.Build_Common` now constructs54 common entries for an
enabled ADL-N engine: RING_MODE_GEN7 atbase+0x29C (masked), HWS_PGA at+0x80,
IMR at+0xA8, twelve whitelist slots+0x4D0..+0x4FC,32 local MOCS registers
0xB020..0xB09C, and seven explicitly steered EU performance-control registers
0xE458/0xE558/0xE658/0xE758/0xE45C/0xE55C/0xE65C. Local MOCS entries are
non-MCR for this pre12.55 platform. The distinct RING_MI_MODE at+0x9C used
for stopping engines is not substituted for RING_MODE_GEN7 in this list.
Offsets/counts come from pinned
[engine registers](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_engine_regs.h)
and [GT registers](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt_regs.h).

The builder fails without a valid inventory/steering result, for disabled
engines or insufficient MMIO extent, and discards partial output on failure.
Its `Ready` flag means only that the common subset was constructed; it is not
a complete register list. Engine workarounds must still be merged (preserving
their flags), and actual register state must be initialized before firmware
captures/restores it. This code performs no register reads/writes/publication.

#### ADL-N nonterminated steering discovery

The pure `intel_gpu_adln_steering` decoder follows pinned v6.16
[Gen12 topology discovery](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_sseu.c),
[default steering setup](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_workarounds.c),
and [L3BANK overrides](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt_mcr.c).
It requires slice-enable0x9138 low8 bits equal1, a nonzero six-bit geometry
DSS-enable mask from0x913C, and a nonzero four-bit L3-bank enable mask obtained
by inverting the disable bits at0x9118. All-ones raw reads fail closed.
Other reserved bits are ignored; the caller must validate identity, stable
reads and forcewake access. The decoder does not itself read hardware.

Group0 and the lowest enabled DSS select the default target, because under
render power gating forcewake may power only that minimum configuration.
If that DSS index also names an enabled L3 bank, default steering suffices.
Otherwise registers0xB100..0xB3FF select the lowest enabled L3 bank explicitly.
Do not use arbitrary enabled DSSs or assume index0 is present.

This selection can populate GuC register-entry steering fields, but programming
the CPU MCR selector is separate: read/write serialization, multicast semantics
and restoration of selector state must be designed before native MCR access.
The helper makes no selector writes. The native driver's initial GT-forcewake
probe now collects two complete slice/DSS/L3 samples through its existing
read-only fuse-page mapping. It admits the decoded topology only after GT
release succeeds, the media-fuse inventory validates ADL-N identity, and both
topology samples agree. Matching samples detect observed change; they do not
prove atomic sampling or firmware ownership. Logs include both raw snapshots
and either selected group/default/L3 targets or explicit unavailable status.
EU availability, full topology inventory, engine workarounds and MOCS
initialization remain separate unfinished prerequisites. This probe does not
authorize later use after an ownership transition or replace a forcewake lease.

#### ADL-N engine-domain initialization plan

`intel_gpu_adln_engine_settings` encodes the applicable engine-domain settings
from pinned [workaround initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_workarounds.c).
ADL-N is an ADL-P subplatform. The fast-color blitter workaround is excluded:
its [predicate](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt.c)
requires graphics IP12.55..12.71, unlike this platform. There are no compute
engines in the admitted inventory. This plan does not cover GT or context WAs.

Each engine receives masked CMD_CCTL atbase+0xC4, mask0x3FFF. Read/write fields
take MOCS values, not indices: each selected uncached table index is shifted
left1 before field placement. The caller supplies the index only after choosing
and initializing the actual MOCS table; accepting a six-bit value here does not
certify its cache behavior. Render additionally gets:

| Register | Mask | Value | CPU access |
| --- | --- | --- | --- |
| GARBCNTL 0xB004 | 0x80 | 0 | read/modify/write |
| SAMPLER_MODE 0xE18C | 0x8001 | 0x8001 | masked MCR |
| CS_DEBUG_MODE1 0x20EC | 0x2 | 0x2 | masked |
| ROW_CHICKEN2 0xE4F4 | 0x4100 | 0x4100 | masked MCR |
| FF_THREAD_MODE 0x20A0 | 0x80000 | 0x80000 | read/modify/write |
| ROW_CHICKEN4 0xE48C | 0x200 | 0x200 | masked MCR |
| RING_PSMI_CTL 0x2050 | 0x1080 | 0x1080 | masked |
| FF_SLICE_CS_CHICKEN1 0x20E0 | 0x4000 | 0x4000 | masked |

SAMPLER_MODE merges indirect-state-base override and SMALLPL; ROW_CHICKEN2
merges early-read disable and push-constant hold disable. The latter settings
must not overwrite one another. Masked writes encode mask in the upper16 bits;
normal writes preserve unrelated bits of a valid fresh read. This pure planner
performs no reads/writes and does not establish required leases/serialization.
The CPU MCR distinction must not be copied directly into GuC flags: upstream
marks **all engine-workaround entries** explicitly steered in the ADS save list,
even those applied through ordinary CPU MMIO.

`intel_gpu_adln_regset.Build` now merges the common54 entries with the engine
settings' save metadata, producing63 render entries or55 for copy/video/enhance.
Every setting entry carries GuC steering with group0 and the decoded instance;
its masked flag follows the actual register semantics, not whether CPU access
used MCR. Setting values/masks are NOT copied into the ordinary save entries.
The metadata builder uses index0 only to enumerate setting registers; it neither
selects a MOCS table nor computes values for hardware application. Tests compare
against index63 to guard independence of save metadata from the selected index.

The common subset remains available for focused tests, but both builders return
the renamed `Register_Set` type. No obsolete `Common_List` alias is retained.
Failure discards partial output. Sorted uniqueness, preservation of every common
entry and exact workaround flags are tested across315 engine/DSS combinations.
This completes the audited engine-register metadata merge, NOT ADS initialization:
GPU addresses/count descriptors, section serialization, actual initialized
register state, contexts, coherency and publication remain outstanding.

`intel_gpu_ads_register_image.Build` now serializes that register section and
the4096-byte `reg_state_list` prefix of the ADS header. Each descriptor uses
the GuC class and **physical** engine instance, a32-bit little-endian GPU
address,16-bit count and zero reserved field. Registers are concatenated in
CuBit engine-enum order; descriptors carry the resulting offsets so descriptor
table order need not match payload order. Disabled-engine descriptors and unused
payload tail remain zero. Maximum payload is4528 bytes (63+4*55 records).

The supplied GPU address denotes the register-section start, not allocation
base. It must be nonzero/DWORD-aligned; page alignment would incorrectly reject
the packed offset21692. The complete required range and each emitted subrange
must fit below0xFEE00000 without wrapping. Unavailable/invalid inventory or
steering, insufficient MMIO extent, and failed list construction return an
empty invalid result. Tests cover all eight media fuse combinations, payload
bytes, the entire descriptor table, zero tails, exact upper-bound fit and
overflow rejection. Numeric address validation still does not prove GGTT
ownership/mapping/coherency. Remaining ADS header fields, system-info tail,
contexts, capture/private areas and native publication remain unfinished.

#### Complete system-info encoding and doorbell observation

`intel_gpu_ads_system_info` combines the engine prefix with the64-byte generic
tail. An admitted ADL-N topology has one physical slice (not a count of DSSs).
Its surviving physical VDBOX0/2 each have SFC access, so this field matches
their physical mask. This follows `gen11_vdbox_has_sfc`'s Gen12 rule, not Gen11's
even-*logical*-index rule; ADL-N predates the separate SFC-enable fuse field.
See [media fuse handling](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_engine_cs.c).

GEN12_DIST_DBS_POPULATED at0xD08 encodes doorbells-per-SQIDI minus1 in bits23:16,
yielding1..256. The encoder writes that count as a full little-endian DWORD,
not an8-bit count. All remaining generic fields are zero, matching pinned
v6.16 ADS initialization. See the
[GuC register definition](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h).

The native probe reads this register twice after acquiring all inventory-required
forcewake domains; it constructs the system-info bytes only after successful
release. Unequal/all-ones reads or invalid topology/inventory fail admission.
It logs both raw values and `ADS-info-valid`. This is a read-only observation
and local byte construction, not GPU publication or proof the sample remains
usable after later reset/ownership changes. Tests cover2048 inventory/count
combinations, including count256, reserved bits and sample mismatches. The
native driver builds; NUC behavior remains unverified and the current `.img`
has not yet been updated with the doorbell probe.

#### Golden-context reservations

`intel_gpu_adln_golden` reserves one image per enabled engine **class**, not
one per physical engine. Both VCS0 and VCS2 share the video-class reservation.
Pinned [context sizing](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_engine_cs.c)
returns14 pages (57344 bytes) for Gen12 render and2 pages (8192 bytes) for
copy/video/enhancement. ADL-N's skipped prefix is one HWSP page plus80 DWORDs,
4416 bytes, so ADS engine-state sizes are52928 and3776 respectively. Those
constants do not include the separate shared-data page used by GuC submission.

The address must still name the **full** context image: do not add4416 to it.
The plan places images render/copy/video/enhancement, writes address/size by
GuC class index, and leaves disabled classes zero. Total reservation is64..80KiB.
Capacity and page-aligned nonzero GPU ranges below0xFEE00000 are checked, with
empty invalid results for bad inventory, short backing, misalignment or overflow.
Hosted tests cover all eight media inventories and exact/over-limit boundaries;
SPARK checks runtime safety, termination and the successful capacity bound.

This prepares metadata only. It does not map the backing, build logical contexts,
run a workload, capture default engine state or copy that state into the reserved
images. The early/late distinction in
[GuC ADS initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_ads.c)
remains mandatory: firmware engine recovery must stay disabled until valid
golden contents and the actual recovery path exist.

### Capture-list encoding boundary (2026-09-28)

`intel_gpu_capture_list` encodes the ADL-N subset of GuC capture lists into
one zero-padded 4KiB page (up to255 descriptors). It is deliberately separate
from the ADS save-register encoder. The header is a descriptor count, each
16-byte record contains offset/value/flags/mask, value is0xDEADF00D, and mask
is zero for the platform lists we intend to use. Capture steering is encoded
in group bits12..15 and instance bits20..23 without the save-list steering bit.
Unaligned/out-of-MMIO offsets or excessive counts reject with an entirely
zero result. Empty lists encode a valid zero page, NOT permission to pass a
null GPU pointer: the ADS capture table must reference owned mapped backing.

Pinned reference: Linux v6.16
[capture initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_capture.c),
particularly `guc_capture_list_init` and `__fill_ext_reg`.
Platform register selection, capture table assembly and firmware publication
remain pending; this helper does not make the native ADS ready by itself.

### ADL-N capture register selection (2026-09-28)

`intel_gpu_adln_capture` now builds PF global, class and instance pages from
the admitted engine inventory and six-bit DSS topology. Global has9 entries;
each enabled class has the common33 engine-relative instance entries. Render
class has3 fixed INSTDONE registers plus2 per enabled DSS; enhancement has4
SFC_DONE registers. Video/blitter class lists and absent engine classes stay
empty. Multiple video instances share the video-class instance description:
GuC applies each physical engine base, not a base baked into these offsets.

Linux v6.16 `for_each_ss_steering` uses the pre12.55 topology lookup and
`GEN_MAX_SS_PER_HSW_SLICE=8`. ADL-N's DSS0..5 therefore use group0 and their
own instance index; no doubling and no XeHPG geometry-extra register applies.
References: [MCR iteration](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt_mcr.h)
and [SSEU definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_sseu.h).

The hosted matrix covers8 engine fuse combinations by63 nonempty DSS masks.
It does not exercise actual hardware capture. ADS pointer-table assembly,
backed null-list routing, mapping/coherency and native firmware publication
remain separate unfinished work.

### Capture section and pointer assembly (2026-09-28)

`intel_gpu_ads_capture_image` consumes the platform lists and emits a bounded
32KiB capture section plus the packed264-byte capture pointer block for ADS.
The first page is always zero. All66 pointers initially name that backed page;
PF enabled lists are then appended in class/instance order, with PF global
last, matching the upstream construction order. VF and unsupported classes
retain the zero-list address rather than a null address. Unused payload tail
is zero, and `Used` reports the populated prefix (20..32KiB).

Admission requires nonzero4KiB-aligned GPU base, full32KiB backing and a range
ending at or below0xFEE00000. Invalid input returns zero data and pointers.
This is numeric assembly only: no GGTT ownership or publication is inferred.
The full ADS compositor must reserve the32KiB capacity, copy the pointer block
at the ABI's capture_instance field, and retain the mapping for firmware use.

### Final header composition audit (2026-09-28)

`tests/intel-gpu/check-ads-abi.py` independently reads the pinned upstream
packed declarations and checks the complete field layout. It rejects unknown
declaration syntax instead of skipping fields. Verified offsets are: policies
4100, system info4104, control4112, golden addresses4116, state sizes4180,
private4244, capture block4252..4515, workaround address4516/4520 and size4524.
Header size4572 and system-info size640 match the current section planner.
This is an ABI regression check, not a C compiler or hardware validation.

The remaining header composer must keep reserved fields and control_data zero.
For admitted ADL-N and GuC70.49.4, none of v6.16 `guc_waklv_init`'s predicates
apply (12.70..12.74 graphics,13.0 media or DG2). Therefore the KLV address and
size stay zero; this does NOT mean GT/engine/context workarounds are unnecessary.
Early golden pointers may name zero-initialized retained images as upstream
does, but watchdog recovery must remain disabled until actual state capture.
Usage backing is not referenced by control_data: upstream exposes its address
separately through `intel_guc_engine_usage_offset`.
