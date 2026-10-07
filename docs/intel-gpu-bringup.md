# Native Intel graphics: N-series NUC and HD 620 laptop

Status: the NUC has physically demonstrated native GPU submission, Mesa
triangles (256 completed cycles), and the animated teapot gallery. Those
results do not establish a complete driver or an accelerated Desktop: the
opt-in Vulkan Desktop startup remains under investigation. Later physical
tests reach a responsive Desktop and Logs, but explicitly select SOFTWARE
with `Mesa unavailable; CPU compositor fallback`. This is recovery of Desktop
usability, not verification of accelerated composition. Native modesetting and EDID remain roadmap
work; preserve the inherited firmware scanout while bringing up the backend.
The initial 2026-09-27 read-only bootstrap described below is historical,
not the current rendering capability limit. This supplements, rather
than replaces, [the shared display architecture](display-outputs-and-scaling.md).
The [rendering/presentation contract](gpu-rendering-and-presentation.md) records
how Intel fits independent rendering and mixed-adapter machines.

## Target and reference policy

### Pipeline diagnostic candidate, 2026-10-07

Latest physical photo (attachment 5DEF3EBD-AD5C-48E3-BE59-C2B8EAE98308)
again confirms SOFTWARE selection. Three CPU pixel-storage charges total
24,883,200 bytes against a 134,217,728-byte limit; those entries do not show
CPU pixel-budget exhaustion and are not GPU-backing accounting. The photo
does not identify the boot image or show a pipeline failure record. Do not
treat absence from this visible log segment as absence from the entire log.
The next hardware report needs the image name and pipeline stage/index/vk
(or the explicit diagnostic-unavailable line). Candidate checksum reverified;
no image changed.

Startup timing clarification: the frozen driver uses whole-allocation backing
initialization, not the current-tree64KiB initialization quanta. Its10ms idle
wait therefore does not establish a per-chunk delay explanation. A guard-only
private compile failed because that older pool lacks Local_Work_Pending; the
attempted private guard change was restored. Current-tree hosted backing tests
pass with explicit transport-wait versus runnable-local-work assertions, but
they are not timing evidence for this image. Diagnose startup latency using
phase timestamps before attributing it to the new path or claiming a speedup.

Follow-up: the complete current-tree pool body/interface and event-loop guard
were reviewed and integrated together into the private native source snapshot.
Native compile/link passed (61121 exit0, existing warnings); pool sources match
the hosted25376-tested source. This is not native execution or a measured
startup improvement. The diagnostic image hash89dacca6 remains unchanged and
does not include the bounded-initialization port or later quota/slot work.

Ready: `.build-workspaces/graphics-primary-v47-ws22fhfc/kernel/cubit_intel_pipeline_diagnostic_20261007.img`
SHA256 `89dacca69473587cd3409d0544f986d9efb81a4590df3f10f0e6ba625ecb7ccd`.
Only Desktop differs from the system-budget image's extracted payloads; hashes
guarded kernel/initrd/driver/apps unchanged and the original image is preserved.
Desktop SHA256 `f6fc080d3ac0c61ede35dd04294ccf053a95e5a3231acf25db9de272504d6f85`.
No pipeline workaround, driver update or later client-quota changes included.

Validation: 50 hosted diagnostic cases (14 creation failures, 33 missing
functions, success, prerequisite and positive-status cases), native Desktop
fallback/cursor/menu checks, then exact-image UEFI4CPU noPS2 and Console/Logs
Apps launch PASS (29800 exit0). Exact-image logs `/tmp/cubit-usb-live.2vdmq4_b`
and `/tmp/cubit-usb-live.ur6law72`. These do not validate Intel rendering or
exercise the physical failing pipeline. Diagnostic source patch and stage key:
`tests/compositor/pipeline-diagnostic/README.md`.

NUC request: search Logs for `pipeline failure` and capture the complete
`DESKTOP-VULKAN: pipeline failure stage= ... index= ... vk= ...` line, or the
explicit unavailable diagnostic if no C failure record exists. Also capture
startup READY/SOFTWARE. Long startup is not addressed by this diagnostic-only
change; preserve it as a controlled comparison with the previous image.

### Metadata boundary regression, 2026-10-07

Expanded `record_store_tests.adb` passed in Nix (session63222, exit0):
page-by-page growth preserves old records, initializes only complete records,
and leaves partial trailing records and uncommitted tail guards untouched.
An exact 64KiB growth step succeeds; oversized growth, relocation, replay,
zero/misaligned mappings and wrapping address ranges are rejected. This is
hosted CPU-metadata evidence, not a proof of mapping validity or GPU safety.
The retained-store placement-initialization warning was reviewed: typed
default initialization there is deliberate and must not be suppressed by
blindly replacing the initializing declaration with an imported view.
No production source or image changed during this audit.

### Client quota arithmetic proof, 2026-10-07

The live ledger now calls `Intel_GPU_Client_Budget_Policy` for reserve/release
arithmetic. SPARK level 2 with checks-as-errors passed: 10 analysis results,
zero unproved, including both functional postconditions. Rejection leaves
the charge unchanged; accepted reservations cannot wrap or exceed the limit;
accepted releases cannot underflow. No assumptions were added. Report:
`/tmp/cubit-client-policy-proof-20261007/gnatprove/gnatprove.out`.

Hosted ledger and configured ticket-integration tests passed after extraction
(768 stable sessions and 15,360 admission checks in the ledger suite).
Private native Intel driver compilation and linking also passed (session10257,
exit0; existing conversion/overlay warnings remain). No image was repackaged.
These proofs cover arithmetic, not authentication, metadata storage, exact
one-shot retirement receipts, CPU/GPU/TLB completion, or whole-driver safety.
Those remain integration obligations; hardware completion is not inferred
from these hosted tests. The system-budget NUC image below is unchanged and
does not contain the later client-ledger integration or this extraction.

### System backing policy candidate, 2026-10-07

Physical result received: the NUC reports arena quota2069889024 and backing
growth past32MiB to41943040 then44040192 bytes, with metadata32->64 slots.
`TARGETS_AFTER`, `PIPELINE_BEFORE`, `PIPELINE_AFTER` are followed by selection
without upload/readback checkpoints; startup reportsSOFTWARE. The frozen
Desktop gates pipeline entry on successful target setup, and upload entry on
successful pipeline setup. Thus target allocation passed; provided the shown
sequence is complete, pipeline preparation returned false. This is NOT a
successful accelerated Desktop. Mesa search shows only the generic fallback;
the C pipeline helper currently collapses Vulkan failures to1. Next diagnostic
captures first failing substage/index/signedVkResult and emits it after return.
Compositor owns that change; preserve this image as the hardware baseline.
Separate CPU pixelstorage reached24883200 against134217728 bytes, not its cap.

Image coherence warning (IPC-001 handoff): the shared runtime/logstore now use
channel-based logging, including readers. Do not replace individual logging
payloads in this frozen candidate with current-tree binaries. A subsequent
image using the new transport must rebuild and verify the matching runtime,
logstore, Logs, Console, driver and Mesa logging bridge together. Existing
candidate evidence applies to its frozen payload set, not a mixed replacement.

Ready for NUC testing:
`.build-workspaces/graphics-primary-v47-ws22fhfc/kernel/cubit_intel_system_budget_20261007.img`
SHA256 `f0d2a706e460a5bc49e89d5a79654716eeceefbb7e496d45125b4eb0a9970b2c`.
Changes only Intel driver and initrd/devmgr payloads relative to the denial
diagnostic; kernel, Desktop, Console, Logs and other applications are unchanged.
Old image SHA256 was rechecked unchanged. Exact-image QEMU noPS2 boot passed
(`/tmp/cubit-usb-live.crayi8wt`); quiet-xHCI Console/Logs Apps launch and optical
boot checks passed (`/tmp/cubit-usb-live.nvie6sla`, session21363 exit0).
An earlier run passed app launch but failed its debug-xHCI-marker assertion;
the repeat used the existing quiet-xHCI option, not a changed image/test.
Screenshot `/tmp/cubit-usb-live.o6_843br/graphics-tools.png` was inspected:
CPU fallback, Logs and Console normal. These are not Intel rendering tests.

NUC request: capture `intel-gpu: system backing policy accepted=TRUE quota=...`,
then `DESKTOP-VULKAN: startup=READY` or `startup=SOFTWARE`. Check Desktop clicks,
keys, window dragging and Logs launch. If fallback or a freeze remains, capture
the last `DESKTOP-CHECKPOINT` and any `backing denied check=` line. A larger
quota alone is not success: target/pipeline/upload/readback setup and runtime
rendering still require physical verification. Startup latency may remain.

Independent policy proof follow-up: `Native_System_Heap` now specifies and
proves block alignment, quota <= one quarter of valid managed RAM, quota <=
half the retained DMA ceiling, unchanged metadata/DMA policy, and zero quota
for unknown inventory. GNATprove level2/checks-as-errors run55016 passed;
report `/tmp/cubit-heap-policy-proof-20261007/gnatprove/gnatprove.out`.
This proves the selector's arithmetic contract, not kernel inventory accuracy,
physical availability, allocator lifetime, or hardware rendering. The candidate
binary remains unchanged; the follow-up adds contracts, not selection logic.

### Per-client accounting follow-up

Audit of `Intel_GPU_Buffer_Requests` confirms authenticated ownership but no
per-client retained-byte quota. Its pending-byte field covers only the single
in-flight create; it is not a session ledger. A complete ledger must:

- Reserve against authenticated session identity before deferred allocation,
  separately from BO handles, shared physical commitment and CPU metadata.
- Keep charges for uncertain allocation, lost delivery, closed handles and
  quarantined sessions. Neither `Close` nor `Retire_Session` proves reuse safe.
- Refund only after `Acknowledge_Retirement` accepts the exact ticket generation
  and supervisor/GPU/TLB/CPU retirement; stale or duplicate acknowledgements
  cannot refund a newer allocation.
- Charge private context, root/scratch and replacement/incremental page-table
  backing too. The audited byte-less `Reserve_Private` interface would leave
  an unaccounted path if quotas were added only to public BO creation.
- Preserve charges for shared/exported backing until the last reference is
  retired; closing the original name cannot evade the quota.
- Grow metadata with stable identities and bounded updates; do not scan the
  complete allocation registry on every request to reconstruct usage.

These are implementation requirements, not claims of current enforcement.
The new native system-backing budget is aggregate admission policy only.

Ticket-charge preparation now stores conservative requested bytes alongside
each exact generation. Public Create records the requested size before deferred
work. Private and context reservation APIs require a page count (no compatibility
default); native callers supply context/table/scratch sizes. Unknown or stale
tickets report zero. Pending/failure/close/quarantine preserve charges; successful
public, private, closed-table and context retirement acknowledgements clear them.
This is an O(1) ticket observation, not a sum of all client usage or enforcement.
Physical commitment can differ from reserved/retained charge. Session aggregates,
limit admission and exact rollback rules remain to be integrated.

Nix hosted4852/29879 pass request, context, private-table, role-resolution and
VM-update lifecycle suites, including charge assertions across128 public/context
and256 private-table generations. Role fixture size corrected to match actual
8192-byte modeled backing (70533). Native compilation deferred because shared
build lock was unavailable; not shipped in the f0d2a706 candidate. These are
hosted state-machine tests, not proof or hardware retirement evidence.

Follow-up native gate80042 passed full Intel compile/link in the existing
isolated workspace after review of the exact ticket-charge diffs. No shared
build output or ISO changed; candidate f0d2a706 was rehashed unchanged. This
checks integration of the required page-count arguments, not live retirement.

The initial client-ledger core (`Intel_GPU_Client_Budgets`) was implemented
and tested independently of production allocation. It stores immutable session
identities in retained `Record_Store` metadata and bounds digital-search lookup
to65 record reads, independent of account count. Open never reopens closed
identities; reserve uses checked remaining capacity; close retains the charge;
confirmed release can drain closed accounts; quarantine denies allocation,
release and metadata growth. CPU metadata and physical backing remain separate.
The trusted release entry point does not authenticate allocation tickets itself:
the dispatcher must call it exactly once after a valid ticket transition.
Consequently this component alone is not production per-client enforcement.

Nix hosted74664 PASS:768 sessions across metadata growth,15360 admission checks,
closed/reopened identities, drain and underflow rejection, quarantine and near
U64-limit arithmetic. Sources `tests/intel-gpu/client_budgets_tests.adb`;
isolated project `/tmp/cubit_client_budgets.gpr`. No proof/native/HW claim for
this core yet. Next integration must atomically reserve account+ticket and
release account+exact retirement, including all private allocation roles and
uncertain failures, without charging GPU backing as CPU metadata or vice versa.

Client-quota integration now connects that ledger to the request service.
Trusted startup configures it before any ticket. Public creation, private tables
and context parents reserve bytes before ticket mutation; budget rejection
issues no allocation ticket. Successful exact public/private/closed-table/context
retirement refunds once; stale, wrong-owner, uncertain or duplicate transitions
cannot refund. Failed allocation and delivery retain their reservation. Session
closure retains charge and closes the budget identity, including a session
closed before its first allocation. Accounting inconsistency quarantines the
service rather than making backing reusable. Device-lifetime session0 tickets
remain subject to the shared backing quota, not a fictional app account.

Native startup explicitly enables a per-session ceiling of half the selected
shared backing budget, rounded down to4KiB. This is a tunable admission policy,
not reserved RAM, a fairness scheduler or GPU residency management. Standalone
generic embeddings remain unconfigured until their trusted owner opts in.
Account metadata has a retained-growth API; hosted integration exercises64
session identities across growth. The current native render-session registry
still admits only16 lifetime identities, so lifting that separate limit and
wiring account growth into its admission path remain future work.

Session-growth integration audit: this limit is coupled to more than the quota
ledger. `Intel_GPU_Render_Control` keeps a fixed recipient-incarnation array
and returns capability slot `39 + Storage_Index` (currently40..55).
`Intel_GPU_Application_State.Items` holds16 limited context records, and native
preparation/ledger/image-retirement cursors use the same bound. Therefore
extending only `Render_Sessions` or changing its constant is unsafe: it would
also change capability destinations without establishing ownership of them.
The next session-scaling change must separate stable issued identity/index
from allocated recipient capability slot, grow context and recipient metadata
before session admission, and extend client-account storage in the same
admission sequence. Failed admission must not expose a usable tag. Reusing a
capability destination additionally requires confirmed endpoint/grant cleanup;
session Close alone is not that receipt. Preserve the existing SPARK
authentication contracts when separating policy from storage. This is a
source audit, not implemented session growth or a native capability test.

Recipient-slot separation follow-up: the controller now consumes its bootstrap
capability pool with a separate monotonic cursor and records the assigned slot
beside the recipient incarnation. Reserve replies and all three native
recipient checks use `Stored_Recipient_Slot`, not `39 + Storage_Index`.
This lookup is metadata, not authority; active-session or broker authentication
still precedes capability use. Close, failed delivery and quarantine retain the
record and never recycle the slot. Pool40..55 and the16-session limit remain;
this is preparation for coordinated growth, not a larger fixed limit or a
claim that live session growth now works.

Hosted render-control tests passed (29256 exit0), including slot observation
before issue, after failed delivery, and after quarantine. SPARK level2 with
checks-as-errors passed for Render_Control and Render_Sessions (56300 exit0,
69 analysis results, zero unproved). Proof covers the declared contracts and
runtime checks, not kernel endpoint ownership or GPU retirement.
Private native driver compile/link passed (82788 exit0; existing warnings).
The hardware candidate image was not repackaged.

Platform gate for session growth: `CuBit.Messages.CapabilitySlot` is0..63 and
kernel `PER_PROCESS_CAPABILITIES` is64. The launcher-to-broker protocol also
restricts source slots to40..55 and app destinations to0..63. There is no
unbounded destination range for the driver to select. Before lifting the
session limit, coordinate capability reservation/growth across kernel, runtime,
launcher and broker, or establish an exact revocation/retirement receipt for
safe endpoint-slot reuse. Neither closing a session nor observing a free
numeric slot proves all old grants and users are retired. Ownership/interface
request is recorded in `coordination/filesystem.md`; this audit changed no
shared capability ABI and did not introduce a larger fixed limit.

Hosted47498 first integrated test passed; expanded91511 passed the configured
growth/retirement fixture and all five existing request/context/table/VM suites.
Native private26138 compile+link passed. Current hardware candidate f0d2a706
is unchanged and does not contain this quota integration. No native execution,
whole-ledger SPARK proof or Intel hardware quota test is claimed yet.

### Startup diagnostic image, 2026-10-06

Physical result received 2026-10-07: `backing denied check=quota bytes=8388608
slot=31`, after committed snapshots reached 33554432 bytes. This establishes
the 32MiB policy ceiling as the immediate allocation blocker, not failure to
obtain a physical block. It does not establish that no other startup problems
remain. No further NUC diagnostic is required to identify this rejection.

The failing image's supervisor and driver use `Default_Heap`, whose byte quota is
derived from the legacy sixteen-block `Physical_Extents.Capacity`. Neither
calls the existing `Configure_Heap` API. Physical backing and extent metadata
already grow lazily; the remaining integration is trusted budget selection and
matching driver admission geometry, separately from per-client BO quotas.
Do not treat a physical-memory snapshot as a reservation or raise the fixed
default to disguise this missing policy integration. Device-local VRAM and
DMA address limits must remain independent from the system-backed budget.

Hosted regression (Nix session96103, PASS): aggregate16MiB setup plus three8MiB
targets reproduces quota rejection on the fourth allocation. A test-only64MiB
policy admits the same workload by growing to exactly40MiB, one2MiB physical
callback per step, with lazy extent metadata and stable prior views. Aggregate
setup models the observed footprint, not the exact BO composition. This is
mock-backed allocator evidence, not a production policy fix, SPARK proof or
Intel hardware rendering test. The image below remains unchanged.

Follow-up implementation: both production endpoints now call `Configure_Heap`
before first backing use. Shared `Native_System_Heap` reads existing kernel
MEM_TOTAL (query1601, immutable buddy-managed RAM), selects one quarter of that
inventory capped at half the current4GiB DMA aperture, and rounds down to2MiB.
Unknown inventory or a zero result fails closed. This policy is not free-memory
accounting, a reservation, device-local VRAM, or a guarantee that physical
allocation succeeds. It preserves lazy physical/metadata growth and retirement.
The current32bit DMA cap deliberately limits the live path even on terabyte
machines; expanding DMA addressing/IOMMU support and per-client fairness are
separate remaining work. Do not infer total Desktop demand from40MiB: upload,
readback, pipeline state and source textures need additional backing.

Nix hosted48325 PASS16385 inventory sizes plus existing extent regressions;
buffer-memory47410 PASS malformed replies, growth, cancellation and retirement.
Native compile12099 and private matched link80639 PASS. No SPARK proof or NUC
success is claimed for the new policy. Separately named candidate packaging
and exact-image boot checks follow; old images remain unchanged.

Latest diagnostic candidate:
`.build-workspaces/graphics-primary-v47-ws22fhfc/kernel/cubit_intel_backing_denial_20261006.img`
SHA256 `baff4e69506953331dcd06fa0c77ee5041c74f7c48a497522637e39a09e532de`.
Matched devmgr/Intel denial diagnostics now report
`intel-gpu: backing denied check=... bytes=... slot=...` through the existing
Intel logger. The diagnostic reply is checked against version, request identity,
size and reason range; it grants no backing or authority. Hosted malformed-reply
and allocator regressions pass, as do native compiles and private links.
Desktop retains the indicator and startup checkpoints but routes periodic stats
only to serial, not logstore. Other apps and the kernel are unchanged.
Exact-image QEMU USB/UEFI four-CPU no-PS/2 and Console/Logs launch tests pass
(`/tmp/cubit-usb-live.b8ukmod1`, `/tmp/cubit-usb-live.8g9ck2_t`); the latter
screenshot was inspected. No physical Intel fix or rendering validation is
claimed by those QEMU tests. The requested NUC denial line has now been
received and decoded above.

Physical follow-up: software-only comparison restores responsiveness on the
NUC (user reports everything works again after the input/app-launch checklist).
This isolates the regression to the Vulkan Desktop startup/path or its downstream
interactions; it does not prove synchronous submission is the sole cause.
The backend-indicator comparison is ready but not yet physically tested.
Both live in `.build-workspaces/graphics-primary-v47-ws22fhfc/kernel/`:

- `cubit_intel_software_ab2_20261006.img`, SHA256
  `2ab1bf3bebe5d4fb8d88d491861580d43c22a68843e0237f5e5b282167a08989`:
  Desktop bypasses GPU initialization and selects software from startup. Other
  apps and Intel driver remain present. Test this first for restored click/key
  responsiveness and Apps launches.
- `cubit_intel_backend_indicator_20261006.img`, SHA256
  `69aae60b54f9197c5c3d8dcd00086613e45fec18ef82854c6521f1507566c79f`:
  normal backend selection plus an on-screen selected CPU/GPU label and
  loop/frame/key/mouse/button/request counters. The label is selection evidence,
  not proof of GPU completion. Overlay damage piggybacks existing frames;
  it does not add a redraw timer or per-event logs.

Indicator interpretation: the HUD updates only when a frame completes, so a
frozen HUD does not prove input never arrived. Frame counts are rendered frames,
not scanout/photons; key/button counts are event transitions, and requests include
background clients. On NUC, compare two photos around moving the cursor,
pressing/releasing a key, clicking/releasing Apps, then moving again and waiting
about ten seconds. GPU selection with regression directs stage timing toward
capture/submission/fence/readback. CPU selection with regression instead keeps
initialization/cleanup side effects in scope. If this diagnostic is healthy,
repeat the original diagnostic image before declaring a fix: instrumentation
may change timing. Do not replace images or introduce another variant until
this distinction is observed.

For each image, extracted `/boot` files match the diagnostic baseline and only
Desktop differs under `/apps`. Both pass exact-image QEMU USB/UEFI four-CPU
no-PS/2 and Console/Logs launch checks. Software evidence: `bmt01a4s`, `ud72egbj`;
indicator evidence: `zpoxzz7a`, `9jawlaju` under `/tmp/cubit-usb-live.*`.
The indicator screenshot was visually inspected. These checks use software
fallback; they do not validate physical Intel compositor performance.

Source audit found a concrete blocking boundary: linked Mesa's
`anv_cubit_submit_bo` holds its lifetime mutex across `cubit_intel_submit_batch`;
the Ada bridge uses synchronous `capCall`, and the Intel driver replies only
after application submission completion/disable. A later fence-status query
does not make that preceding call asynchronous. Desktop drains input and
requests before rendering, but its admission budgets cannot preempt an entered
FFI call. This establishes a latency risk, not the measured cause of the NUC
symptom. Do not fix it by timing out and reusing GPU-visible backing, or by
recursively dispatching Desktop work while Vulkan locks are held.

Native synthetic comparison completed: during a five-second cooperative Pending
hold, Desktop dispatched 7 input events and completed 49 request roundtrips.
The same duration spent inside a synchronous backend call allowed neither;
both resumed after return. Evidence: `/tmp/cubit-dispatch-comparison-result.json`
and `/tmp/cubit-dispatch-{pending,blocking}-native-1`. This reproduces the
blocking mechanism, not actual Intel latency or the physical symptom's cause.
The next decision needs the software-only NUC comparison; no speculative
production timeout, renderer switch, or backing reuse was introduced.

Separate private artifact:
`.build-workspaces/graphics-primary-v47-ws22fhfc/kernel/cubit_intel_startup_diagnostics_20261006.img`
SHA256 `e5b026720eafd12f4eeaad09d222a9012b2392c58c7cfc09ab97d0c800aa45d7`.
The extracted Desktop payload matches the reviewed diagnostic binary
`a9a9965e5f548e793c5dae0bf182ecb94a67675a28e4c89f497445efff860ab4`.
The previous `cubit_intel_desktop_vulkan_20261005.img` was preserved unchanged.

Exact-image QEMU USB/UEFI four-CPU tests passed without PS/2 and on a separate
boot launching Console and Logs from Apps. Evidence:
`/tmp/cubit-usb-live.c9tmhx4a` and `/tmp/cubit-usb-live.rm_g1_m4`.
The latter screenshot was visually inspected. Both serial logs show bootstrap
release before renderer initialization and SOFTWARE fallback. This validates
native CuBit integration, not Intel GPU rendering or the physical black-screen fix.

NUC request: photograph whether the blue "CuBit Desktop reached / Preparing
renderer startup" screen appears, remains, or changes to black. If Desktop
appears, capture `DESKTOP-CHECKPOINT` and `DESKTOP-VULKAN` startup records.
The static screen establishes only that Desktop reached its bootstrap phase;
missing log records do not prove the exact stall location. This is not yet a
general rescue console or a native modesetting implementation.

Target the actual PCI GPU identity/revision, not the CPU marketing name. The
initial recognition table includes Intel Kaby Lake ULT GT2 (5916/5921) and
Alder Lake-N (46D0 through 46D4). Recognition is not a support declaration.
Record the NUC's exact ID/revision and the laptop's ID before register access.
The laptop's i7-7500/HD 620 should not be treated as an identical generation
to the NUC; generation-specific register/power/link rules stay explicit.

Reference Intel's programming manuals for register contracts and Linux i915
and the shared Intel display code for sequencing and platform workarounds.
Linux i915 is not a standalone library we can simply link into CuBit. Do not
import Linux scheduling, DRM file descriptors, or global root privileges into
our public protocol. Review licensing before copying implementation code;
record source revision and license for any imported material. No Linux driver
implementation has been copied in this slice.

References:

- [Intel programming manuals index](https://www.intel.com/content/www/us/en/docs/graphics-for-linux/developer-reference/1-0/overview.html)
- [Linux Intel PCI identities](https://github.com/torvalds/linux/blob/master/include/drm/intel/pciids.h)
- [Linux i915 internals](https://docs.kernel.org/gpu/i915.html)
- [Shared Intel display driver](https://docs.kernel.org/gpu/intel-display/index.html)
- [Mesa ANV](https://docs.mesa3d.org/drivers/anv.html)

## First native slice: inventory without taking over scanout

1. Extend the existing authorized PCI discovery path to report BDF, vendor,
   device, revision, command bits and BAR descriptors. Do not assume 00:02.0.
   Do not probe a live BAR's size by writing all ones while scanout is active.
   Reuse validated resource information where available; otherwise establish
   a coordinated resource-discovery phase before attempting a mapping.
2. Pass a device-scoped, lifetime-bound mapping to a userspace Intel service.
   Validate memory BAR kind/width, nonzero base, length, overflow, and ownership.
   The new `Intel_GPU_Probe` helper only checks arithmetic within that mapping;
   it neither validates PCI BAR encoding nor grants access to physical memory.
3. Introduce explicit per-platform register descriptions. Start with a small,
   documented set of safe observation registers. An MMIO read is not inherently
   harmless: avoid clear-on-read registers and unpowered domains. Do not read
   arbitrary offsets for diagnostics. Handle required power/forcewake protocols
   explicitly before any dependent access, with a bounded failure result.
4. Snapshot active pipe/transcoder/plane geometry and connector state. Report
   raw evidence plus decoded values through authorized diagnostics. Preserve
   the firmware scanout and do not register a ready native display output yet.
5. Validate the snapshot on the NUC; use the laptop as a separate platform
   fixture, not evidence that Alder Lake-N register sequencing is correct.

The initial private PCI inventory stage used the probe package inside devmgr
to print whole-line Intel PCI identity,
revision, command/header values and decoded BAR addresses. It classifies display
devices for the existing Devices inventory without declaring a native driver
active. It reads configuration space only; no GPU MMIO, BAR sizing writes,
command-bit writes or new capabilities. BAR size is reported as unknown.
The later private bootstrap below adds bounded read-only mapping without
register access. Shared devmgr backport awaits the networking edit-window
acknowledgment.

## Milestones with visible acceptance criteria

### Current hardware regression: compositor startup visibility (2026-10-06)

The user reported a black screen after the boot panel showed
`procmgr: init launch 4/7: display.svc`; the latest diagnostic was an Intel
backing-budget snapshot and the first-failure field was empty. The monitor
apparently retains its signal. The exact tested artifact is not yet confirmed.
This does not establish loss of the display link, a missing modeset, or a GPU
allocation failure.

The retained `graphics-primary-v47-ws22fhfc` workspace's display startup calls
`setupBackend`, then clears the output to `0x00131518` before registering it.
For its firmware backend, `setupBackend` maps the framebuffer through MAPFB,
which retires boot-panel drawing. This is a plausible source of the nearly
black transition without loss of signal, not proof that the tested image used
this exact source or that Desktop subsequently started. Preserve that distinction
when interpreting the last visible boot-panel line.

The frozen compositor candidate calls synchronous Vulkan initialization after
activating its internal shell but before the first frame/event loop. Its normal
log forwarding also depends on reaching that loop. Independent one-shot startup
checkpoints are being tested privately; they are not yet a delivered fix.

Before requesting another physical run, provide an observable diagnostic path:
either a minimal CPU status frame presented through the authenticated display
service before GPU startup, or an independent diagnostic consumer the user can
actually access. Merely collecting logs behind a nonfunctional desktop is not
sufficient. A CPU status frame must complete its normal ownership/fence handoff
before GPU rendering takes over; do not restore the retired boot framebuffer.
Missing best-effort log checkpoints are not proof of a stall because collector
budgets can discard them. The diagnostic test must cover missing/slow logging,
immutable pending publisher pages, unique completion routing and no extra wait
on the logger. Hardware rendering and physical scanout remain separate gates.

### Feature milestones

1. **Inventory:** safe PCI/register snapshot; unsupported hardware continues
   through the existing firmware backend. No reset, bus-master enable, GTT
   mutation, or display ownership claim merely because the device was recognized.
2. **Owned scanout:** explicit handoff from firmware-backed display to Intel
   output, correctly pinned/mapped buffers, completion fencing, and a test
   pattern without corrupting firmware or other clients' memory. Keep the old
   owner until the transition is admitted; after takeover, a fault is explicit,
   not a silent return to a potentially invalid firmware address.
3. **Native outputs:** EDID/link-admitted modes, hotplug, dual HDMI, and bounded
   transactional Settings Apply/Revert. Reuse the existing display registry,
   geometry, placement and primary-display policy. Desktop chooses primary.
   Track implementation and hardware gates in the
   [modesetting and monitor-discovery checklist](development-backlog.md#native-intel-modesetting-and-monitor-discovery).
4. **Trusted rendering bring-up:** GPU VM, engine/context lifecycle, fences,
   firmware requirements and watchdog/reset behavior; first offscreen clear
   and triangle, then presentation. No arbitrary application workloads yet.
5. **Isolated rendering + Mesa:** audit the selected ANV version's complete OS
   dependency surface, implement the required CuBit submission/memory/sync
   adapter and loader/WSI integration. Exercise cross-context negative tests.
   Evaluate Zink for OpenGL over Vulkan; it is a candidate, not a proven port.
6. **Teapot and consumers:** hardware-rendered OpenGL teapot, Servo WebGPU,
   and desktop compositing over the same GPU resource/fence foundation.
   Clearly label software-rendered intermediate demos. Widget acceleration
   follows compositing; existing apps should not need a new toolkit.

## Authority, memory and latency boundaries

### ADL-N firmware selection and staged layout validation

Linux v6.12's `intel_uc_fw.c` explicitly redirects `IS_ALDERLAKE_P_N` to
the ADL-S firmware selection: ADL-N lacks HWConfig, and the ADL-P binary may
attempt to fetch a nonexistent table. The selected major-70 family is therefore
`i915/tgl_guc_70.bin`, not `adlp_guc_70.bin`. This is a selection rule, not a
pinned binary/version/redistribution decision. Reference:
[firmware selection](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.c).
Do not confuse this firmware-family override with reclassifying all ADL-N
hardware as ADL-S. In particular, GuC submission policy must be established
separately from the filename:
[microcontroller defaults](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/intel_uc.c).

`Intel_GPU_Firmware.Decode` implements only the CSS component-layout boundary.
The fixed 128-byte header is decoded little-endian; DWORD counts are widened
before arithmetic. It checks the CSS/header/key/modulus/exponent relation,
nonempty code/signature and containment of both mandatory payloads in the
provided blob length. Optional modulus/exponent bytes may be absent, following
the documented truncated-image format. Reference layout:
[MIT-licensed CSS ABI description](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/intel_uc_fw_abi.h).
No Linux implementation was copied.

Hosted boundary/hostile-count tests pass. SPARK proves returned code/signature
containment and zeroed rejected layouts, plus index safety and termination.
This does not authenticate firmware, admit its version/platform, bound it to
WOPCM capacity or implement DMA upload. No firmware is bundled or loaded yet.
Those are separate required gates, including pinned provenance and license
notices; successful structural parsing must never be labelled trusted firmware.

See the [Mesa 26.2.3 OS-boundary audit](mesa-anv-port-audit.md) for the inspected
backend seams and dependencies beyond the callback table. No native Mesa port
is claimed by that source inventory.

Applications receive render contexts and owned/imported buffer handles, not
MMIO or unrestricted DMA authority. The desktop alone receives the relevant
display-presentation authority; render access does not imply capture, access to
another window, or modesetting. Imported images must be explicitly shared.

An in-process Mesa compiler is untrusted relative to the GPU service. Isolation
must survive malformed command buffers, stale handles, GPU addresses targeting
other contexts, reset races, and nonterminating shaders. GPU page tables,
command privilege restrictions and system DMA confinement are distinct pieces.
Do not call this secure merely because an IOMMU is enabled or the host service
proves with SPARK. Unknown firmware and reset behavior are release blockers for
untrusted accelerated workloads, not reasons to grant broader authority.

Retain backing pages until every CPU/GPU/scanout reader has released them or
hardware is demonstrably quiescent. Asynchronous timeout is not cancellation.
Batch submissions; avoid CPU readback and unnecessary intermediate copies.
Bound queue depth and measure input-to-present latency under rendering load,
not just frame rate. Preserve the software path for unsupported adapters.

## Test scope and next evidence

`tests/intel-gpu` covers exact identity recognition and aligned 32-bit register
address containment. It does not touch hardware. SPARK covers the helper's
arithmetic contracts only, not MMIO semantics, DMA isolation, firmware behavior,
or the driver we have yet to write.

QEMU virtio tests remain valuable for shared display/buffer protocols but do
not validate Intel register programming. Physical NUC snapshots and bounded
bring-up stages are required. Do not describe a QEMU virtio pass as an i915 pass.

Before native mapping, coordinate devmgr edits with networking and review the
existing BAR/capability handoff. The USB-flash fix has physical boot confirmation
but remains in the private v15 workspace pending its separate shared backport.
USB keyboard and mouse behind the keyboard's integrated hub now work on the
physical NUC; see the v19 baseline below. Runtime hotplug remains outstanding.

## 2026-09-27: hardware baseline and resource handoff

The NUC now has physically confirmed normal keyboard/mouse operation through
the VIA USB2 hub on v19, including complete startup diagnostics without reported
losses. USB runtime hotplug remains separate work.

`Intel_GPU_Resources.Plan_Registers` is the next pure admission boundary. It
requires a recognized platform, enabled PCI memory decoding, a non-prefetchable
memory BAR, and a trusted resource extent. It admits only whole-page contained
requests and returns zero address/length on rejection. It does not derive BAR
size, enable decoding, mint authority, map memory, or access registers.

The adapter must bind that extent to the same live device/BAR and supply a
read-only uncached mapping; it must still validate register power/access rules.
An arbitrary caller-provided size or the Linux photograph is not a trusted
resource assignment for the current CuBit boot. Therefore no new native MMIO
access is enabled by this helper alone. Firmware scanout remains untouched.

### Fixed layout evidence and mapping API gap

Intel datasheet 767626, GTTMMADR at PCI offsets 10h/14h, specifies a 16 MiB
BAR aligned to 16 MiB, with a 2 MiB register region, 6 MiB reserved, and an
8 MiB GGTT window. The documented base uses address bits 38:24; the low BAR
flags indicate 64-bit non-prefetchable memory. [Register definition](https://edc.intel.com/content/www/us/en/design/publications/12th-generation-core-processor-datasheet-volume-2-of-2/graphics-translation-table-memory-mapped-range-address-gttmmadr0-0-2-0-pci-offse/).

`Plan_ADLN_Registers` validates this encoding for the existing Alder Lake-N
identity whitelist and admits only the first 2 MiB. It rejects Kaby Lake until
that platform's mapping is independently validated. All 4,096 pages of a
16 MiB BAR are tested: reserved/GGTT pages cannot enter the register mapping.
SPARK proves the returned region stays inside that register extent. This is
not proof of the hardware specification or of firmware resource assignment.
Private native devmgr now compiles this plan into its PCI-only diagnostic
inventory. It still does not grant/map/touch the GPU or change PCI registers.

Audit finding: `SYSCALL_MAP_DEVICE` uses writable `PG_USERIO` unconditionally;
`checkDeviceMemAccess` requires both read and write rights and compares summed
modular endpoints. Before claiming an enforced read-only GPU inspection path,
add requested-access-aware admission, nonoverflow range checks, and read-only
page flags. A coordination request is recorded; shared kernel files are not
modified in this slice. A read-only plan in userspace is not sufficient.

Private follow-up: the isolated kernel now accepts MAP_DEVICE arg3=1 for
read-only access and clears PG_WRITABLE; mode 0 retains read/write behavior.
Capability admission uses the proved `Device_Memory_Admission` nonwrapping
containment predicate and requested rights. Unknown modes and unaligned or
wrapping mapping requests are rejected. Existing shared kernel files remain
unchanged pending coordination. Native RAM-backed read-success/write-fault
fixture 4n7sahgp passed separately from ordinary boot regressions.

## Native userspace service bootstrap

The new `intel-gpu.drv` currently authenticates devmgr, validates a four-word
versioned bootstrap, maps the admitted register region read-only, and remains
idle. It does not access GPU registers, register a display backend, enable
bus mastering, or expose submission to applications. Private devmgr remembers
recognized ADLN PCI devices and grants only that register region; unsupported
hardware keeps its existing display backend.

Bootstrap uses `capSubmit` with no completion token, not SEND_EVENT: the event
lane deliberately reports sender zero and cannot satisfy the service's sender
authentication. The first event-based fixture was rejected, as required. The
asynchronous request lane preserves kernel-authenticated sender identity and
does not make devmgr synchronously wait on an experimental GPU service.

The private QEMU fixture uses aligned allocated RAM and a synthetic descriptor,
explicitly logged as NOT hardware, to exercise the real service binary and
mapping path. It establishes neither Intel register behavior nor acceleration.
Register allowlisting and power-domain handling remain prerequisites for the
first physical snapshot. Routine startup diagnostics currently go to serial;
the physical inspection result must be wired into logstore before a useful
NUC diagnostic release.

## Initial power-control observation (staged, not live MMIO)

`Intel_GPU_Observation` limits capture to two 32-bit reads: firmware power
control at 0x45400 and driver power control at 0x45404. These offsets are the
HSW_PWR_WELL_CTL1/2 definitions in Intel's
[Linux display register header](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/display/intel_display_regs.h.html).
The MIT-licensed reference implementation at Linux revision
`54ac9ff8f1196afc49d644a1625e0af1c9fcf7f5` reads both when inspecting power
requesters, and polls the driver control for power transitions:
[power-well implementation](https://linux.googlesource.com/linux/kernel/git/torvalds/linux/+/54ac9ff8f1196afc49d644a1625e0af1c9fcf7f5/drivers/gpu/drm/i915/display/intel_display_power_well.c).
No implementation code was copied. This is reference evidence for the first
observation set, not a proof of all platform-specific hardware preconditions.

The staged capture requires ADLN, confirmed PCI D0 and checked mapping bounds.
It performs no reads on rejection; otherwise it preserves both raw values,
including all ones or zero, without claiming those values are usable hardware
state. Reading request/state bits does not acquire a power reference and cannot
authorize dependent pipe/plane reads. The two observations are not atomic.

The native main remains unchanged: its bootstrap does not yet carry trustworthy
PCI D0 evidence. Before connecting Capture to volatile MMIO, extend trusted PCI
discovery with bounded capability-list/PMCSR validation, check the current power
state (do not wake the device implicitly), and retain that state/mapping lifetime
through observation. If power state cannot be established, report unavailable.
Also publish the results through logstore for physical testing. No new hardware
image is implied by the hosted snapshot tests.

The pure `Intel_GPU_PCI_Power.Decode` now handles a 256-byte type-0 config
snapshot, traversing at most 48 aligned capability headers. It rejects loops,
duplicate/truncated PM entries, unsupported PM versions and PM/header overlap.
It decodes PMCSR state bits without writing them. Missing capability/device
evidence remains unavailable. Register constants follow
[Linux PCI definitions](https://github.com/torvalds/linux/blob/master/include/uapi/linux/pci_regs.h),
not a copied implementation. Hosted adversarial tests and SPARK bounds/termination
checks pass; native collection and authenticated bootstrap integration remain
outstanding. A snapshot is not a power reference or protection against another
device owner changing D-state after discovery.

Private native follow-up: devmgr now collects the type-0 PCI config snapshot,
prints the decoded power state and launches Intel inspection only for admitted
resources in D0. Bootstrap version 2 requires bit 16 of its command/evidence
word to indicate trusted D0 discovery; missing evidence, upper reserved bits
and the old undeployed version are rejected. Both native binaries build.
Shared bootstrap tests and proof pass; this new handoff has not yet been boot-
tested. The driver still performs no MMIO reads. No new image was published.

Native snapshot follow-up: the driver now instantiates Capture with a volatile
full-width read adapter after successful v2 bootstrap and read-only mapping.
Only the two named power-control registers are read, once each; no writes or
dependent pipe/plane access. Explicit text labels avoid native enum Image output
being numeric. RAM-backed native fixture `9l3mvhjp` passes the two zero-value
snapshot markers and subsequent desktop startup; the USB hub input regression
also passes. This validates generated native reads and handoff, not physical
Intel power/MMIO semantics. The service still reports through serial only;
authorized logstore output is next before a useful NUC diagnostic release.
The private generic image contains the test fixture until rebuilt; do not burn
it as the normal hardware image. Source catalog/audit are restored to normal.

Logstore follow-up: native RAM fixture `yc20kdyi` confirms snapshot delivery
through logstore to the startup viewer (not merely a direct serial print).
Private devmgr recognizes a canonical 0x0229 request only from its tracked Intel
inspection PID with the minted 0x4947 authority tag. It issues a distinct
publisher tag, bootstrap diagnostic budget 15 / issuance 2, and no observer
rights. USB's existing grant path remains unchanged. The Intel service submits
one combined record asynchronously, dispatches completions and bounds startup
logging wait to 30 seconds. Its Publisher storage remains process-lived on
timeout, preserving uncertain acquisitions. Missing logs never authorize GPU
operations. The normal image catalog is restored; the built fixture image is
still test-only until rebuilt. No physical GPU evidence is claimed.

### ADLN firmware protected-memory admission (2026-09-27)

`Intel_GPU_Firmware.Fits_ADLN_WOPCM` now checks a proposed region against an
explicit trusted capacity (at most 8 MiB), 16 KiB base alignment, 4 KiB size
alignment, 36 KiB top hardware-context reservation, 24 KiB GuC internal
reservation, and 16 KiB separation above optional HuC firmware. Upload sizes
are CSS header plus code, not signature bytes. The pinned GuC requires 335104
upload bytes; a 360448-byte GuC region accommodates it, while 356352 does not.

References: Linux v6.12
[WOPCM layout](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/intel_wopcm.c),
[register masks](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/intel_guc_reg.h),
and [upload size](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/uc/intel_uc_fw.h).
The 8 MiB limit is a validation ceiling, NOT a claim of available ADLN capacity.
Unlike upstream's deprivileged/prelocked fallback, this helper does not enlarge
an unknown capacity to make a firmware-programmed layout pass.

Nix-hosted tests pass for exact-fit/oversize, misalignment, optional HuC
overlap, maximal integer inputs and every byte of the next 4 KiB beyond the
capacity boundary. Focused level-2 SPARK proof establishes the accepted-region
containment/reservation postcondition and runtime checks. Hardware constants,
capacity provenance, stable register state and correct platform selection are
assumptions, not proved hardware properties. This is not upload integration:
reset/forcewake, GGTT firmware mapping, DMA and authentication remain pending.
No new image or physical GPU execution is represented by these hosted tests.

### GT forcewake handshake (2026-09-27)

The new generic `Intel_GPU_Forcewake` adapter implements the normal ADLN GT
handshake: wait for acknowledgement bit 0 clear, write masked request
`0x00010001` to `0xA188`, then wait for bit 0 set in `0x130044`. Release writes
`0x00010000` and waits for clear. Every phase has a caller-provided positive
sample limit. Both acquisition phases share an elapsed-time budget (50 ms by
default), checked before and after MMIO reads. The clock callback must be
monotonic; regression or wrap yields Invalid_Clock. Elapsed subtraction avoids
overflow from forming an absolute deadline. A stalled clock still reaches the
poll limit. All-ones reads fail explicitly, not as apparent acknowledgement.
Failure after requesting wake clears that request, but does not claim successful
cleanup: the caller must quarantine/reset the domain. A stale initial ack fails
without writing. Other request bits are not modified.

This follows the normal path in Linux v6.12
[intel_uncore.c](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/intel_uncore.c)
and register definitions in
[intel_gt_regs.h](https://github.com/torvalds/linux/blob/v6.12/drivers/gpu/drm/i915/gt/intel_gt_regs.h).
The Gen12 table assigns GuC's 0xC000..0xCFFF register range to GT forcewake.
Upstream also has fallback-bit retries for missed acknowledgement; those are
not implemented yet. Timeout is an error, never permission to continue.

Hosted scripted MMIO tests cover immediate/delayed ack, stuck clear/set,
all-ones before/after request, unrelated ack bits and release failure. Additional
tests pass for exact deadline expiry, a budget shared across phases, slow MMIO
returning ack after expiry, clock regression/wrap, frozen clock and zero budget.
Release always attempts to clear its request, even with zero wait budget. Run:

```sh
nix develop -c bash -c 'mkdir -p tests/intel-gpu/forcewake-build && cd kernel && alr exec -- bash -c "cd ../tests/intel-gpu && gnatmake -gnatec=host.adc -gnat2022 -gnata -gnatwa -gnatwe -I../../userspace/services/intel-gpu -D forcewake-build -o forcewake-build/test forcewake_tests.adb && forcewake-build/test"'
```

This is regression-tested driver logic, not a SPARK proof or native integration.
Before native use: bind clock/pause callbacks to the native runtime, hold device runtime power,
serialize domain ownership (including reset), validate ordered MMIO mappings,
and provide controlled write authority. Callback execution and scheduling must
themselves be bounded: the adapter cannot preempt a stalled callback, so the
deadline check is not a hard real-time latency guarantee. No GPU firmware or
display state is changed by these tests.

Forcewake ownership follow-up: a limited-private `Lease` tracks Idle/Held/Faulted.
Nested acquisition, unmatched/double release and retry after fault return
Invalid_State without device accesses. The lease is pessimistically marked
Faulted before callbacks, so a callback exception cannot leave it reusable.
Successful acquire changes it to Held; acknowledged release changes it to Idle.
There is deliberately no software-only clear-fault method. Recovery must first
establish a fresh hardware lifetime. Hosted tests cover these transitions and
injected callback failure. Limitedness prevents copying a lease, but does not
prevent creating multiple leases for the same hardware: the native driver must
own exactly one per domain and serialize access. This is not thread safety or
a PCI/runtime-power reference. The existing v2 D0 snapshot is still insufficient
to enable writes while a separate power manager could change device state.

Static bring-up claim audit (2026-09-27): the current devmgr has no Intel GPU
runtime suspend/PMCSR update path. The private native snapshot now freezes
PCI configuration writes for the discovered Intel BDF before resuming its
inspection child. Both PCI data-write wrappers reject that BDF thereafter;
the child receives no raw PCI port capability. The freeze is conservative on
child failure/exit: no implicit rebind or power transition. Kernel PCI write
helpers exist, but the inspected kernel tree has no call sites for them.
This is a trusted devmgr policy boundary, not capability revocation of devmgr's
own broad PCI authority and not a guarantee against SMM/firmware interference.
Future suspend/reset/rebind must replace this boot-lifetime freeze with explicit
quiescence and ownership transfer, not bypass it.

Private `make -C kernel devmgr` compiles/links with this change. No native
regression boot, proof, write grant, forcewake execution or new image is claimed.
The shared devmgr integration still awaits coordination; do not overwrite it
with this older private snapshot, which lacks unrelated networking updates.

Forcewake authority follow-up: private devmgr now accepts canonical empty
request 0x022A only from its recorded Intel child carrying tag 0x4947. With
the BDF frozen, it rereads PCI configuration, requires D0 and the original
vendor/device identity, replans BAR+0xA000 and checks it still matches the
original claimed BAR base. It then mints a single 4 KiB read/write device-memory
capability into child slot 5. The existing 2 MiB mapping stays read-only.
Issuance is one-shot; failed/uncertain client receipt does not permit automatic
replacement. This grants a whole page, not register-level isolation: the driver
is trusted not to touch neighboring registers. No PCI, DMA, GGTT or display
ownership is included. Fresh reads do not exclude SMM races; firmware stability
remains an explicit platform assumption.

The private devmgr compiles/links, but the native client has not requested or
exercised this capability yet. Rejection-path native tests and integration of
the forcewake adapter are next; no new image is published by this change.

Native-client follow-up: the service now submits 0x022A asynchronously with a
30-second grant wait, validates its completion, maps the one-page grant at a
separate virtual address and runs the real forcewake generic with volatile
32-bit accesses, GETTIME and 1 ms sleeps. The write callback permits only
0xA188 and the two masked request values. Successful acquisition is immediately
released; no protected GuC reads, reset, firmware upload or display takeover
is attempted yet. Native compile/link passes.

QEMU RAM fixture `i5n05jfp` exercises the real client and devmgr denial path:
the synthetic device has no frozen physical PCI claim, receives grant-denied,
publishes this result through logstore, and Desktop starts. The native checker
and boot-log viewer test pass. This does not test successful mapping/writes or
Intel forcewake. The fixture catalog/audit were restored; the generic private
image remains test-only until rebuilt. The published v20 image is unchanged.

Correction and granted-path test: run `_n_n2mvf` exposed that the old private
devmgr `waitReady` consumed an unrelated Intel request and returned NULL. Thus
`i5n05jfp` proved client rejection of a bad reply, NOT the intended policy
denial. The private helper now matches the expected driver PID and replies with
canonical F002 (busy) to early authenticated 022A requests. The native client
retries only that explicit busy result, within its original 30-second budget.

Run `i4ua5qpy` then passed the private RAM granted-page fixture: the actual
driver obtains the page, maps its alias, writes the request, times out against
the deliberately zero acknowledgement, and writes the clear mask. QMP physical
memory inspection at backing+0xA188 independently observes 0x00010000. Native
checker and log-viewer tests pass with forcewake=acquire-timeout and Desktop
alive. This is native mapping/write/timeout cleanup evidence, not GPU emulation
or successful physical forcewake. Catalog/audit restored; generic private image
is still test-only until rebuilt. The fixture branch is gated by the existing
map-check bootstrap file and is not enabled in the normal catalog.

### NUC forcewake test image v21

Private `kernel/cubit_n95_intel_forcewake_v21.img` is the normal catalog,
not the RAM fixture. SHA-256:
`e7505b4db21cfd025f5363b60299dd6b9ce4b5a9acb37fc6691d5c1813a7765e`.
The adjacent `.plan.json` records image inputs. Audit verifies 8 bootstrap
files and 21 required optical payloads. QEMU UEFI four-CPU USB-flash tests:
`8fqcbknb` downstream hub keyboard/mouse PASS; `4ostyg64` startup viewer PASS.
These normal QEMU runs do not emulate the Intel GPU. v20 remains available.

Physical success marker: `forcewake=release-ready` in the Intel snapshot log.
It means both acquisition and release acknowledgements were observed; it is
not firmware authentication, rendering or display takeover. On failure capture
the exact forcewake result and whether Desktop/input still function. The probe
does not retry a failed hardware handshake or reset/upload firmware. This is an
isolated bring-up snapshot, not a rebuild of newer shared networking work.

### Firmware packaging checkpoint

The subsequent private CCL image declares two supplied artifacts, `intel-guc`
and `intel-firmware-license`, placed in optical `firmware/intel/tgl_guc_70.bin`
and `licenses/Intel-GPU.txt`. The Linux-hosted realization wrapper resolves
both pinned Nix inputs from `tests/intel-gpu/firmware-source.nix`; nothing is
downloaded by the running OS or embedded in the driver's executable. The ISO
auditor traverses the actual primary filesystem tree and checks both original
SHA-256 hashes. This preserves the firmware binary and redistribution notice
byte-for-byte; packaging is not authentication or an upload authorization.

Build/audit `jj5jbjs1` passes with SHA-256
`1d399ca71afd2c5f61555756353937ca8a410bfbd8ccffbb131cfcb01ea8d006`.
This changed only the generic private image; published v21 is unchanged.
The new packaging image has not been boot-tested or offered as a replacement.
Driver-side bounded file loading, compatibility/version checks, GGTT upload
mapping and hardware authentication are still outstanding. Shared image/build
definitions have not been overwritten with the private snapshot's versions.

### Firmware namespace correction

Inspection of native `ISO9660.Find` showed that its bootstrap lookup starts
at the ISO's `apps` directory, even for paths with a leading slash. The initial
root-level firmware package above was therefore not reachable through that
interface. The private catalog now puts the binary at
`apps/firmware/intel/tgl_guc_70.bin`; the future client's lookup path is
`firmware/intel/tgl_guc_70.bin`. The independent ISO audit traverses that same
directory and verifies the pinned hash. The redistribution license remains
at ISO-root `licenses/Intel-GPU.txt`; the driver does not need to read it.
This does not broaden the filesystem namespace or grant the driver access.

Rebuild/audit passes; generic private image SHA-256 is
`f1bbc6098d307b77c12d049b617cef8caf0cdade848d58851bf1cc9f47efd994`.
QEMU run `prgfja19` passes UEFI four-CPU USB-flash/hub boot and log-viewer
replay. This is a boot regression plus an on-disk path/hash audit, not a native
firmware-read test or physical GPU test. Published v21 remains unchanged.

### Bounded firmware reader

`Intel_GPU_Firmware_Reader.Load` now implements the transport-independent
file-to-private-buffer path. It rejects sizes outside 128 bytes..1 MiB or
larger than caller storage, requests at most 4 KiB per read, advances by the
actual returned count, rejects zero progress and oversized replies, and only
returns a valid CSS plan after reading the whole declared file. The 1 MiB
limit is an explicit bring-up resource budget, not a claimed hardware maximum.
The caller owns storage; this helper allocates no heap memory.

Nix-hosted reader regressions pass, including short reads, partial failure,
EOF and extreme array origins. The real-file adapter successfully reads the
entire pinned 335360-byte GuC image through the helper. Existing Intel admission
regressions pass too. These results are not SPARK proof of this reader, native
IPC evidence, or authentication. Next: native bounded file-handle/loan adapter
and per-driver read scope. No image or device behavior changed in this step.

### Native firmware-file adapter (not activated yet)

`Intel_GPU_Firmware_File` compiles against the freestanding CuBit runtime in
the private workspace. It opens the fixed firmware path through an already
held filesystem endpoint, issues positioned reads through the bounded reader,
and closes the handle before returning private storage and its CSS plan.
It validates completion tokens, reply shape and read counts; a single 30-second
monotonic budget bounds the asynchronous sequence (assuming clock progress and
scheduling). A separate process-lived, page-aligned 4 KiB scratch grant receives
filesystem writes. Only completed successful reads are copied into the private
1 MiB heap allocation. Neither allocation is reclaimed in this one-shot
bring-up implementation. A timeout quarantines the outstanding loan; revocation
is not treated as cancellation or proof of retirement.

The adapter is not yet invoked by the driver, and no filesystem authority has
yet been granted to it. Compilation is not native I/O validation. Next is the
read-only firmware scope and a QEMU native-read fixture before activating the
path on physical Intel hardware. No reset, DMA upload or authentication occurs.

### Native firmware loading activated and tested

The private devmgr now installs a filesystem Read_Objects scope for
`firmware/intel/tgl_guc_70.bin` before granting filesystem endpoint slot 6 to
the Intel child. This uses the existing path-prefix policy, not a new immutable
file-object capability. It grants no filesystem write/create authority. The
driver invokes its one-shot loader after the forcewake probe and before
publishing its snapshot. Explicit diagnostic strings are necessary: this
minimal runtime rendered the status enumeration as `0`, not `LOADED`.

QEMU RAM-backed Intel fixture `5bgscj9s` passes: native scope installation,
filesystem read of 335360 bytes, CSS code size 334976, forcewake timeout/clear
write, logstore/viewer delivery and Desktop startup. The native checker passes.
Earlier run `vg16y1as` read successfully but was stopped while waiting for the
incorrect enum string. This proves the native happy path, not denial cases,
firmware authenticity, or physical GPU execution.

The fixture bootstrap entry was removed afterward. Normal private image
build/audit `u06c7bad` passes with SHA-256
`8805b48c92eb5a4d5e5fc434159fe36636368bad49b9a83f471446eeba469dfc`.
Published v21 remains unchanged. Native deny-path tests, compatibility admission,
GPU mappings, reset/upload/authentication and rendering remain outstanding.

### Native missing-scope rejection

QEMU run `ke_dcinq` deliberately granted the fixture's filesystem endpoint
without installing any path scope for the fresh Intel process. The native
filesystem rejected open; the driver reported `access-denied bytes 0 code 0`,
published the failure through logstore/viewer, and Desktop started normally.
Forcewake timeout cleanup still passed. The runner's
`--intel-firmware-denied` option asserts these markers and absence of a loaded
result. Reproducing this test requires the map-check bootstrap fixture and
temporarily replacing its `Grant_Intel_Firmware` call with endpoint-only setup.
Those temporary changes were restored after the test.

The first run `rys9unqw` exposed a diagnostic mismatch, not an authorization
bypass: filesystem open errors use an all-ones invalid-handle value. The
adapter now recognizes canonical access-denied replies with the appropriate
sentinel. That first run was stopped while waiting for the expected label.
This tests missing policy, not every path-alias or write-denial scenario.

Normal private build/audit `h5pizeul` passes, SHA-256
`7596d8f836bb65940adcfdb7837093cb96ee1a37811a55f59f202227cb966d04`.
Published v21 is unchanged; no firmware upload or physical GPU claim.

### Selected firmware metadata admission

`Matches_Selected_ADLN_GuC` admits only the selected 70.49.4 metadata and
layout: 335360 total bytes, 334976 code bytes, 256 signature bytes, expected
module type/header version/module ID/vendor, modulus/exponent counts, software
version and private-data size. Values come from the hash-pinned artifact;
field offsets and version encoding follow the
[Intel-authored Linux CSS ABI definition](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_uc_fw_abi.h.html).
This is a deliberately narrow bring-up allowlist. A pin update requires review
of this policy. The header does not itself establish platform compatibility,
authenticity or that CuBit implements every firmware protocol feature.

Host tests admit the actual pinned blob, reject adjacent lengths, and reject
352 single-bit mutations across checked fields. GNATprove level 2 passes for
the firmware unit, including the postcondition that admission implies valid
layout. The native loader now returns `metadata-rejected` and no usable address
when these checks fail. Freestanding driver compilation passes; this changed
gate has not yet been boot-tested or packaged into a new image. Firmware bytes
and headers remain unauthenticated until an integrity/authentication mechanism
is established. No GPU upload or execution is enabled.

### Native metadata gate and GGTT window planning

QEMU fixture `cn9_24i5` passes native loading with the metadata gate enabled,
forcewake timeout cleanup and boot-log replay. Normal catalog restored and
image audit `cl20t9a9` passes, SHA-256
`338f947308f346b5ccf43d781ddb8044daed1e032da7093bab44d34e117502fc`.
Published v21 remains unchanged.

`Intel_GPU_GGTT.Plan_Window` computes the page-rounded table MMIO window for
a GPU virtual range. Gen8+ uses 8-byte entries and a table aperture beginning
8 MiB into the 16 MiB register BAR; see the
[Intel GGTT implementation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ggtt.c.html).
The helper requires a discovered table size, rejects wrapping/oversized inputs,
and does not confuse GPU virtual addresses with CPU physical addresses. Hosted
boundary tests and focused SPARK level-2 contracts pass. It is not yet wired
to actual table discovery or MMIO access.

Before writes, establish table geometry, device-visible DMA addresses and
cache/translation synchronization, plus ownership of the chosen GPU range.
An apparently unused PTE is not sufficient evidence that firmware scanout or
another engine will never use that range. Preserve existing mappings until
the handoff/reservation rule is explicit; do not clear the whole GGTT merely
to bootstrap GuC. This remaining hardware integration is not proved by the
arithmetic helper.

### GGC geometry handoff

Bootstrap v3 replaces undeployed v2: word 3 low 16 bits contain version 3,
bits 31:16 carry raw PCI GGC, and the upper 32 bits must be zero. Private devmgr
reads GGC at GPU PCI offset 0x50 before its existing configuration freeze.
The Intel driver decodes GGMS bits 7:6 as disabled/2/4/8 MiB table storage;
an all-ones read is unavailable.
These field definitions follow the
[Intel PCI definitions](https://codebrowser.dev/linux/linux/include/drm/intel/i915_drm.h.html)
and the existing Gen8+ GGTT size computation. Other GGC fields are preserved
in the handoff but not interpreted as ownership or usable RAM.

All 65,536 inputs are regression-tested, with bootstrap upper-bit/version
rejection tests. Focused SPARK checks and private native builds pass. The
driver reports geometry in serial and its log snapshot. The synthetic fixture
uses GGC 0x40; no physical NUC GGC value has been measured yet. This change has
not been image/boot-tested yet. It adds no GGTT mapping or write capability.

### V3 native evidence and DMA lifecycle review

QEMU fixture `_rmbky0s` and the updated native checker pass the v3 GGC handoff:
synthetic GGC 0x40 yields 2097152 table bytes, visible in the log snapshot.
Firmware admission, forcewake timeout cleanup and Desktop/log-viewer startup
still pass. The fixture was removed and normal image audit `4s_qr7e8` passes,
SHA-256 `e60fd7393780954775a47f850b52d63ef7d8c2e91e4424a68ae73ced63ea9f90`.
Published v21 is unchanged. This does not measure physical NUC geometry.

The current native DMA allocator returns a contiguous physical allocation and
maps it with PG_USERDATA, not the MMIO mapping's uncached flags. GPU upload
must therefore explicitly establish the platform's memory-visibility rules;
an ordinary Ada copy alone is not our completed DMA-publication contract.
Process teardown calls releaseDMAAllocations. Its CPU ownership/loan lifetime
is not evidence that a device has stopped fetching those pages. Before GPU
submission, establish quiescence or DMA isolation and invalidate GPU mappings
before recycling memory, including abnormal driver exit. Kernel changes here
require coordination with the other agent; none were made in this step.

### Read-only GGTT inspection

Reset/handoff dependency audit is in [intel-gpu-reset-handoff.md](intel-gpu-reset-handoff.md).
Our GT-only handshake is not the all-domain/engine-preparation reset envelope.
Do not connect the hosted PTE publication helper directly to MMIO based only
on vacant PTE observations or a single GDRST acknowledgment.

`Intel_GPU_GGTT_Publish` implements a bounded transaction for a previously
reserved range and retained backing. It checks all destination PTEs are exactly
zero before any write; prepares memory visibility via a platform callback;
marks the result Quarantined before the first write; validates readback; and
requires successful invalidation before reporting Published. Any partial write,
readback failure or failed invalidation leaves quarantine. No rollback/free is
attempted. Published means mapping publication only, not firmware execution.
This is currently exercised with Linux-hosted adapters, not native MMIO.
Exclusive device/range ownership, scanout exclusion, DMA visibility and
correct platform invalidation remain obligations of the native adapter.
The preflight does NOT solve concurrent writers or reserve firmware-zero PTEs.

Native8MiB RAM fixture `wulobznq` passes full1048576-entry scan, four
sentinels, retained firmware preparation in4GiB guest, forcewake timeout
cleanup, logviewer and Desktop. Dedicated check-native passes. Normal image
audit `iwy3xqvk` passes; private `cubit_n95_intel_buffer_v24.img` SHA256
`f228ac404431bfc9e6d58e52667c75d96639ced189d11567b7ac33fe2bd71fec`.
Physical NUC validation is pending. Expected new markers are
`scanned= 1048576` and `firmware buffer prepared-retained (NOT GPU-published)`.
Present count depends on actual firmware state; do not expect fixture count4.

NUC-size correction:8MiB exceeds MAP_DEVICE's1024-page call limit. The driver
now maps consecutive2MiB chunks READ only. No kernel limit is raised. Failure
of any chunk stops inspection; already-created read-only aliases stay owned
by the process and are not reused. The updated native RAM fixture advertises
GGC=C0 and supplies8MiB, with sentinels at entries0,262143,262144,1048575 to
exercise both sides of a mapping boundary and the table end. Earlier2MiB
fixture results alone did not cover this geometry.

Full-table extension: native022B now returns the exact GGC-derived table
extent (2/4/8MiB), minted READ only by private devmgr. The driver checks that
extent against its bootstrap discovery and scans all entries. The RAM fixture
uses a2MiB allocation with present sentinels in entries0 and262143, producing
`present= 2 scanned= 262144`; this distinguishes full coverage from the old
first-page sample. No free-space ownership is inferred, no PTE is written,
and physical NUC full-table results are still pending. A candidate search must
separately exclude firmware scanout/aperture reservations and coordinate all
writers before any mapping can be committed.

The next mapping helper, `Encode_System_Page`, admits only nonzero aligned
device-visible DMA addresses below 4GiB and produces the system-memory GGTT
PTE (address plus present bit). This deliberately conservative bring-up limit
is not an assertion about ADL-N's full address width. It does not program MMIO,
reserve an address, establish DMA ownership, or select CPU cache attributes.
Do not use CPU virtual addresses or assume CPU physical equals DMA address
once IOMMU translation is active.

Reference: Linux `gen8_ggtt_pte_encode` and `gen8_ggtt_insert_entries` at
[revision c14adcbd1a9648dc9d16dfd12c1e9bc0c14ef6aa](https://code.googlesource.com/linux/torvalds/linux/+/c14adcbd1a9648dc9d16dfd12c1e9bc0c14ef6aa/drivers/gpu/drm/i915/gt/intel_ggtt.c).
GGTT encoding is not PPGTT encoding: the system-memory path must not add
PPGTT write/cache bits or the Gen12 local-memory selector. The reference also
explicitly does not enforce read-only access through this GGTT path. A CPU
read-only grant consequently must not be described as GPU write protection.
Native use still needs reserved GPU VA disjoint from firmware mappings,
ordered PTE publication and platform-correct translation invalidation.

Physical v23 photo confirms `forcewake=release-ready`, `file=LOADED
bytes= 335360`, and `ggtt= 8388608` with `present= 512`. The last count is
only the first 512 entries in the single mapped 4096-byte table page, not the
entire GGTT. Table size is 8 MiB of translation entries, not available video
memory. Every sampled entry has its present bit set; neither unique backing
nor active use is established. Do not infer a free GPU VA range from this
sample or overwrite it. Firmware scanout/Desktop remains visibly working.
Photo source: attachment 85FF96B6-5454-4B5A-830F-9ABC4015DFF2/1-Photo-1.jpg.

Short-record regression: QEMU fixture `it_y9tya` and check-native pass four
separate viewer records (power snapshot, forcewake, firmware file, GGTT).
One publisher grant is reused only after definitive completion; uncertain
completion stops later sends without recycling its loan. Normal non-fixture
image audit `mla2dfrr` passed; v23 SHA256:
`c9506f3e9987933082faae51491f2b2cbb12bb5e76f8d6a8d491f8817f31753f`.
No new GPU operations; this addresses the clipped physical diagnostic text.

Physical NUC photo received after v22 handoff confirms the snapshot contains
`firmware=00005405 driver=0000FC0F; forcewake=release-ready; firmware scanout
retained; file=LOADED`. This is evidence that the bounded GT forcewake acquire
and release handshake completed on hardware and that the firmware file passed
the loader's admission checks. It is NOT firmware authentication/upload or GPU
execution. Desktop remains visible. GGTT fields run off the photo's right edge
and cannot be claimed from this evidence. Split the diagnostic into shorter
records before the next image so those fields remain readable.

The private native path now supports a single 4096-byte GGTT inspection grant
(022B). Devmgr authenticates the tracked Intel child and endpoint authority,
revalidates frozen PCI identity, BAR, D0 and nonzero GGMS, then grants READ
only at BAR0 + 8 MiB. The driver maps it read-only, samples the first PTE as
two 32-bit reads and counts present bits among the first 512 entries. This is
diagnostic sampling, not an atomic snapshot, allocation map or ownership claim.
No GGTT writes, GPU submission or display takeover are introduced.

Native builds and QEMU fixture `vyfck9c9` pass: a separate RAM page containing
one sentinel entry produces `first=0000000012345001 present= 1`. The fixture
also passes forcewake timeout cleanup, native firmware loading and Desktop/
log-viewer startup. QEMU is testing our IPC/mapping plumbing, not Intel hardware.
The physical NUC result remains untested, and published v21 is unchanged.

After removing the fixture, normal image audit `pyrtkaw3` and QEMU USB hub
input regression `wv268224` pass. Image SHA-256:
`1622fee74c8fb2486e300efa7aac1cd864511bc878a33d9201c40ecc5760b27c`.
