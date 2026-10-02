# Native discovery and session admission integration

Source audit, 2026-10-01. This is an implementation handoff, not a claim that
native Vulkan device enumeration or creation works today.

## Native Mesa instance execution evidence

On 2026-10-01, `mesa-native-instance` passed in native CuBit/QEMU (TCG,
512 MiB), using the real statically linked ANV archives, native libc and an
authority-free ELF manifest. Two successive instances each created successfully,
returned `VK_ERROR_INITIALIZATION_FAILED` with zero devices when no trusted
provider was installed, and were destroyed. The runner completed its fault scan.
This is native instance lifecycle execution, not a hosted mock, Linux Vulkan
loader, physical-device success, rendering or hardware acceleration.

Build under the shared lock in Nix using
`tests/mesa-anv/test-native-instance-link.py BUILD --retain-transport --no-provider`.
Stage the resulting `mesa-no-provider.app` into `kernel/isodir/boot`, then run
`tests/headless/run.sh --test mesa-native-instance --accel tcg,thread=multi`.
The runner copies the base disk and installs the test-only startup profile;
the production live image remains unchanged. Verdicts use explicit
`cubit_debug_write`, not unconnected stdout. The generic 128 MiB fixture could
not load the static image; the default matches other Mesa tests at 512 MiB.

Evidence: `/tmp/cubit-mesa-native-lifecycle-final.serial` and
`/tmp/cubit-mesa-native-lifecycle-final-run.log` (run12245 terminal0), linked
artifact `tests/mesa-anv/target/native-instance-link.hrjxoqtu/mesa-no-provider.app`.
Earlier runs with insufficient RAM or invisible stdout verdicts are failures,
not successful native execution evidence.

## Existing pieces and missing connections

### Opt-in hardware device image

`images/mesa-device.ccl` selects `tests/hardware/init-mesa-device.ccl`:
Desktop and Boot Logs start normally, then a manifest-approved
`mesa-logical-device.app` runs the real ANV instance/physical-device/logical-device
lifecycle. It replaces the render-session fixture only in this opt-in profile;
normal live images remain unchanged. After linking and staging that executable,
`tests/usb-optical/build-live.sh "$DOOM_WAD" --uefi --mesa-device` selects it.
Use Nix and the shared build lock. This profile is not proof of device creation.

Expected successful hardware sequence includes memory-policy2, create0,
enumerate0/count1, MESA-DEVICE create0, destroyed, then result0/close0.
Explicit beginning markers bracket potentially blocking Mesa create, enumerate,
and destroy calls. A missing completion marker is not success. Mesa device
creation may submit internal setup batches; this fixture submits no application
drawing batch and does not establish hardware-accelerated Mesa presentation.

Packaged hardware checkpoint: `kernel/cubit_live_mesa_device.img`, SHA256
`f47c592873702bc10cc0513c96e3ebaae76c182b16a981988ff8e52f4c85d583`.
Probe link49923 and packaging99662 passed. Exact-image QEMU31360 passed
UEFI/USB-flash/no-PS2 Desktop/input regression. With no Intel GPU, procmgr
loaded the actual probe ELF, denied render admission, and did not resume it
(serial lines725/734 in
`/tmp/nix-shell.DmMwb5/cubit-usb-live.y82umnm5/serial.log`).
Thus this verifies packaging and the no-provider denial, not Mesa device
creation. The adjacent image plan records staged component hashes; this is
not a clean rebuild of every system component.

### Authorized native discovery fixture

Separate triangle hardware checkpoint:
`kernel/cubit_live_mesa_triangle.img`, SHA256
`de0e2ec10bf6ed0a59db5f88610d85d0e74b768a59191aa3ce4059b7ba092e0f`.
Select with `build-live.sh "$DOOM_WAD" --uefi --mesa-triangle` after staging
the native linked `mesa-triangle.app`. Image tests21182(all21), package52950,
exact-image QEMU31396 passed. QEMU loaded the real triangle ELF then denied
render admission without an Intel backend; Desktop/USB boot succeeded. Serial:
`/tmp/nix-shell.NDo7Gw/cubit-usb-live.kodgi3he/serial.log` lines725/734.
This is not triangle execution. On hardware, require pipeline ready, readback
red1152/blue2944/other0, pixel mismatches0, triangle result0 and final
discovery result0/close0. The fixture is offscreen only; there is no triangle
window. Device-only image SHAf47c5928 remains unchanged.

The further opt-in `--triangle-smoke --shader-dir DIRECTORY` mode (requires
`--logical-device`, excludes transfer mode) links `mesa-triangle.app`.
`build-triangle-shaders.py DIRECTORY` uses glslang and spirv-val in the Nix
host environment to generate Vulkan1.0 SPIR-V, not Intel machine instructions.
Mesa compiles those shaders when creating the graphics pipeline. The probe
draws three vertices into a64x64 RGBA8 optimal image, transitions it for copy,
copies to a coherent host-visible buffer, and waits for a fence. Every pixel
is checked against the geometric triangle at viewport vertices(8,8),(56,8),
(32,56); doubled pixel centers never land exactly on an edge. This avoids
accepting a correct center pixel with a broken surrounding image.

Linux software oracle:
`nix-shell tests/mesa-anv/triangle-host-shell.nix --run 'bash tests/mesa-anv/test-triangle-host.sh'`.
Run82551 passed on llvmpipe:1152 red +2944 blue,4096 checked,zero mismatches.
Log `/tmp/cubit-vulkan-triangle-host.log`; exact validated shaders and hashes
in `/tmp/cubit-vulkan-triangle.h2ErTs`. Native final link9672 passed at
`target/native-instance-link.lpiyktow/mesa-triangle.app`. No native execution,
Intel GPU execution or presentation is established by this Linux oracle/link.
The device-test image remains unchanged; neither transfer nor triangle mode
has silently replaced its fixture. Resource teardown after submitted work
requires fence completion/device loss, or idle/device loss after submit error.

The Linux oracle now explicitly requests Khronos validation and synchronization
validation, captures warnings/errors through a debug messenger, and makes either
fail the executable. Pinned layer search paths avoid mixing host distribution
manifests with the Nix Mesa manifests. Run49527 passed4096 pixels with zero
warnings/errors. A separate CPU-ICD-only negative-control process attempts a
zero-size buffer and must produce a validation error and exit7; it also passed.
Evidence: `/tmp/cubit-vulkan-triangle-validation-controls.log` and
`/tmp/cubit-vulkan-triangle.z8c7xK/negative.log`. This validates API usage in
this tested path, not all failure paths, native transport, or Intel execution.

An additional `--transfer-smoke` linker option (requires `--logical-device`)
builds a separate `mesa-transfer.app`. It records `vkCmdFillBuffer` into a real
Mesa command buffer, adds TRANSFER_WRITE-to-HOST_READ synchronization, submits
once and waits on a Vulkan fence before checking all1024 mapped words. It uses
only an advertised HOST_VISIBLE|HOST_COHERENT memory type. A timeout/transient
wait error retains all resources and polls without resubmission; device loss
uses Mesa teardown and the backend's independent native backing-retirement rules.
This is transfer/queue validation, not a shader, drawing or presentation test.
It does not replace the offered device-only image.

`test-native-transfer.py PREPARED_MESA_SOURCE` checks the fixture with hosted
Vulkan-call mocks: barrier ordering, full readback, corruption, timeout and
transient wait retention, device loss, absent coherent memory/entrypoint, and
ten failing creation/bind/map/record/submit calls. Failed submit requires device
idle or device loss before cleanup; it is not treated as proof that no work ran.
Final51448 passed those mocks and native linking
(`target/native-instance-link.4_4qlz_l/mesa-transfer.app`).
No physical transfer execution is established by these tests.

The fixture also requests a named logstore publisher capability. A stateless
test-only Ada FFI wrapper (`mesa_probe_log`) holds the real CuBit.Logging
publisher while C executes. Console messages remain available, and bounded
single-line copies go to Boot Logs. A missing/busy publisher disables further
publication without replaying the GPU operation or changing its verdict.
The wrapper waits for both grant retirement and outstanding logging completion
before releasing stack storage; no publisher state is kept behind a dangling
C pointer. This fixture has no competing async IPC completion consumer.

Logged logical-device link25435 passed, artifact
`tests/mesa-anv/target/native-instance-link.p9iko508/mesa-logical-device.app`.
This verifies native linking, not actual log delivery or GPU execution. It is
not included in the already-offered memory-admission image; that image still
runs the independently logged render-session fixture.

The GPU-independent `mesa-log-bridge` native regression exercises this same
Ada wrapper from C against the real CuBit logstore and observer capability.
It checks two sequential publisher lifetimes, preserved callback return values,
one authenticated record per invocation, malformed-text rejection, and observer
close. Returning from each wrapper requires grant and completion retirement.
Build with `make -C kernel mesa-log-test`, then run
`bash tests/headless/run.sh --test mesa-log-bridge --accel tcg,thread=multi --timeout 60`
inside Nix while holding the shared build lock (logstore must be staged).
This is a native logging regression, not a Mesa GPU or physical NUC test.
Native build39477 and 60-second CuBit/QEMU run3486 completed successfully;
`/tmp/cubit-mesa-log-native.serial` contains both round verdicts and final PASS,
with the runner's final fault scan passing in `/tmp/cubit-mesa-log-native.log`.

`tests/mesa-anv/test-native-instance-link.py BUILD --retain-transport
--authorized-discovery` links `mesa-authorized-discovery.app` with the real
Mesa archives and native Ada IPC implementation. The manifest requests render
authority; its generated Ada binding supplies the slot to C, without a numeric
slot assumption or PID/name lookup. The fixture installs an explicit provider,
enumerates through Vulkan instance dispatch, destroys the instance, checks
provider-reference balance, then closes its own render session once.

This is test bootstrap, not the production multi-device provider. By default
logical-device creation fails. The provider and budget are process-owned,
and the fixture refuses a second invocation or a second session opening.
The existing independent session-admission work remains required for arbitrary
production vkCreateDevice calls.

Adding `--logical-device` produces `mesa-logical-device.app`. After coherent
physical-device admission it retrieves that device, creates one render queue
through real Vulkan dispatch and destroys the logical device before destroying
the instance. The first device-open callback transfers the fresh launch-supplied
session through `anv_cubit_attach_owned_session`. Both failed and successful
opening attempts consume the one-shot opportunity. A transferred pin is never
closed again by the fixture: the Mesa tracker owns its close/retirement.
The process drives detached retirement and stays alive on uncertainty, with no
slot replacement or backing reclamation claim. Its retirement callback only
publishes an atomic notification. Mesa construction can execute internal setup
batches; "no application drawing batch" does NOT mean "no GPU work".

Logical-device native link43543 passed, artifact
`tests/mesa-anv/target/native-instance-link.kmss3m22/mesa-logical-device.app`.
Hosted callback test94546 covers failure before transfer, failure after transfer
and success; all reject another opening even after a retirement notification.
This is compiled integration plus mocked ownership testing, not native device
creation or hardware execution. No offered image was replaced. The service now
has conditional policy2 admission as described below; until a NUC run meets
those conditions, successful native physical/device creation remains unverified.

`MESA-DISCOVERY memory-policy=1` followed by explicit-only admission diagnostics
is NOT a pass: it records the current blocker without misclassifying any other
factory failure as expected rejection. Only coherent policy, successful
enumeration of exactly one device and balanced references return discovery
success. Even then no logical device or GPU submission has been tested.

Native link15327 passed on 2026-10-01, artifact
`tests/mesa-anv/target/native-instance-link.zmjyx6o4/mesa-authorized-discovery.app`.
No execution or image packaging is claimed. Launch requires explicit startup
render approval on Intel hardware; QEMU without that backend cannot establish
the positive path. Initial compile20230 and link36631 failed respectively on
generated-binding lookup and a direct ANV symbol reference; both were corrected
using the generated output include path and Vulkan dispatch lookup.

Hosted rollback regression27193 passed 21 injected failure positions across a
three-provider inventory: each of the six state/device allocations and each
of the three retain, device-query, memory-info, memory-type and WSI steps.
Every failed transaction leaves the public device list empty, preserves only
the installed inventory's pins/allocations, leaves shared logical-memory
accounting unchanged and never opens a logical session. Clearing the injected
failure permits a complete three-device retry and balanced final teardown.
The fixture checks that each intended failure was actually reached. These are
mocked factory dependencies, not native IPC, hardware recovery or a claim that
every upstream Mesa allocation failure is covered. Actual ANV budget dispatch
also passed in that hosted run.

### Cache-contract evidence (2026-10-01)

The user-provided Intel TGL PRM Volume 6, revision 5.23, "Memory Interface
Control Registers" (printed pp16-18) and "Required PAT & MOCS Tables"
(pp20-22), are the register-policy reference. Volume 7 revision 12.21,
"GPU/IA Level Coherency" (pp6-7), distinguishes coherent snooped L3 flows
from software-managed non-coherent flows. These TGL documents are not by
themselves proof of every ADL-N platform condition.

Pinned Mesa26.2.3 maps PCI46d2 to adl_gt05. Its GFX12_PAT_ENTRIES assigns
cached_coherent to PAT0/WB and describes two-way coherence. The device inherits
has_llc through GFX12/GFX11/GFX9/GFX8. CuBit's application binding path uses
Write_Back PPGTT leaves (PAT selection bits zero), and its PAT0 setup selects
WB. This is agreement of source-level settings, not a hardware execution result.

The ordinary integrated Gen12 branch in Mesa's isl_device_setup_mocs selects
internal2, external61, uncached/blitter3 and HDC48. Hardware also selects
entry63 for L3 evictions: Intel requires LLC cacheability and L3 uncached there.
The Ada table supplies those settings; adln_mocs_tests now checks the eviction
fields explicitly as well as the existing raw oracle and command-selected
entry checks. Entry61 deliberately disables LLC caching for displayable
surfaces; a blanket "all entries WB" admission test would be wrong.

Admission is limited to matching CPU WB aliases of the retained owned arena,
GPU PAT0 PPGTT and current PAT/MOCS/power/reset ownership. Effective WB system
RAM remains a platform prerequisite, including firmware MTRR configuration;
the driver does not measure or repair arbitrary MTRRs. Mesa barriers and device
synchronization remain mandatory. A CPU-no-flush hardware probe is a regression
check, not a replacement for the documented platform contract. The Vulkan
physical-device gate remains intact and consumes the driver's actual policy.

`Intel_GPU_Memory_Admission` now expresses the candidate decision separately
from IPC encoding: exact46d2, current runtime/ownership, healthy caller session,
no fault, plus CPU-to-GPU and GPU-to-CPU no-CPU-flush boot checks. Without both
checks a live supported session remains explicit-maintenance; lost ownership,
session health or runtime availability yields unavailable. The checks supplement
the documented owned-WB/PPGTT contract, not infer coherence for other memory.
Hosted5888 covered64 Boolean combinations and65536 PCI IDs. Focused SPARK8453
proved the decision postcondition and two termination obligations, with no
unproved/justified checks or assumptions. Hardware coherence and effective RAM
memory type are outside that proof.

Native build29871 now wires this policy into main's authenticated Memory query:
exact envelope/selector, kernel-stamped caller resolution and Session_Healthy,
current Publication_Owner_Ready plus Context_Owner and no runtime fault. Boot
qualification is retained only after diagnostic work completes and the context
is disabled: CPU-copy result must equal the expected value; the GPU no-flush
sample must equal both the flushed sample and expected red-center/zero-corners
content, following a checked target clear. A failed/missing check leaves a
healthy session explicit-maintenance. Current health/ownership loss returns
unavailable even after successful boot qualification. Allocation zeroing,
initial flushes and submission barriers are unchanged.

Separate hardware image `kernel/cubit_live_memory_admission.img` was packaged
with the updated driver and logged render-session fixture (native CPU mapping,
offline bind/unbind/rebind and private-context initialization, not Mesa drawing).
Build/package29621 passed firmware/license/image audits. The prior offered
`cubit_live_render_session.img` retains SHA256321e4fdcb2c961f2fcbbc2e164af0ba9f23236b1d72b9d7d285166f7e2580c1a.
No physical policy2 result is claimed before the NUC test.
Exact-image QEMU21630 passed UEFI/USB-flash4CPU/no-PS2/quiet-xhci desktop/input
regression. The Intel-only fixture was denied before resume, as expected
without supported hardware; no positive session/GPU execution is inferred.
Image SHA256: `939bdc30c17ee72aaa6194911171952c27c57df178077e6179fcb6223701a290`.

- `anv_cubit_physical.c` constructs physical devices and now implements
  per-instance retained discovery inventories. Trusted bootstrap must call
  `anv_cubit_install_discovery` with the authorized providers before enumeration.
  No capability is acquired by installing descriptors.
- `tests/mesa-anv/instance-platform.patch` selects native enumeration and
  releases discovery after common instance teardown. Missing inventory fails
  explicitly; an explicitly installed empty inventory enumerates zero devices.
  Device construction is transactional: failure destroys all pending devices
  before anything becomes visible in the instance list. Production bootstrap
  still does not install a provider; there is no DRM or fake-device fallback.
- `Intel_Render_Admission_Native` implements reserve, delegate, activate and
  abort. `Intel_Render_Admission_Dispatch` routes completion tokens. The
  production devmgr now instantiates this broker;
  `tests/mesa-anv/native-integration/gpu_admission_probe.adb` is its native
  protocol fixture, not evidence of production launch success.
- procmgr's normal main loop blocks in `receive`. Its startup render handshake
  now explicitly polls completions and waits on activity while the child stays
  suspended; the launcher currently owns the sole asynchronous completion
  token range in this process. Future asynchronous subsystems must use a common
  completion router, never privately consume one another's completions.
  The dispatcher exposes `Runnable` and `Next_Deadline`: process queues fairly,
  advance ready work, and only sleep on activity when no immediate broker work
  remains. Take the minimum deadline across all hosted subsystems. Expired or
  explicitly cancelled pending requests retain their receipts but stop returning
  their old deadline; late completion still schedules abort. This scheduling
  interface is used by devmgr's central broker loop.
- devmgr's GPU viewer grants are diagnostic-specific. They are not general
  application render-session admission and must not become ambient Mesa
  authority.
- `intel-gpu/main.adb:Handle_Render_Control` derives backend readiness from
  current ownership and retained resources. Bootstrap source binds the
  controller only after checking registered devmgr sender, the kernel-stamped
  `Intel_GPU_Boot.Broker_Tag`, message shape and decoded resource plan.
  devmgr's GPU endpoint31 carries that tag and READ|WRITE|GRANT; derived client
  endpoints must omit GRANT. Both production mains and the native admission
  fixture compile with these changes; positive production admission on hardware
  remains unverified. Readiness requires a completed boot marker, acknowledged
  disabled diagnostic context, valid GGTT ledger, retained submission/scratch
  backing, idle allocation/update paths and current GuC/engine ownership.
  Only the authenticated broker can trigger that observation; abort does not
  require a healthy backend. Activation separately checks the installed sharing
  recipient and waits for private backing. This observation does not reserve
  allocation capacity. No shipped startup app opts into render admission.
  Never bind whichever process sends the first request.
- Native discovery requests the private-VM policy; `VM_Query_Policy` resolves
  the kernel-stamped admitted session. A raw diagnostic endpoint is not a
  substitute for an admitted discovery session. Keep that session separate
  from each logical device's independently retired render session.
- `Application_Recipient` in the driver additionally expects immutable
  recipient endpoints in capability slots 40..55, indexed by reserved session
  tag. The native broker now accepts a separate grantable application endpoint,
  validates its captured incarnation, and derives it READ-only into the captured
  driver before deriving the READ|WRITE render endpoint into the application.
  Each step yields to the dispatcher; failure or cancellation aborts instead of
  activating. No raw-PID mint or onward GRANT is used. Production startup must
  supply both source endpoints and CSPACE authority for both recipients,
  keeping source slots immutable and driver destination slots stable through CPU-grant
  retirement. A successful reserve/delegate/activate probe alone does not
  establish that application buffer sharing can work.
  Activation now separately requires `Recipient_Ready` (default false).
  The native handler uses authenticated `Activation_Identity` followed by
  `Endpoint_Matches` for the driver-side slot. This is a snapshot check, not a
  replacement for bootstrap's immutable-slot lifetime guarantee. Hosted
  controller fixtures explicitly model the endpoint check; they do not prove
  the kernel installation path. The focused SPARK proof covers the default-
  deny activation contract and checked identity/tag extraction, not hardware.
  Reciprocal broker/dispatcher hosted tests now cover all16 driver slots,
  endpoint attenuation, partial failures and cancellation between delegations.
  The native fixture also passed in CuBit/QEMU with private bootstrap permissions
  and the actual installed-recipient inspection on activation. All four admission
  cases, memory-query and baseline IPC checks passed with runner exit0 in
  `/tmp/cubit-reciprocal-current.sZ4sWkoT/`. Production startup/provider wiring
  remains pending; this synthetic GPU service does not exercise Intel hardware.

## Ownership requirements for the production provider

### Launch authority boundary (protocol implemented; handlers pending)

Keep the GPU's existing controller at devmgr. procmgr already receives explicit
CAP_CSPACE root authority from devmgr for manifest policy installation; it can
derive a generation-bound application endpoint into devmgr slots40..55 without
introducing another policy root. Those are broker-local sources, distinct from
the GPU driver's independently owned recipient slots40..55. Both sets must stay
reserved through the corresponding lifetime.

`Intel_GPU_Broker_Request` defines the separate procmgr-to-devmgr request:
label4948, a distinct kernel-stamped launch authority tag, and four words
`[version, broker-local source slot, app destination slot, nonce]`. The decoder
requires the expected launcher's kernel sender/tag and exact envelope before
converting bounded slot fields. It accepts no application PID. Slot range
validity is not policy approval or permission to overwrite occupied slots.

Production wiring must still install the dedicated launcher endpoint, reserve
and deduplicate source slots/nonces, capture and validate the actual source
endpoint, retain the incoming reply while admission progresses, route completions,
and resume the child only after success. Failed or undelivered replies need
abort/retirement; neither a timeout nor a decoded nonce releases authority.
The pure decoder's focused proof establishes field checks and range safety,
not these kernel/launch/lifetime obligations. It is not yet called by either
production service.

`Intel_GPU_Broker_Launches` supplies the bounded request ledger for that adapter.
It retains nonces and captured identities, rejects exact/conflicting replay,
forbids reassigning a reserved source slot to another incarnation, and rejects
multiple requests for the same application's destination slot. It separates
admission completion from taking a one-shot reply capability and recording
delivery. An undelivered successful admission produces an explicit abort
obligation. Entries are not recycled, including failures; `Acknowledged` means
the admission reply was delivered, not that GPU health or authority persists.
Focused SPARK contracts cover reservation eligibility/count, one-shot reply
selection and delivery outcomes; hosted tests exercise replay, capacity and
late/duplicate transitions. The ledger itself neither holds kernel reply caps
nor executes aborts.

`Intel_Render_Broker` now composes that ledger with the admission dispatcher.
It authenticates/decode-checks before saving the launcher's implicit reply,
captures the actual application source endpoint, and rejects replay before
overwriting any reply slot. A trusted caller supplies disjoint reserved reply
slots; the adapter also rejects source/implicit-reply/retained-reply overlap.
The saved reply is distinct from the application recipient endpoint. Terminal
admissions remain runnable until their one-shot reply is attempted. Failed
success delivery calls the actual dispatcher cancellation path; slots and
ledger entries remain retained. Reply words are `[version, outcome, nonce,
captured application identity]`, with outcomes 0 admitted, 1 rejected, 2 uncertain.
No reply field itself grants authority. The service must reject ID=0 requests
immediately; nonzero IDs transfer reply duty to the adapter.

Hosted `memory-fixture/broker.gpr` tests exercise successful delivery, failed
success delivery followed by abort, timeout with late reserve/abort receipts,
save failure, replay and unsafe reply slots. Existing dispatcher regressions
also pass. The generic adapter compiles against the native CuBit runtime;
these results do not establish native execution of the adapter. Production
devmgr now routes the launch label and completion tokens through this adapter
in its central activity loop. It reserves reply slots16..26,28..30,32,56,
skipping RTC27, GPU control31 and viewer57; source slots40..55 remain reserved
for launcher-supplied application endpoints. Admission gets a five-second
monotonic deadline without expiring active sessions. Procmgr's tagged broker
endpoint issuance and suspended-child continuation are now implemented, but
the production launch path still needs native positive/negative validation.
The Mesa provider remains unwired.
The production devmgr build/link and native CuBit/QEMU `devices` regression
passed with this loop (`/tmp/cubit-devmgr-launch-devices.serial`): the inspector
retrieved inventory and opened its window, with the runner's fault scan clean.
This exercises ordinary service routing, not positive GPU admission or rendering.
Caller-owned slot reservations and the
single-owner non-reentrant service loop remain explicit integration obligations.

`Intel_Render_Launch_Client` supplies the launcher's asynchronous side and is
called by production procmgr for explicitly approved render requests. A
trusted approval plus a
previously captured child endpoint is required. It derives READ|GRANT recipient
authority into one permanently reserved devmgr source slot, submits the tagged
launch request, and accepts success only from the expected broker completion
with the exact envelope, nonce and child incarnation. The caller must route
real kernel completions and keep the child suspended until `Admitted`.
Malformed or failed completions become `Uncertain`; they do not authorize
resume or source reuse. The broker owns the timeout; the launcher cannot treat
a local timeout as cancellation of an outstanding reservation. One retained
launcher instance exclusively owns remote slots40..55 and its completion-token
range. Hosted tests cover policy denial, generation mismatch, submission and
delegation failures, malformed replies, duplicate/unrelated completions and
retained capacity exhaustion. Native launch-side execution now passes in the
private CuBit/QEMU fixture: `/tmp/cubit-launch-native-fixed.lO21IGrp/` completed
the full async-ipc regression and required saved-reply/reciprocal-admission
marker. The synthetic GPU and broker are co-located, the launcher is separate,
and test-only bootstrap policy supplies their authority. This verifies real
kernel IPC, not production procmgr policy selection or hardware rendering.

### Provider lifetime requirements

1. Trusted startup/broker supplies an authenticated GPU identity and a pinned
   discovery endpoint. Serialize discovery calls on that endpoint. Never
   rediscover authority using a PID or application-provided device name.
2. Each logical device obtains a separately admitted render session. Capture
   the destination process incarnation before delegation; activate only after
   successful delegation. Keep the child suspended until its required
   authorities are ready, or expose an explicit asynchronous request protocol.
3. Keep process/GPU memory accounting shared across instances. The provider
   retains the accounting record; physical-device creation must not reset it.
4. Endpoint ownership transfers atomically to the logical-device lifetime
   through `anv_cubit_attach_owned_session`. The input pin descriptor is
   cleared when consumed, including failures after attachment. An unchanged
   descriptor remains caller-owned. The copied notification context is
   process-owned, not allocated within the Vulkan device wrapper.
5. `anv_cubit_memory_finish` detaches the Vulkan wrapper before retirement can
   complete. `anv_cubit_memory_poll` retains the slot and CPU cleanup state.
   The provider must keep the endpoint pinned through this detached lifetime,
   including quarantined/uncertain retirement. Physical-device destruction is
   not permission to revoke or replace that endpoint.
6. `anv_cubit_memory_slot_retained` is an observation, not an atomic lease.
   A check followed by independent slot replacement is not a safe transfer.
   The owned attach API copies an exactly-once confirmed-retirement callback.
   That callback may only publish an atomic notification under the transport
   lock. Provider cleanup separately consumes the notification; it must not
   release/recycle the endpoint after an uncertain close. The production
   provider and its capability reservation mechanism still need wiring.

## Required integration evidence

Trusted startup plans can now express `(render approve-declared)` on a `start`
entry; omission or `(render deny)` denies approval. This is separate from the
executable's request. Duplicate or unknown approval forms are rejected and the
host tool makes approval visible as `render=declared`. Procmgr now consumes
this field only for trusted startup launches. The executable declares
`(request-render read-write render)`; kind11 is distinct from the existing
kind2 GPU service request used by display ownership. Both reserved parameters
must be zero, the destination must be valid, and duplicate render requests
are denied. Ordinary runtime spawn does not carry render approval.
No shipped profile enables it. The initial native regression caught an
incorrect interception of display's service17 request; the distinct kind11
fix passes all 30 hosted manifest tests, including duplicate render rejection.
Production procmgr/devmgr rebuilt and the native CuBit/QEMU devices regression
passed with inventory and native-window markers plus the runner fault scan
(`/tmp/cubit-render-launch-fixed.serial`). This restores display startup;
Positive production render admission evidence is still needed; negative
production coverage is described below.
A timeout retains remote
bookkeeping and never resumes the child; it does not prove remote retirement.

Production negative admission is now exercised by `render-launch-policy`:
unapproved launch does not submit; approved launch submits once but receives
no rendering session on QEMU; neither sentinel executes; both child stops
succeed and the subsequent Devices window opens. Native run9537 passed the
strengthened oracle and normal fault scan (`/tmp/cubit-render-policy-fixed.serial`).
The initial run exposed missing WRITE authority during failed-launch cleanup.
Procmgr now mints a WRITE-only, generation-bound process capability for that
specific rejected child using its existing policy authority; it does not add
wildcard kill authority. The kernel's stop return is checked. Positive
production admission, actual GPU-resource retirement and Mesa creation remain
separate outstanding evidence; successful synthetic reciprocal IPC is not a
substitute for them.

- Real kernel-stamped admission and discovery, not hosted reply stubs.
- Two instances share GPU accounting but two logical devices do not share a
  render endpoint. Destroying either physical wrapper cannot revoke the other.
- Failed admission, cancellation, child exit and late replies cannot resume
  an unauthorized child or activate a replacement process with the same PID.
- Failure both before and after tracker acquisition has one identifiable
  cleanup owner; delayed retirement cannot recycle a slot early.
- Native instance enumeration, logical-device creation and Mesa rendering are
  exercised separately from the existing native hand-built GPU triangle.

The coherent-memory gate remains independent: policy 1 must not be relabeled
policy 2 merely to make enumeration pass. A linked factory is not evidence
of hardware-coherent Vulkan host-visible memory.

### Driver activation completion boundary

Intel driver activation now retains the broker reply in dedicated slot60
(budget61/application62 stay separate). It waits for the private context,
page-table and scratch allocation to complete before acknowledging success.
Completion rechecks the active captured identity, the kernel recipient endpoint,
usable backing and current driver health. Empty/pending/retired backing cannot
report session health or a usable private VM. Allocation failure, cancellation
or failed success delivery closes admission and starts retirement; it never
converts uncertainty into permission to free backing. The single-owner loop
serializes completion with incoming abort requests.

Target compile/link39787 passed; exhaustive phase/boolean readiness tests and
focused SPARK analysis passed (9 checks, none unproved or justified). The proof
covers the pure lifetime unit, not MMIO, kernel reply transport or the whole
driver. Successful native/hardware execution of this deferred activation path
remains unverified. The subsequent live-readiness integration target-compiled
and linked successfully (direct Intel build72804); control hosted tests and
focused SPARK analysis also passed. The full `make intel-gpu` attempt56481
stopped earlier in the CCL manifest tool (`LIST_BUILTIN` missing case), so the
direct build used the existing unchanged Intel manifest object. This is not a
fresh system-image build or a hardware admission result. Memory policy remains
explicit-maintenance policy1; this readiness change does not enable the Mesa
coherent-memory policy2 path.

Owned attachment was target-compiled and exercised with mocked IPC by
`test-native-memory-policy.py /tmp/cubit-budget-snapshot-build`: invalid/aliased
attachment leaves the pin unchanged; deferred cleanup survives erasing the
wrapper; confirmed retirement notifies once; immediate failed health checks
still consume their pin; 128 completed lifetimes can reuse bookkeeping; an
uncertain close never notifies. These are hosted regressions, not a native
provider or kernel capability-transfer test.
