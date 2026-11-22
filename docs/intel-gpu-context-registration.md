# GuC context registration bring-up

## Current main-checkout checkpoint (2026-09-29)

The private repeated-submission work is now integrated into main: first marker1,
append the second segment, SCHED_CONTEXT notification, marker2 wait, then
scheduling-disable. Saved LRC and H2G pointers are sampled before disable;
logs use textual result names. The live append adapter never copies the
GPU-written context image. Its coherent saved-tail path is gated to PCI46D2.
Failures retain backing and quarantine; no general client submission API exists.

The segment now has96DWORDs/384bytes. The batch wrapper disables arbitration
on return; the corrected footer re-enables it, then emits NOOP, arbitration
check, NOOP. Intel TGL PRM vol2a printed957-958 requires off/on pairing and
warns of a hang if the ring empties with arbitration disabled. Linux v6.16
`gen12_emit_fini_breadcrumb_tail` restores arbitration too. MI_ARB_CHECK's
pre-parser control does not replace MI_ARB_ON_OFF. The latter now has an Ada
representation record covering all32bits, with independent field tests.

Last NUC evidence before the fix: initial marker1 completed; repeated marker2
and disable timed out. The corrected immutable image remains
`.build-workspaces/intel-presence.F8KpDB/kernel/cubit_live_arb_footer.img`,
SHA256 `dba26be5960b94798a699ecd2ef36fbb2cebe13772d8829041e0402341b0cba9`.
QEMU validates its desktop/software Mesa only, not Intel execution. Expected
NUC success was subsequently reported: repeated marker2, reads5, tail768;
saved-context head/tail both0x300; initialization marker COMPLETE and disable
COMPLETE. The private batch read returned1129661001, matching its expected
0x43554249 value. This verifies the fixed private-PPGTT batch memory write and
repeated ring submission on the user's NUC. It is not a drawing batch, Vulkan
conformance, general client isolation or a sustained stress test.

This section supersedes earlier segment sizes and one-shot-only statements
below. Numeric proofs/tests do not establish hardware completion or isolation.

## DC transition reference audit (2026-09-28)

The NUC now retains A/B pipe power but rejects C/D prerequisites, and reports
`native DC transition transition-failed`. Those acquisitions share admission
inputs; B requires PW1/PW2, while C/D additionally require the completed
DC-off reference. Do not treat this as evidence that C/D are absent, or skip
their scanout inventory before publishing GGTT allocations.

Rechecked Linux v6.16
[intel_display_power_well.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_power_well.c)
`gen9_disable_dc_states`: after disabling DC it checks CDCLK, verifies DBUF,
and restores combo PHYs on display11+. Its comment explains that DMC retains
port A context, but port B context can be lost across DC transitions. Thus
removing CuBit's PHY restore as a shortcut is not justified.

Also compared
[intel_combo_phy.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_combo_phy.c)
and
[intel_combo_phy_regs.h](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_combo_phy_regs.h):
the five known process/voltage combinations and programmed compensation
values, TX clock/divider mask, PCS run-once selection and lane0-to-group
offset difference match our plan. Linux does not exempt ADL-P/N from PHY_MISC
access in `has_phy_misc`. Linux falls back on unknown process values; CuBit
rejects them rather than guessing calibration. None of this proves the NUC
failure is in PHY restoration. Await the new stage/exit diagnostic before
altering register programming or preservation checks.

## NUC pipe IRQ readback correction (2026-09-28)

Hardware reported `reg=00044404 got=FFF9FFFF want=FFFFFFFF`, preventing
post-enable pipe ownership and therefore all downstream scanout collection.
The mismatch is exactly bits 17/18, not an MMIO authority rejection.
Pinned Linux v6.16 evidence:

- [i915_reg.h](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_reg.h)
  identifies 44404 as pipe A IMR and bits 17/18 as plane 6/7 flip-done
  interrupts for ICL/TGL.
- [intel_display_device.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_device.c)
  maps ADL-N to Xe-LPD/display 13, with four sprites plus the primary plane.
- [gen2_irq_reset](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_irq.c#L80-L91)
  writes all ones to IMR and performs a posting read without requiring
  all-ones readback, then disables IER and clears IIR twice.

The ADL-N-specific executor keeps the all-ones write but excludes only
bits 17/18 from IMR equality checks. IER and final IIR checks are unchanged.
This is supported by the absent-plane definitions and observed hardware;
it is not a claim that an Intel PRM read-zero guarantee was verified.
Hosted tests cover every readback bit and all four pipes, including both
values of each excluded bit; those tests and native link passed (60769).
Actual NUC progress beyond this gate remains unverified.

## Initialization ring segment (2026-09-28)

Native integration update (supersedes the unwired notes below): main now arms
the zero marker, publishes the initialization segment, registers/enables the
single context, observes completion while servicing bounded GuC events, and
attempts scheduling disable before diagnostic publication. Errors retain all
backing, fault the runtime and prohibit further submissions. A timed-out
marker does not prevent attempting disable if the context/transport remain
healthy; a quarantined context cannot be blindly resubmitted.

`intel_gpu_native_initial_ring` restricts accesses to the retained context
at CPU6108C000, its saved head/tail, 256 ring bytes at context+65536, and the
aligned64-bit HWSP D0 marker. Tail/ring visibility flushes expand to whole
owned pages, only before the context has ever been scheduled. Marker reads
flush its page before a volatile64 read; no CPU HWSP writes occur after
backing initialization. Ownership binds the prepared allocation identity
and initialized GGTT address to the active runtime. Hosted fixed-address
adapter test and native driver link67645 pass. This exercises CPU cache
instructions, not actual GPU DMA coherence or engine completion.

Expected hardware success is `initialization GPU marker COMPLETE; disable
COMPLETE (NO drawing batch submitted)` (enum spelling may vary with runtime).
That would prove this initialization sequence reached its final marker;
it would not establish arbitrary batch execution, Mesa acceleration or3D.
The subsequent diagnostic reports the last successfully read64-bit marker
and read count, including on failure. A marker observed after the deadline
remains a timeout, not successful bounded completion. Seventeen hosted wait
scenarios and the native link pass (16964); no new hardware result yet.

The separate `intel_gpu_initial_completion` helper now arms only after an
initial zero marker, starts its one-second deadline before publication, and
waits with a finite poll budget while servicing bounded GuC events. Ownership
and clock checks surround reads/event processing; nonzero values other than1,
stale markers, failed reads, backward/unavailable clocks, transport failures
and timeouts quarantine the attempt. No retry or backing release is provided.
Hosted regression23369 passes thirteen scenarios including a marker arriving
after the deadline and ownership loss during the final clock check. This is
not native-wired and does not establish actual DMA visibility or GPU progress.
Native integration must use cache-line-aligned flush ranges (the existing
DMA cache helper rejects a four-byte tail flush), and must service/disable
the context before allowing any potentially blocking diagnostic publication.

Completion-marker update: the segment now has64DWORDs (256bytes). After the
58DWORD initialization sequence, a six-word PIPE_CONTROL writes1 to the
same context-relative HWSP D0 scratch that the preceding barriers write0.
The final flags are CS_STALL, STORE_DATA_INDEX, QW_WRITE and FLUSH_ENABLE;
the destination and high data DWORDs are zero-extended. This follows the
post-sync layout in Linux v6.16 gen8_engine_cs.h and the context-relative
scratch addressing already used by gen12_emit_flush_rcs. It does not emit
a user interrupt, and is intended for bounded polling during bring-up.

The marker must start zero in a never-scheduled context. It is not reusable
as a submission fence, and observing1 does not authorize freeing context
memory before scheduling-disable acknowledgment. CPU visibility, initial-zero
admission, deadline/transport monitoring and native execution still need
integration. Updated encoding and134-callback publication tests plus native
link74537 pass. Focused SPARK65847 proves initialization, termination and
the builder validity/zero-rejection contract; not hardware command semantics.
The handed-off NUC image is unchanged.

The one-shot `intel_gpu_initial_ring_publish` executor now covers initial
publication ordering for a never-scheduled context: check zero head/tail,
write and read back the 58 command words, flush the ring for device visibility,
then write/read back/flush the saved LRC tail. It uses byte offset 4124 for
page1's CTX_RING_TAIL (index7), and the retained ring begins at context+65536.
Linux v6.16 `intel_guc_submission.c` updates this saved tail in
`guc_set_lrc_tail` before `guc_add_request`; first enable suffices for a
single-LRC context. This helper neither sends that enable nor proves execution.

Hosted test50910 passed all 122 callback-position failures and ownership
losses, all read-position corruptions, exact flush/tail ordering, invalid
input rejection, and retry rejection. Device visibility remains a callback
contract, not a hosted-test guarantee. It is not yet native-wired; the current
NUC image is unchanged. A GPU completion marker/timeout path and native
integration remain necessary before executing initialization on hardware.

Native preparation update: `intel_gpu_native_context_read` now reads only
WM_CHICKEN2 through an enabled DSS, saving/restoring MCR steering; its CPU
write callback admits only the selector. The generic MCR layer independently
rejects writes to WM_CHICKEN2. Failed transactions quarantine the reader.
Hosted fixed-address MMIO fixtures and MCR rejection tests pass (31207), and
native main links with construction of the initialization segment (84521).
The constructed segment is still not published to the ring or GPU-executed.

`intel_gpu_adln_context_init` encodes 58 DWORDs: full ADL-N RCS barrier,
the six existing context settings, then the same barrier. Each barrier is
22 DWORDs: HDC/cache flush with context-relative HWSP scratch write, disable
pre-parser, cache/TLB invalidation, remapped CCS_AUX_INV write and register
poll, then re-enable pre-parser. Scratch is byte D0 of the context HWSP;
it is not a caller-selected GPU address. This follows Linux v6.16
`gen12_emit_flush_rcs(EMIT_BARRIER)` and `intel_engine_emit_ctx_wa`.

Pinned source evidence:
- `drivers/gpu/drm/i915/gt/gen8_engine_cs.c`, SHA256
  `46eeb6b226aef7effbaf59d4f12dcd14333f04450cf68930b58a17407128e58e`.
- Matching header, SHA256
  `5d3def4da1e5469c10f4be1471574e01510d3353de3aae891fdfb4d2343c8d26`.

Hosted encoding tests pass with masks independently assembled from named
bit positions. Focused SPARK run59894 proves initialization, termination and
the validity/zero-on-rejection postcondition (3 checks, none unproved).
It does not prove hardware command semantics, AUX-poll termination, or the
trustworthiness of the supplied WM_CHICKEN2 sample. The segment is not wired
into native ring publication. Still needed: owned forcewake/MCR sampling,
ordered ring/tail publication, GuC submission, an externally bounded GPU
completion wait, and hardware validation. An empty-context scheduling test
does not establish any of these.

## Native empty-context scheduling integration (2026-09-28)

After RCS startup and the CT control roundtrip, native main now registers a
single retained RCS0 context (ID 7, fences 100..103; probe uses 42), queues its
policy, and waits for enable and then disable acknowledgments. The command
ring remains empty. Policy explicitly requests preempt-to-idle, a 1 ms
quantum, and a 500 ms preemption timeout; these are bring-up choices, not
hardware-derived limits or a claim of matching every Linux default.

`intel_gpu_guc_context_wait` applies a fixed one-second deadline across
callbacks and a poll bound for stopped clocks. Only explicitly nonpublished
backpressure permits retry. Acknowledgment, malformed transport, late request
failures and unrelated retained events use the existing typed session.
Ownership is rechecked after clock callbacks, including immediately before
accepting completion. Any failed attempt retains all backing and quarantines
the runtime. Native main keeps servicing late events on a 10 ms timeout while
the context path is healthy; other configurations retain blocking IPC receive.

The native service links and hosted session/wait regressions pass (36177).
This is not hardware validation: there is no submitted GPU workload yet,
and context initialization commands with their required barriers remain to
be integrated. No new boot image was generated for this change.

## Audited interface (2026-09-28)

The v70 single-context action is twelve DWORDs: action, flags, context ID,
engine class, logical engine mask, workqueue descriptor low/high, workqueue
base low/high, workqueue size, LRC descriptor low/high. Single contexts leave
the five workqueue words zero; those describe parallel parent contexts.
The LRC field includes descriptor flags, not just an aligned address.
Registration uses the nonblocking submission send path with zero expected
G2H words. Deregistration expects a separate G2H completion.

References: upstream i915
[registration preparation and send](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_submission.c.html#2539),
[nonblocking send wrapper](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc.h.html#357),
and [registration fields / KMD flag](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_fwif.h.html#230).
KMD is flag bit zero. These browsable references are not a pinned ABI test;
check the project's pinned Linux reference before encoding the live request.

## Consequences for CuBit

Do not reuse `Intel_GPU_GuC_CT_Roundtrip` as the context-registration executor.
That helper deliberately requires one synchronous, fenced success reply for
the logging-control probe. A context action needs its own message type and
lifecycle; waiting for that probe's reply shape would misdiagnose registration.

Keep the following evidence distinct:

- Request queued: host ring publication completed; backing remains retained.
- Scheduling enabled: the matching firmware event was validated.
- Batch completed: an engine-written completion record was observed with the
  required memory ordering. Neither of the preceding states proves this.
- Context retired: scheduling disabled and deregistration completed before
  reusing its ID, GPU addresses, ring or context storage.

Initial implementation should own one render context for the driver lifetime,
without ID reuse. It must still quarantine all backing on uncertain failure.
This is a bring-up lifetime policy, not the final multi-client allocator.

Before native submission, finish the LRC descriptor/state-image audit,
allocate and retain ring/context/PPGTT/batch/completion backing, establish GPU
visibility, and add asynchronous event dispatch with bounded response credits.
Numeric PPGTT preparation alone does not satisfy these requirements. Also
verify context policy setup and generation-specific context workarounds.

Acceptance is a real NUC batch writing a known completion value, followed by
readback and isolation/failure tests. Hosted encoding/state-machine tests and
QEMU desktop boots cannot establish this. No context registration or hardware
batch execution is claimed by this audit; the CT-roundtrip image is unchanged.

## Initial descriptor encoder

`Intel_GPU_ADLN_LRC_Descriptor` encodes the low descriptor word for an owned
four-level context: valid, force-restore, addressing mode and privilege, with
explicit low/normal/high EU priority. It checks nonzero page-aligned base and
extent below the GuC address ceiling. It does not check GGTT ownership, pin
bias, engine-specific image size or image contents; those remain caller duties.
The privileged bit is not an authorization mechanism. Do not pass unvalidated
client batches through this initial driver-owned path.

Pinned source: Linux v6.16 `gt/intel_lrc.c` (`lrc_descriptor`,
`lrc_update_regs`), `intel_lrc.h` (mode/priority flags), `intel_lrc_reg.h`
(force restore). Downloaded reference SHA256 values, respectively:

```
293671b2c52e14e93b54dbdb2c66f93803d9383f66cff1480a0697f03071a3e5
e71534746b6f6720c978c0096316c51873f5afc2974d94cd4979303384a6f5b3
671b2adfb2825597425eea515b892cba1365f658d552f9a90439182f0f7f21c0
```

Hosted test6030 passed all admitted one-page addresses for all three priorities,
every unaligned page offset, ceiling/overflow/empty rejection and a fourteen-page
render extent. This encoder remains offline: it neither creates nor registers
a logical context, and it is not included in the current NUC image.
SPARK job92218 passed runtime checks, termination and the invalid-input
zero-result postcondition. Exact valid encoding is regression-tested, not
claimed as a full functional proof or a hardware validation.

## Render register-page skeleton

`Intel_GPU_ADLN_LRC_Template.Build` emits the Gen12 RCS register-load layout
for the page following HWSP. Its five command groups begin at DWORDs
1,33,52,65,81; the inhibited-restore batch terminator is at185. Values and
unused words remain zero. This is not a complete runnable context. The adapted
register table retains Intel's MIT notice.

Isolated Nix job69732 passed hosted assertions and SPARK runtime/termination
checks. Independent Nix job58133 decoded the pinned v6.16 `gen12_rcs_offsets`
table and compared all1024 output DWORDs, including repeated addresses and
padding, successfully. This comparison is a regression check, not hardware
execution or a proof of the upstream table's correctness.

Next initialization must populate context control (inhibited first restore),
ring head/tail/start/control, timestamp, PML4 at PDP0 high/low, RPCS and the
required context workarounds; clear STOP_RING using the register's write mask.
The full allocation and every referenced buffer must remain owned and mapped.
Do not publish this zero-valued skeleton or label it submission-ready.

## Initial ring / VM register values

`Intel_GPU_ADLN_LRC_Initial.Build` now populates an empty ring and the PML4
root in that skeleton. Ring sizes are powers of two from4KiB through2MiB;
the whole aligned GGTT range must fit below0xFEE00000. The root uses our
existing nonzero aligned below4GiB DMA policy. Ring GGTT and root DMA are
different address spaces: comparing their numeric values would not prove
physical nonaliasing. Backing ownership remains the caller's obligation.

Initial context control enables synchronous-switch inhibit and engine-restore
inhibit, including both write masks. MI_MODE clears STOP_RING with its mask.
RPCS requests one slice, matching v6.16 `gen12_sseu_info_init` and
`intel_sseu_make_rpcs`: only slice power gating is requested for this path.
Additional pinned references:
[SSEU initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_sseu.c),
[engine register definitions](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_engine_regs.h).
SHA256 respectively:

```
862610546eb21a37e8349bb3eba2b1a79aabc92ab551872ff26f7e4f949a4666
e6ae20bc2946a252a948628df6a84522c19dd45d537f5efa0e5c55d08e82961a
```

Hosted tests check the entire page for all ten ring sizes at their highest
admitted base, all unaligned page offsets, ceiling crossing and invalid roots.
Job70992 passed tests but initially failed an integer-conversion proof.
The ring control now uses explicit32-bit shift/subtraction (all ten results
tested). Job89770 passed tests and SPARK runtime/termination/admission and
invalid-result-zeroing contracts. Valid page contents are tested, not fully
functionally proved. No native image changed.

Still NOT submission-ready: context workarounds, context/ring backing,
visibility and registration are absent. Audit the ADL-N predicates in
`gen12_emit_indirect_ctx_rcs` next, including timestamp, command-buffer,
scratch restoration, auxiliary-table invalidation and state-cache invalidation;
do not apply DG2-only workarounds by analogy.

## Indirect-context workaround batch

`Intel_GPU_ADLN_LRC_Workaround.Build` encodes the128-byte ADL-N render
sequence: restore timestamp twice through GPR0, restore command-buffer control,
restore GPR0, invalidate auxiliary tables and poll their register, then request
instruction-state-cache invalidation. No batch-end opcode is appended: this
indirect sequence is length-delimited, unlike the separate per-context batch.

Pinned v6.16 `i915_pci.c` maps ADLN IDs to `adl_p_info`, Gen12 without flat
CCS. `gen12_emit_aux_table_inv` therefore applies on RCS0; the main GT has no
media GSI displacement. The12.0..12.10 state-cache workaround also applies.
DG2 and12.70/12.71-only branches are excluded. References:
[platform selection](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_pci.c),
[auxiliary invalidation](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/gen8_engine_cs.c).

Allocation consequence: actual render-context backing needs16 pages, not just
the14-page saved image. `__lrc_alloc_state` adds separate INDIRECT_CTX and
PER_CTX_BB pages. Golden-image reservations remain14 pages; do not expand
those just because runnable contexts need extra commands. The encoder rejects
capacity below64KiB or a GGTT extent crossing the GuC ceiling.

Job83446 passed literal encoding/boundary tests and SPARK runtime checks,
termination and invalid-output zeroing. This is NOT hardware execution.
Before native integration, independently validate the command words against
the pinned definitions, assemble both workaround pages into full backing,
set indirect address/length and per-context pointers, and audit global/engine
workarounds separately. The native test image is unchanged.

Additional pinned SHA256 (`gen8_engine_cs.c`, `i915_pci.c`):

```
46eeb6b226aef7effbaf59d4f12dcd14333f04450cf68930b58a17407128e58e
c585105fd02dc4b155f1bd8d0c51f22ac077a7ae7afe391c3252153349add804
```

## Full initial context composition

`Intel_GPU_ADLN_Context_Image.Build` composes64KiB: zero HWSP, initial
register page, remaining zero saved state, indirect page and per-context page.
It fills context DWORD19 with the per-context address plus valid/force bits,
DWORD21 with the indirect address plus two64-byte units, and DWORD23 with
the Gen12 offset13 shifted by6. These indices are relative to the register
page. The separate per-context page contains only batch-end for RCS0.

The fast-color condition in pinned `intel_gt.h` requires COPY_ENGINE_CLASS;
it cannot apply to this render context. The indirect page also contains the
upstream predicate scratch helper at byte2048 and zero scratch at4088, outside
the128-byte indirect sequence. No ADL-N indirect command branches to it.

Context and ring half-open GGTT ranges must not overlap, in either address
order. Physical backing disjointness, identity/ownership, DMA mapping and
device visibility are still outside this numeric compositor's contract.
The first build75411 caught an invalid constrained-array conversion; explicit
page copying corrected it. Job62026 passed hosted composition/overlap tests
and SPARK runtime/termination/invalid-image-zeroing checks. These are not
hardware-context validity proofs. No native image or service entry point changed.
Final expanded all-scratch-word regression34824 also passed.

Pinned `intel_gt.h` SHA256:
`74be24390462209952217f05d61be0793a2b2d6aad1d4867a84ee40711c3c855`.

## Native retained backing views

The firmware allocation's unused tail now has an explicit layout:
context8C000..9C000, ring9C000..A0000, four page-table pagesA0000..A4000,
batchA4000..A5000 and completionA5000..A6000 (allocation-relative offsets).
The CT region ends exactly at8C000. The same initial allocation preparation
zeros/readback-checks/flushes this storage; no new syscall or devmgr grant is
introduced. `Prepared_Submission` returns typed CPU/DMA views only after the
allocation is ready and the region layout passes validation. These are not
GPU addresses and must never be independently freed.

Initial native compile85040 rejected quantified compile-time checks; those
became an explicit layout predicate while static allocation-bound checks stay
compile-time. Native compile84052 passed. Hosted3771 caught an always-false
constant warning under warnings-as-errors; separating that static bound fixed
it. Follow-up62232 hosted tests passed contiguous boundaries and expected
capacities. These checks do not test DMA coherence or hardware access.
Job62232 also completed the final native private compile successfully; lock
released. No native link or new image was produced in this step.

Next: independently validate command opcodes, reserve GPU ranges against the
existing publication ledger, initialize/copy the context and page-table images,
and establish visibility before publishing. The views alone publish nothing;
the current NUC image remains unchanged.

## Independent opcode check and publication boundary

Nix job41786 independently expanded the pinned command/header macros with a
restricted integer-expression interpreter and compared all32 indirect-batch
words against the Ada encoder. It passed, including register-vs-memory poll,
GGTT addressing, source/destination CS-relative bits, and masked state-cache
invalidation. The three context-memory operands were separately reconstructed
as base+4096+{35,183,117}*4. This checks encoding, not hardware behavior.

Native integration decision after inspecting `Prepare_Runtime` and the shared
publication ledger: publish only the contiguous80KiB context+ring region into
GGTT. Do not map the whole submission tail there merely because its physical
allocation is contiguous. Page-table pages need DMA addresses; they do not
need a command-accessible GGTT alias. Map batch and completion as the two
authorized PPGTT data pages (initial VA0x200000/0x201000). Those pages must not
include firmware, CT, ADS, context state or the page tables themselves.

Reserve the80KiB extent through the existing `Runtime_Publication` operation.
Its preparation callback receives the retained, checked GPU address: only then
compose the context pointers, construct/copy PPGTT and initialize the empty
ring. Finish cache visibility before returning success to the PTE writer.
Do not speculate a GPU address before reservation or bypass scanout exclusion.
The startup firmware mapping covers only512KiB, so it does not already expose
these tail regions. Native wiring and actual execution remain pending.

## Combined submission image

`Intel_GPU_Submission_Image.Build` now composes the entire106496-byte tail
image from a retained allocation DMA base and reserved80KiB GGTT start. It
uses the context builder, derives the four table DMA addresses from the same
backing layout, and emits every64-bit PTE as low/high32-bit words. The private
VM maps exactly batchVA0x200000 and completionVA0x201000. Ring memory stays
zero, batch contains only END, completion stays zero. No command is queued.

Isolated Nix job67824 passed hosted tests and SPARK runtime checks, termination
and invalid-image-zeroing. Tests verify every page-table word (including all
nonpresent entries), exact context root/ring addresses, ring/batch/completion
contents and upper-limit/overflow/alignment rejection. These checks do not
prove device isolation or hardware completion. The image is not yet copied
to native DMA backing or published; the NUC image remains unchanged.

Next native step: one-shot materialization into the exact retained tail view,
readback and cache visibility in the existing publication preparation callback.
Retain every reservation/backing region on partial failure. Only then enable
context registration; do not interpret image validity as scheduling authority.

## Submission-tail materialization

`Intel_GPU_Submission_Materialize.Write` accepts only an exact 106496-byte
slice and a valid prepared image. It writes every word in little-endian order,
including zero padding, without touching surrounding firmware or CT storage.
Invalid images and incorrectly sized slices leave the buffer unchanged.
Nix hosted test/proof job62849 passed: all words round-trip through bytes,
nonzero slice bounds and surrounding sentinels are tested, and SPARK proves
index/overflow safety and the success/rejection postcondition. These proofs
cover the pure copy routine, not native pointer validity or GPU visibility.

`Intel_GPU_Submission_Buffer.Initialize` is the native one-shot adapter. It
validates the retained allocation view, builds the image from its DMA base,
copies only the submission tail, performs volatile byte readback, then flushes
that private range. Failures consume the attempt and leave backing retained;
the initialized GPU address is recorded only on success. This relies on a
serialized startup caller and authentic, exclusively CPU-owned retained memory.
Private native compile job26924 passed. The adapter is not yet invoked by
`main.adb`; publication-callback wiring, registration and execution remain
pending. No NUC image was changed and no hardware result is implied.

## Native publication wiring

`main.adb` now invokes the adapter through `Prepare_Runtime`, after reserving
the context/ring extent and before any GGTT PTE write. The same runtime ledger,
scanout exclusions, native write bounds and translation invalidation used by
ADS/log/CT publication apply. Only 80KiB is mapped; the remaining 24KiB of
tables/batch/completion is prepared privately but receives no GGTT alias.
The diagnostic explicitly reports `context/ring mapping ... (NOT
registered/submitted)`. A failed attempt retains its claim and backing.

Private native link28037 passed. Hosted integration53293 passed for successful
publication, preparation/cache-visibility failure, and a store that reaches
the mock device but reports failure. It checks preparation precedes all PTE
writes, exactly20 entries map on success, no entries outside context/ring
change, and every outcome prevents reuse of its reserved range. The existing
GGTT publication regression suite passed too. Earlier fixture failures69995
and90870 were corrected to use the searcher's supported2MiB table geometry
with a small admitted aperture, not by weakening production admission.
These are callback-model tests, not Intel hardware execution; the live image
is unchanged. Context registration and actual batch completion remain next.

## Pinned registration request encoder

`Intel_GPU_GuC_Context_Request.Build` emits a GuC70 single render-context
registration payload: HXG header `0x20004502` (host FAST_REQUEST/action4502),
KMD flag1, owned context ID, render class0, logical engine mask1, five zero
workqueue fields, normal-priority LRC descriptor and zero descriptor high word.
The CT header/fence is deliberately not part of this payload. IDs0..65534
are numerically admitted;65535 is the firmware invalid-ID sentinel. The full
64KiB context must fit below the GuC ceiling and at/above aligned nonzero pin
bias. Mapping ownership/visibility and ID uniqueness remain caller obligations.

Pinned Linux v6.16 references downloaded from torvalds/linux:

- `gt/uc/intel_guc_submission.c`: registration at2539, preparation at2860+,
  policy initialization at2713 and scheduling event handler at5104.
  SHA256 `c70bc69a547096e2a841c3c894c34cdbd069294331a737df8eb82faae1a691bc`.
- `gt/uc/intel_guc_ct.c`: `ct_write` selects FAST_REQUEST for nonblocking sends.
  SHA256 `82c531410f28e7e0ccff066ac432e56dd7da8a946d087ba98cb4e8803d7511e6`.
- `gt/uc/abi/guc_messages_abi.h`: FAST_REQUEST type2 at bits30:28; host origin0.
  SHA256 `19f40e7445712c06546e0dde419ba9a23b334721e738bf26f3541bc9c8e6e53e`.
- `gt/uc/abi/guc_actions_abi.h`: REGISTER_CONTEXT4502, schedule-set1001/done1002.
  SHA256 `091add941d1967f6148eb1fb57a81d3415016ae7e03e5d1bb52b82d057c95cc3`.

Hosted/proof49256 passed all admitted IDs, all unaligned page offsets,
pin-bias and ceiling rejection, exact12-word payload and zero invalid output.
SPARK proves termination, runtime checks and admissibility/zero-output contract;
valid wire values are regression-tested. Initial89093 test aggregate syntax
failure was corrected. No native send or new image is included in this step.

Important for the next dispatcher: FAST_REQUEST has no positive synchronous
reply, but the ABI DOES permit a failure response when the recipient cannot
accept it. Do not discard matching failures as unrelated traffic. Linux's
scheduling-done handler requires at least two action-data words, looks up the
context ID, and rejects contexts without a pending enable/disable operation.
The mode event is separate from registration and GPU batch completion. Before
enable, policy setup also supplies priority, execution quantum, preemption
timeout and SLPC request, with conditional forced-preempt-to-idle. Those
requests and their lifetime/error accounting are still to be implemented.

## Policy and scheduling request encoding

The same request package now builds normal-KMD-priority policy (priority2),
execution quantum/preemption timeout in microseconds, SLPC frequency request0,
and optional preempt-to-idle. The valid payload length is10 or12 DWORDs, not
the entire backing array unconditionally. Header is FAST_REQUEST/action100B;
each KLV has length1. CuBit rejects zero quantum/timeout to avoid accidentally
disabling these firmware limits. Numeric nonzero values are encoded, not
validated as appropriate scheduling policy. Runtime selection remains pending.

Pinned `gt/uc/abi/guc_klvs_abi.h` SHA256:
`3bbb2fdee4289e23a19631b309d5b4d51ca561ffe5000b25078e6a28dd5ebe37`.
Keys2001/2002/2003/2004/2005 respectively encode quantum, timeout, priority,
preempt-to-idle and SLPC frequency. Reference construction is
`guc_context_policy_init_v70` in the already-pinned submission source.
The encoder takes preempt-to-idle explicitly; determining the ADL-N engine
flag at its initialization site is still pending (it was not found in
`intel_engine_cs.c`, so absence there must not be read as a false flag).

`Scheduling_Mode` emits FAST_REQUEST/action1001, context ID, enable1/disable0.
Callers must reserve response credits and set pending state before publishing,
then validate the distinct scheduling-done event; the encoder does not do this.
Tests/proof70590 passed registration regressions plus all admitted context IDs
for both policy lengths and scheduling modes, invalid IDs/disabled limits and
full-width timing values. SPARK proves runtime safety/termination and the
length/rejection contracts, not firmware execution. No native send or image
change yet; the asynchronous lifecycle/dispatcher remains the next step.

## One-context asynchronous lifecycle

`Intel_GPU_GuC_Context_Lifecycle` now models registration, policy, pending
enable, enabled, pending disable and disabled separately. Pending is recorded
before a non-reentrant serialized send. Only explicit no-publication
backpressure restores the prior state and permits retry. An uncertain send,
matching late failure, duplicate owned scheduling event or explicit timeout
quarantines the context. Four unique fences remain associated with the
lifetime, including already queued registration/policy actions. There is no
ID reuse, re-enable, deregistration or backing-release path in this initial
one-shot model; disabled still means retained, not reclaimable.

Enable/disable hold four response DWORD credits (CT+HXG+two action words)
before publication, releasing them only on explicit backpressure or the
accepted scheduling event. Quarantine retains outstanding credits. The caller
must reserve these credits and fences from the channel globally; this object
does not implement a multi-context/channel-wide allocator. The dispatcher
must validate raw HXG origin/type/shape before calling the typed event methods.
The scheduling-done second data word's semantics still need ABI confirmation;
the pinned i915 handler checks the length but does not inspect that word.

Hosted97297 and expanded82797 passed state-transition tests, late failures from
each action, backpressure retries for every operation, invalid admission,
wrong context IDs, duplicate events and failed reinitialization. SPARK97297
proved all advertised sticky-quarantine and accepted-event contracts plus
runtime checks (after syntax22710 was corrected). It does not prove firmware
behavior, global credit accounting or native dispatch. No native sends/image
changes; next wire validation and executor integration remain required.

## Scheduling-event decoding and runnable-state check

Resolved the previously open second-word question against pinned Linux v6.16
`drivers/gpu/drm/xe/xe_guc_submit.c`, `xe_guc_sched_done_handler` and
`handle_sched_done` (lines1896..1965): action data is context ID followed by
runnable state1(enable)/0(disable). SHA256:
`690dc4c9d50d04a1122d83fa5a36de06c7bba8fa234f792442887c8a03cee8b6`.
The lifecycle now checks this word against its pending operation; contrary or
invalid state quarantines without releasing credits. i915's omission of that
check is not copied into CuBit.

`Intel_GPU_GuC_Context_Event.Decode` accepts owned, transport-validated payloads.
For pinned scheduling events it requires exactly three words, header90001002,
valid context ID and runnable0/1. It decodes one-word GuC failure responses
with their original transport fence,16-bit error and12-bit hint. Invalid
origin/type/known-event shape is malformed. Other events and synchronous
response types remain explicitly Other_Message for the channel dispatcher;
they must not be silently discarded or counted as scheduling completion.

Nix hosted/proof85234 passed: every admitted context ID/both runnable states,
nonzero array bounds, empty/truncated/oversized known events, invalid IDs,
invalid runnable/header/origin, failure fields, unrelated messages and
lifecycle contradiction regressions. SPARK proves decoder bounds/termination,
decoded ID/runnable constraints and existing sticky-quarantine contracts.
No live driver dispatch/send is wired yet; no image or hardware claim changed.

## Nonblocking context session executor

`Intel_GPU_GuC_Context_Session` connects request encoding, pending lifecycle,
transport outcome and raw-event dispatch. It emits only each request's actual
length, records pending before invoking Queue, and rechecks ownership after
the send. Matching failures (including late registration failures) quarantine;
valid scheduling events must match the owned ID and pending runnable state.
Other valid messages go to a bounded caller retention callback; overflow is a
fault, not silent loss. Quarantined/fresh sessions cannot submit. The executor
is nonblocking; the native caller still must provide fixed monotonic deadlines,
globally reserved fences/credits and continued event service while enabled.

Nix hosted18457 passed an integrated simulated-channel register/policy/enable/
disable sequence, exact outgoing lengths/fields/fences, policy backpressure,
uncertain enable delivery, late registration rejection, contrary runnable
state, ownership loss and retention overflow. This is regression testing of
the executor using the proved helper contracts, not a proof of the generic
executor or real firmware execution. It has not been instantiated in main;
engine initialization/workaround admission must gate native enable. No NUC
image was rebuilt. Native transport adapters and bounded progress loop remain
next, along with explicit policy selection for the ADL-N render engine.

## Engine initialization audit and context-settings segment

The native path currently resets engines and builds GuC ADS metadata; that is
NOT equivalent to applying engine/GT settings or starting a render engine.
`Intel_GPU_ADLN_Engine_Settings` is currently consumed by the ADS register-list
builder only, not a native settings writer. Pinned `guc_resume` additionally
initializes MOCS, configures an engine HWSP (HWSTAM/HWS_PGA), selects nonlegacy
mode and clears STOP_RING with a posting read. These operations and applicable
GT/engine workarounds still require native admission before scheduling.

There is also a distinct context-initialization GPU segment in
`gen12_ctx_workarounds_init`/`intel_engine_emit_ctx_wa`, separate from our
indirect workaround batch. Pinned v6.16 `intel_workarounds.c` SHA256:
`4e62577e46d23025b82e27047fd1cdeea97054105272e4fac67261b555be2ba2`.
`Intel_GPU_ADLN_Context_Settings.Build` now encodes its six ADL-N render
settings as a14-DWORD segment (LRI, six sorted register/value pairs, NOOP).
It sets thread-group preemption, disables CPS-aware color/LE-GE depth/TDC
optimization, programs GS224/TDS128 timers directly, and preserves unrelated
WM_CHICKEN2 bits from an explicitly admitted read. FF_MODE2 must not use CPU
read-modify-write: upstream documents unreliable CPU readback.

This segment needs the upstream-equivalent surrounding flush/barrier commands
and must run as part of initial context setup before ordinary3D work. It cannot
be substituted for engine/GT startup or executed as an independent batch.
It is not yet inserted in the ring; WM_CHICKEN2 native forcewake/MCR read is
also pending. Nix hosted/proof36109 passed exact words, all single-bit baseline
preservation cases, invalid-read rejection and invalid-output/termination
contracts. No native image changed. Next: native MOCS/engine-start setup and
initial-ring barriers/breadcrumb, then guarded context enable on hardware.

## ADL-N MOCS plan

Pinned v6.16 `intel_mocs.c` selects `gen12_mocs_table` for ADL-N, not the
TGL/RKL compatibility table. The platform has global MOCS; uc_index=3 and
unused entries inherit entry2. `Intel_GPU_ADLN_MOCS` now supplies all64 control
entries at4000..40FC and32 packed L3 entries atB020..B09C. The latter use plain
MMIO on graphics12.0; multicast applies only from12.55. Entries62/63 are
programmed per reference but remain reserved for hardware, not app choices.
Source SHA256:
`8f789f79594c08dcc63f61dd20cd6e18d75d3c0baa5ae87d59b37d95a41427cc`.

Hosted/SPARK56436 passed packing/address/bounds/termination checks. Independent
reference check12436 passed every control/L3 entry: the new
`tests/intel-gpu/check-mocs-reference.py` hash-checks that source and evaluates
its integer macro expressions before comparing with the Ada test executable's
dump. No C code runs or enters the CuBit build. The comparison is reproducible
with the pinned reference file and `adln_mocs_tests` binary as arguments.

Native application is next: current reset grants cover4000 but notB000.
Extend the bounded page grant and exact-register writer, then configure/read
back the table under quiesced retained ownership/forcewake before new GuC
transactions. Do not merely use uc_index3 in CMD_CCTL before installing its
table. Upstream initializes L3 before GuC memory transactions and again for
render-engine resume. No native table writes or image changes in this step.
# Native MOCS and plane diagnostic follow-up (2026-09-28)

Native RCS-start adapter and main integration now install the separately
published HWSP and execute the checked start sequence after a successful CT
control roundtrip. MMIO allows only HWSTAM=allones, HWS_PGA=the bound page,
legacy-mode disable and STOP clear; only MI_MODE posting reads are exposed.
Owner readiness includes mapped/initialized backing, GT/render workarounds,
CT registration and live runtime admission. Failure faults runtime admission.
Native link and mock sequence60663 pass. No context is registered or submitted
by this change, no image was rebuilt, and hardware execution is unverified.

Native GT settings adapter and main startup integration are now present.
The mapped allowlist adds video pages1C3000/1D3000 (slots38/39), and the bounded
MCR transaction admits9550. Main applies GT settings before render settings;
both must be ready before firmware/runtime publication. The firmware-lock
exception is accepted only as the executor's distinct result and is logged
with raw9424 evidence. Native link and GT/page/MCR tests6117 pass. Hardware
MMIO behavior is unvalidated and no new image has been produced. RCS start
and asynchronous context execution still remain to be integrated.

GT settings executor now applies the pure plan with fresh RMW and posting
readback, ownership checks around callbacks, and one-shot failure retention.
An ignored9424 clear yields Ready_With_Firmware_Override and preserves raw
diagnostics; all-ones reads, writes failing, required-bit mismatches and owner
loss remain hard failures. Hosted95052 tests each outcome and retry rejection.
This is a mocked executor, not a native GT application or formal proof; the
native adapter and startup integration remain to be done.

GT-wide ADL-N settings are now represented separately from engine/context
settings. The pinned gen12_gt_workarounds_init path initializes default MCR
steering to the lowest enabled DSS, sets IECPUNIT_CLKGATE_DIS at each present
even VDBOX base+3F10, sets DFR_DISABLE at MCR9550, and clears bit1 of9424.
The last register can be firmware-locked: upstream explicitly suppresses its
readback verification. The plan represents that exception distinctly so the
native executor can report it rather than claim the clear took effect.
Hosted tests53693 cover63 nonempty DSS masks and8 media fuse combinations;
focused SPARK runtime checks and termination pass. Native application,
additional video-page grants and MCR9550 access remain outstanding.

Main now instantiates and executes render-engine settings after PAT/MOCS and
before firmware/runtime publication. Publication_Owner_Ready additionally
requires Render_Settings_Ready. Failure leaves backing unpublished and emits
an explicit textual status. Native driver link and executor/MCR/adapter
regressions79822 pass. This only covers the render-engine settings list, not
all GT/context workarounds; native RCS start and context execution remain
separate outstanding steps. No updated image or hardware result yet.

Native engine settings MMIO adapter now maps exact plan offsets through the
control-page grant list, including selector-page0 and engine workaround-page
E000 at slots36/37. Public writes require the admitted masked encoding or
fresh preserved-bit RMW value; direct public selector writes are rejected.
MCR transactions are internal, serialized, require an enabled DSS selected
from admitted topology, and preserve failure quarantine. Hosted17353 verifies
address allowlists and that unowned reads/writes perform no MMIO; thirteen-page
grant tests and native adapter/devmgr compilation pass. This does not test
positive hardware MMIO, and main does not yet instantiate the adapter.

MCR sequencing now has a bounded transaction implementation for the three
render workaround registers E18C/E4F4/E48C. It sets group0/enabled DSS at FDC,
enables multicast, preserves unrelated selector bits, verifies selection,
performs the target access, and restores/verifies inherited steering. Failure
is sticky; ownership loss stops further MMIO rather than attempting an
unauthorized restoration. A native adapter must still validate the selected
instance against admitted topology and map exact register pages. Hosted4094
passes multicast/selected-read, restoration, sentinel-read, failed selector,
failed restoration and ownership-loss cases; no hardware or SPARK claim.

Reference: Linux v6.16 drivers/gpu/drm/i915/gt/intel_gt_mcr.c, SHA256
a5f1ce46d0a99120cc1e9a1147c909cdb4ee4e1777d7228a1923db762df70bcf.
Pre-MTL steering saves/restores the selector; read selection and multicast
write semantics differ. CuBit explicitly establishes multicast even when
the inherited selector was in unicast mode.

`intel_gpu_engine_configure` applies the existing engine settings plan using
masked writes or fresh preserved-bit RMW as appropriate, with ownership and
readback checks and no retries of partial attempts. Linux v6.16
`intel_workarounds.c:wa_list_apply` distinguishes enabled-instance MCR reads
from multicast writes; this distinction is explicit in the adapter contract.
Hosted37715 exercises all five admitted engine plans and injected write/read/
ownership/readback errors. Native MCR selection/restoration remains absent;
this is a regression-tested executor, not hardware validation or a proof of
the callback assumptions. Existing NUC image remains unchanged.

The native runtime publication chain now maps the engine HWSP separately,
after successful context/ring publication. Its preparation checks the retained
typed page view and matching successful initializer address, then flushes
the exact4KiB page without reinitializing the published tail. The existing
reservation/PTE verification/invalidation transaction owns this mapping and
retains failed attempts. Extended mock tests confirm a separate21st PTE for
the A6000 physical slice, with private VM/batch/completion pages unaliased.
Hosted tests and native link94433 passed. Engine start remains unwired until
the remaining engine/GT workarounds are established. No new image/hardware
result is claimed by this integration.

The retained submission tail now extends through A7000, adding a distinct
4KiB Engine_Status_Page at A6000. Image/materializer lengths derive from
the backing extent (110592 bytes). The existing native initialization path
zeros, reads back and flushes this page with the rest of the tail, without
mapping it in either the context/ring GGTT interval or private PPGTT. Separate
GGTT publication and RCS-start binding remain outstanding. Native link22221
passed; hosted layout/image/materialization tests and focused SPARK checks
83784 passed. The handed-off diagnostic image is unchanged.

RCS startup now has a tested one-shot executor (`intel_gpu_rcs_start`), based
on pinned Linux v6.16 `intel_guc_submission.c` setup_hwsp/start_engine and
`intel_engine_cs.c` intel_engine_set_hwsp_writemask. RCS writes are 2098=all
ones, 2080=owned engine HWSP GGTT address, 229C=00080008, 209C=01000000;
209C posting read must not be all ones or retain STOP_RING. Ownership and
status-page readiness are checked around each write. Failure retains the
attempt, with no blind retry. Mock tests73274 pass; no native execution or
formal proof is claimed. This executor still needs a separate initialized,
published engine HWSP and engine/GT workaround completion before native use.
It is not part of the already handed-off gpu_diagnostics image.

The missing-log audit found a separate GPU caller bug: a cumulative dropped
record count latched Logging_Uncertain even for an acknowledged Rate_Limited
reply, suppressing all subsequent snapshots. The caller now checks the exact
matching completion shape and permits recovery for OK and Rate_Limited only.
Pending loans, transport failures and malformed responses remain fail-closed.
Hosted exhaustive 16-bit label classification and malformed reply tests plus
the native driver link passed in job52313. This establishes a code bug, not
that rate limiting actually happened on the NUC. Separately, logstore's
16-record recent queue cannot replay the whole boot to a late-opening viewer;
the current subscription resets its initial loss count. No logging-service
retention policy was changed here.

The ADL-N MOCS plan now has a bounded native adapter and a one-shot executor.
The executor validates all 96 register values with posting reads, checks
ownership around access, and fails closed on sentinel reads, write failures,
ownership loss or mismatched readback. Reset-page authority includes the
L3 MOCS page at B000; the adapter only accepts exact plan offsets and values.
GPU publication now requires both PAT and MOCS setup success.

Private native Intel link and devmgr compilation passed (job 96546), as did
hosted MOCS execution and eleven-page grant tests (52316). These are compile
and mock-MMIO results, not evidence of hardware MOCS initialization.

NUC reports of plane `collected=FALSE state=0` mean no valid sample: state0 is
Invalid_Read and raw zeros were default records. The native plane adapter now
preserves the collection outcome, with explicit textual diagnostics for power
unavailability, rejected prerequisites, failed reads and end-of-access power
loss. Main prints register values only for collected samples. Hosted tests
cover all twenty plane locations plus rejection and failed reads (82194);
the complete native driver links (19760). No image was rebuilt for these
changes and the underlying NUC failure is not yet diagnosed.
# Fixed private-VM batch branch (2026-09-28)

Ordering correction before packaging: preserve BOTH barriers emitted by
Linux `intel_engine_emit_ctx_wa` around context settings, then branch to the
batch, then emit an additional full barrier and final completion marker.
The preceding 70-DWORD development version incorrectly moved the trailing
workaround barrier past the batch. It was never packaged for the user.
The corrected ring is 92 DWORDs/368 bytes. Encoding, all 190 publication
callback fault/ownership-loss points, native mapped-memory tests, native
link and focused SPARK checks pass. This supersedes length/order notes below.

Reference: https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_workarounds.c

Readback integration: native publication now requires a zero dedicated batch
completion slot before touching the ring. After ordered ring completion AND
the scheduling-disable acknowledgment, startup reads that slot and requires
the probe value with a zero upper DWORD. Missing readback or mismatch faults
the context and retains all backing. The fixed page is derived from the
submission backing layout, cache-flushed, then read with aligned volatile64
access, with ownership checks before and after. No CPU store is performed.
Native compilation/link and the mapped-memory fixture pass; simulated writes
are host tests, not evidence of GPU cache coherency or hardware execution.
This supersedes the earlier pending-readback note below.

Follow-up: the native initialization ring builder now inserts this branch
after context settings and before its second full flush/invalidation barrier.
The final marker follows that barrier. Length is now 70 DWORDs/280 bytes;
the native publisher derives its write/flush bounds from the command array.
Hosted encoding, all 146 publication callback fault/ownership-loss injection
points, and the native mapped-memory fixture pass. Batch-result readback is
still required in startup; the ring marker alone is not a successful probe
claim. No new image or hardware execution result is available.

`Intel_GPU_ADLN_Batch_Start` now prepares the six-DWORD ring sequence from
Linux v6.16 `gen8_emit_bb_start`: arbitration enable, nonsecure PPGTT batch
start, the fixed submission-image batch address, arbitration disable, NOOP.
It accepts no arbitrary caller address or GGTT selector. The caller must
publish and retain the private page tables and batch before scheduling.

The hosted regression checks the complete encoding, selector, address and
alignment. Focused SPARK analysis succeeds (including termination); this
does not establish GPU command semantics. The initial encoder-only milestone
did not insert it into the ring; the follow-up above supersedes that state.
The integration retains backing on every failure.
The handed-off DC diagnostic image is unchanged; there is no hardware batch
execution result yet.

Reference: https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/gen8_engine_cs.c

## Live publication audit (2026-09-29)

This is a source cross-check, not verification of repeated execution on the
NUC. The private workspace has sequence-aware completion and SCHED_CONTEXT
notification code; native startup still runs only the initial probe.

Linux v6.16 establishes the following relevant boundaries:

- `intel_guc_submission.c`, `guc_set_lrc_tail`: write the saved LRC tail
  DWORD; `__guc_add_request` uses MODE_SET/ENABLE initially and SCHED_CONTEXT
  when already enabled. Do not register a new context for each submission.
- `intel_guc_ct.c`, `ct_write`: the publication barrier explicitly orders
  both the H2G words and the LRC tail before the CT descriptor tail update.
- `intel_guc.c`, `intel_guc_write_barrier`: system memory uses `wmb()`;
  local memory takes a different MMIO ordering path. Do not extrapolate the
  system-memory path to a discrete GPU.
- `intel_lrc.c`, `lrc_pre_pin`, and `intel_ring.c`, `intel_ring_pin`, use
  `intel_gt_coherent_map_type`. In `intel_gt.c` this selects WB for LLC
  system-memory platforms, WC for the non-LLC case (with additional local
  memory/media exceptions).
- `i915_pci.c` maps ADL-N to `adl_p_info`. GEN12 -> GEN11 -> GEN9 -> GEN8 ->
  G75 -> GEN7 inherits `has_llc=1`. GEN11 separately sets
  `has_coherent_ggtt=false`; that flag must not be mistaken for absence of
  LLC or used alone to infer the CPU mapping policy.
- `intel_ring.h`, `__intel_ring_space`, reserves CACHELINE_BYTES, not just
  one DWORD. Its documented constraint prohibits head greater than tail
  within the same cache line. Ring wrap/reuse must preserve this rule.

Local comparison: `Syscall.IPC.handleAllocDma` maps the retained allocation
as `Virtmem.PG_USERDATA` (PAT entry0/WB). The Intel allocation request's
argument `1` is not a WC cache-mode request. `Intel_GPU_Native_CT_Send`
already executes `mfence` before storing the descriptor tail and again in
Make_Visible. These observations explain why adding a generic whole-page
flush is not the right substitute for a coherent live-context contract.

Implementation consequence: keep Initial_Ring's never-scheduled requirement.
The live path needs explicit supported-platform/backing/coherency checks,
ring-space reservation, command visibility, a single saved-tail DWORD store,
then CT publication and notification. Do not write or copy saved head/state
fields. The existing full-page flush in Initial_Ring is justified only by
its exclusive never-run precondition; do not silently generalize it to
concurrently GPU-written context state. Sequence completion does not establish
context-switch completion or permission to release context backing.

### Context allocation transport regression

NUC artifact update: private `kernel/cubit_live_context_allocated.img` now
contains both the supervisor allocation branch and the driver connection,
while retaining the private L3/offscreen drawing diagnostic. Build, image
audits, and QEMU UEFI four-CPU USB-flash/Mesa launch-animation-close regression
pass. SHA256:
`3e7e1421eb6f4e5712429ce4d23f7a1b2ac5af97c12bd27338bdde7c63f2f16f`.
QEMU does not execute this Intel path. On NUC check `context backing retained=TRUE`,
context/ring and HWSP `PUBLISHED`, then L3 and drawing completion, center
`FFFF0000` with `match=TRUE`, zero corners, and final disable `COMPLETE`.
The earlier `context_backing.img` is preserved for comparison.

Native wiring update (2026-09-30): the main checkout's diagnostic now acquires
supervisor allocation slot 1 before runtime GGTT publication. Its context,
ring, private page tables, batch/completion storage and engine-status page
derive from that retained allocation rather than the firmware allocation's
tail. GuC context ID 7 is unchanged and is a separate namespace. Initial and
live ring CPU access uses slot 1's supervisor-defined window. Failed allocation
does not fall back to the firmware slice; existing publication/ownership gates
still apply. Native compile and full link pass, but this new wiring has not
run on the NUC. The private `cubit_live_context_backing.img` remains unchanged
and still uses the previous firmware-slice implementation. This is not yet
multi-context native integration or the Mesa render backend.

`tests/intel-gpu/context_memory.gpr` compiles the actual native
`Intel_GPU_Context_Memory` body against isolated clock/IPC fixtures. Run:

```sh
nix develop -c bash -c 'gprbuild -P tests/intel-gpu/context_memory.gpr && tests/intel-gpu/build-context-memory/context_memory_tests'
```

The 16 scenarios check exact outgoing requests, grants, bounded retry,
submission failures, foreign tokens, failure statuses, malformed envelopes,
denial, invalid backing addresses, unavailable/backwards clocks, elapsed-time
timeout, and the independent frozen-clock poll bound. Every scenario checks
that a second acquisition of the same slot cannot resubmit. This is hosted
regression evidence for the adapter, not live supervisor IPC, logger completion
demultiplexing, hardware execution, or a proof of the transport implementation.

### Sustained submission: transaction identity audit (2026-10-03)

The former context lifetime fence reservation was not sustainable. The original
`guc_context_lifecycle_tests` characterized a 256-ID interval: four initial
controls leave 252 IDs; the serialized enable/notify/disable path consumes
three per repeat. Cleanup-aware admission permits 83 repeats and leaves three
IDs, from which deregistration succeeds. This is **not** a Mesa cycle count:
setup batches and multiple submissions per frame consume additional IDs.

Do not replace this with a pending-request table that expects a success reply
for every command. `Intel_GPU_GuC_Context_Request` emits HXG FAST_REQUEST for
registration, policy, scheduling mode, work notification and deregistration.
The Intel-authored [HXG ABI documentation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/abi/guc_messages_abi.h.html)
specifies no normal confirmation for fast requests, but permits a failure
response when a request cannot be accepted. A missing success reply therefore
cannot distinguish successful fast-request consumption from a lost command.

The [Linux CT implementation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_ct.c.html)
uses a transport-wide fence counter for both nonblocking and synchronous sends;
the synchronous path adds a pending request before publication and removes it
after response handling. That is not our permanent per-context range scheme.
This inspection used Codebrowser's displayed source (previously identified as
v6.19-rc8-185-g2687c848e578), not a verified v6.16 snapshot.

Before the migration below, CuBit receive routing distinguished two identities:

- `Intel_GPU_Context_Routes.Select_Destination` routes scheduling/deregister
  events by the context ID in the payload, ignoring their CT fence for routing.
- Request failures route by the reserved fence interval, then
  `Context_Lifecycle.Failed_Request` checks whether that fence was attempted.
  Reusing an interval without changing this logic can blame a newer context
  for an old failure. A host-only generation counter cannot disambiguate two
  wire messages containing the same reused 16-bit ID.

Before replacing the range ledger, establish fast-request retirement/error
attribution separately from synchronous pending requests. In particular, a
ring-head advance, a scheduling acknowledgement and a GPU memory completion
marker are different evidence; none should be substituted for another without
an ABI guarantee. Timeout or uncertain publication must not release an ID
under an assumed acknowledgement. Preserve context-event lifetime protection
and bounded outstanding work independently of CT fence wrap. The required
wrap/error-ordering policy remains unresolved; this audit does not authorize
recycling, change wire encodings, or claim hardware validation.

Follow-up inspection of Linux `ct_handle_response`, `ct_handle_hxg`, and
`ct_handle_msg` shows that a reply without a pending request is diagnosed as
unsolicited (`-ENOKEY`); the message is logged and freed. This path does not
establish a per-context failed-request owner or itself reset the transport.
Do not describe a CuBit-wide quarantine policy as copied from Linux.

The CuBit alternative evaluated during this audit was to reserve disjoint wire-ID namespaces
for synchronous requests and fast requests, make fast-request IDs diagnostic
only, and treat any fast-request failure as a transport-wide fault. A delayed
failure could then cause a conservative stop, but could not falsely complete
or release a newer allocation. The implementation update below adopts this
policy. Its review must include every receive consumer (including the
boot log-control roundtrip at fence 42), close submission admission on the
fault, retain backing, and verify that no success/event path treats a fast
ID as completion authority. Context IDs still require independent retirement;
this proposal does not permit their reuse. Tests must cross the 16-bit wrap
with multiple contexts, inject delayed failures before/after wrap, and keep
synchronous replies disjoint. Runtime reset/recovery remains separate work.

### Implemented transport-ID migration (2026-10-03)

The shared driver no longer reserves lifetime fence intervals per context.
`Intel_GPU_GuC_Fast_Fences` assigns diagnostic IDs in `8000..FFFF` across all
context controls, wrapping without granting completion or memory authority.
Guaranteed nonpublication permits retry of the same ID; uncertain publication
stops the stream. The synchronous boot probe remains in the low half at 42.
Scheduling and deregistration events route by payload context ID. A FAST
failure quarantines the entire context table, including delayed failures;
this conservative CuBit policy differs from Linux's unsolicited-response log.

The lifecycle, session and table APIs now carry state and admission facts,
not logical fence budgets. VM holds, authenticated session ownership, drain
evidence, scheduling acknowledgments and backing retirement remain separate
requirements. Context IDs are still retained, not recycled by this migration.

Fresh shared-source hosted builds pass the twelve relevant regression
executables, including notification/control repetition, wait/drain failures,
two-context routing, VM materialization/invalidation failure retention and
FAST-ID wrap/error handling. The lifecycle test completes 131072 full repeat
cycles and deregisters instead of expecting the old 83-repeat ceiling.
The native driver also compiles and links in the private snapshot. These are
hosted regression and native build results, not hardware validation. The NUC
cycle-19 backing-allocation failure is a separate unresolved issue; no claim
is made that this migration fixes it.

Primary implementation references (Intel register/command semantics still
require the generation-appropriate PRM; this audit introduces no new encoding):

- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_submission.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc_ct.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/uc/intel_guc.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_lrc.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_ring.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_ring.h
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gt.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_pci.c

## Page-table reuse and remaining growth work (2026-10-03)

v45 reuses existing leaf tables for eligible authenticated live binds. It retains
the exclusion, completed invalidation and delayed metadata-commit rules used by
unbind. Missing directories still select replacement-table allocation; the
32 MiB backing quota and fixed VM metadata capacity have not been redesigned.
Hosted insertion/removal and host-RAM writer tests pass. Native compilation and
QEMU USB boot/Logs/Console checks pass, but QEMU VGA does not validate this Intel
path. The 256-cycle NUC result remains outstanding.

The subsequent Linux source audit checked the Codebrowser snapshot, not a
verified v6.16 checkout (the version-pinned raw fetch failed):

- `__gen8_ppgtt_alloc` reuses existing children; a missing child comes from the
  stash and is initialized with level-appropriate scratch entries before its
  parent link is installed. It then descends to populate further levels.
  This is initialized-child-before-link, not a requirement to construct the
  entire final subtree before linking any node.
  [gen8_ppgtt.c](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/gen8_ppgtt.c.html)
- `__set_pd_entry` publishes through `write_dma_entry`, which writes and flushes
  the entry. `ppgtt_bind_vma` allocates the VA range when necessary, inserts data
  entries, and ends with a write barrier. These details do not replace CuBit's
  ownership, TLB completion or backing-retirement obligations.
  [intel_ppgtt.c](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_ppgtt.c.html)

CuBit's next growth implementation must distinguish the metadata image root
from the actual stable root retained by `Application_Image`. After replacement,
they need not have the same DMA address. Existing leaf-only insertion avoids
this distinction; new top-level links cannot simply write `Source.Page_DMA(1)`.
They must target the retained root mapping and verify its expected entry.

Before linking a newly allocated table, initialize all 512 entries with the
correct scratch/fault fallback, establish visibility, and retain its exact
backing identity. Preserve existing directories and live leaves. Partial
publication must quarantine and retain every possibly reachable new table;
neither an allocation failure nor a failed flush grants rollback/reuse authority.
These are implementation requirements, not completed native growth support.

### Directory growth components (not enabled in native dispatch)

`VM_Image.Growth` now describes missing directories. Its `Backing` child
validates newly owned pages and resolves parent links through the explicitly
retained hardware root. `Backing.Writer` initializes, flushes and verifies all
new table entries before linking them; its one-shot receipt permits software
image adoption only after publication and confirmed invalidation under exclusion.
No application data leaves are added by this operation. Source metadata capacity
and the current DMA encoder's below-4GiB policy remain unchanged.

`Table_Provenance` uses the growable record store for immutable table IDs and
generation-qualified allocation tickets/offsets. Lookups reauthenticate backing
and reject changed CPU/DMA addresses. Its `IO` child supplies volatile CPU access
through those records. Ticket scans are bounded to 64 records per call; scans
retain reference evidence even when lookup authority has been revoked. Neither
the ledger nor a successful scan authorizes backing release. Alias validation,
serialized lifetime and actual cache/TLB completion remain caller obligations.

Table IDs also carry a ledger generation, captured by the owner rather than
refreshed at lookup time. After grouped retirement has acknowledged every ticket
and swept all references, `Retirement.Reopen` can retain the CPU metadata capacity
while advancing that generation. Failed/incomplete retirement cannot reopen;
generation wrap is rejected. Old-generation installs, lookups, retirement starts
and volatile IO fail even when session, table index and physical address repeat.
The native replacement-table path now calls the bounded `Recycle_Confirmed`
adapter after its exact allocator acknowledgement and retired-image check,
before acknowledging the ticket as reusable. The adapter accepts only a single
ticket with at most64 records; larger/multi-ticket trees need stepped group
retirement. Initial and replacement table accesses use captured ledger
generations. This wiring is compile/link-checked and the adapter is hosted-tested
for256 generations; it has not yet been booted on Intel hardware. It avoids
requiring permanently new metadata for every reuse.

Hosted regression command (output location may be overridden):

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr -XVM_GROWTH_OBJECT_DIR=/tmp/cubit-shared-vm-growth-obj -j2 && /tmp/cubit-shared-vm-growth-obj/vm_growth_tests && /tmp/cubit-shared-vm-growth-obj/vm_growth_backing_tests && /tmp/cubit-shared-vm-growth-obj/vm_growth_writer_tests && /tmp/cubit-shared-vm-growth-obj/table_provenance_tests'
```

Tests cover topology arithmetic, backing rejection, partial-write failure,
ownership loss, gated metadata adoption, real volatile host-memory access, and
stable IDs across metadata extension. Cache flush/TLB evidence is simulated;
this is neither physical Intel validation nor a SPARK proof.

Native integration must replace `Capture_Removal`'s one-ticket/256KiB mapping
reconstruction and connect grouped retirement of every table allocation. Keep
the grown-table path disabled until that ownership connection exists, including
failure quarantine and retention of the original root/scratch allocation.

### Pinned v6.16 retirement audit

The upstream subtree is now available locally from the version-pinned archive
(SHA256 `f8bb490be23623db4603657f62218269d9e167e42983c2cc0f77896e8b104707`).
This replaces the earlier unpinned-source limitation for the following findings:

- In `gt/gen8_ppgtt.c`, `__gen8_ppgtt_clear` restores scratch leaf entries,
  updates usage counts, and can remove an empty directory or a covered subtree.
  `release_pd_entry` in `gt/intel_ppgtt.c` detaches a last-reference child under
  the parent lock; `clear_pd_entry` writes/flushed scratch, clears the software
  child pointer and decrements parent usage. `free_px` drops the table's GEM
  object reference. These calls alone are not a model of physical reuse safety.
- `ppgtt_unbind_vma` clears the range, then calls `vma_invalidate_tlb`. The latter
  records per-GT invalidation sequence requirements rather than performing an
  immediate synchronous hardware flush. `gem/i915_gem_pages.c` checks those
  requirements in `flush_tlb_invalidate` before releasing object pages.
- `i915_vma_resource.c` separately tracks pending unbind ranges in a per-VM
  interval tree, with an unbind fence and retained scatter-gather backing.
  Detaching a mapping, retiring its work, and releasing backing are distinct.

Sources: [gen8_ppgtt.c](https://kernel.googlesource.com/pub/scm/linux/kernel/git/torvalds/linux/+/refs/tags/v6.16/drivers/gpu/drm/i915/gt/gen8_ppgtt.c),
[intel_ppgtt.c](https://kernel.googlesource.com/pub/scm/linux/kernel/git/torvalds/linux/+/refs/tags/v6.16/drivers/gpu/drm/i915/gt/intel_ppgtt.c),
[i915_vma.c](https://kernel.googlesource.com/pub/scm/linux/kernel/git/torvalds/linux/+/refs/tags/v6.16/drivers/gpu/drm/i915/i915_vma.c),
[i915_gem_pages.c](https://kernel.googlesource.com/pub/scm/linux/kernel/git/torvalds/linux/+/refs/tags/v6.16/drivers/gpu/drm/i915/gem/i915_gem_pages.c),
[i915_vma_resource.c](https://kernel.googlesource.com/pub/scm/linux/kernel/git/torvalds/linux/+/refs/tags/v6.16/drivers/gpu/drm/i915/i915_vma_resource.c).

CuBit deliberately retains empty directories today. Reusing them fixes repeated
binds within existing topology, but a workload visiting new address ranges can
still exhaust the bounded image. Dynamic backing alone does not solve that.
Remaining integration requirements are:

1. Per-table ticket provenance for ordinary access and partial-publication
   quarantine, not just one current allocation ticket.
2. Context-wide retirement of every referenced allocation only after confirmed
   GPU/address-space retirement and CPU-grant clearance; supervisor acknowledgement
   must match the exact ticket generation before reuse.
3. Empty-subtree pruning or an explicit bounded cache policy, with a receipt
   separating parent detachment, TLB retirement and backing release. Do not free
   a table merely because its last data leaf was cleared.
4. Scalable image metadata and stable identities across growth/reclamation.
   Existing table IDs are not currently recycled; adding reclamation cannot
   silently reuse them while publication or retirement receipts refer to them.

This audit does not establish that Linux's entire lifetime mechanism has been
ported or proved. CuBit's synchronous invalidation/exclusion gate remains in
place; no change to the v45 hardware artifact follows from these findings.
