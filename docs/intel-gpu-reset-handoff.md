# ADL-N reset and GPU-address ownership

## Firmware address reservation audit (next integration)

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
