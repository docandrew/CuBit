# ADL-N combo-PHY restoration boundary

The implementation targets combo PHY A and B only. It does not cover Type-C
PHYs or infer a PHY from the selected display pipe. A is the compensation
master and must be restored before B.

Register definitions and the restore sequence were checked against Linux
v6.16 `drivers/gpu/drm/i915/display/intel_combo_phy.c`,
`intel_combo_phy_regs.h`, and `drivers/gpu/drm/i915/i915_reg.h`.
`intel_display_power_well.c:gen9_disable_dc_states` explains why this is
necessary: DMC retains port A context, but port B context can be lost across
DC transitions. Merely observing the DC enable bits clear is insufficient.

The pure planner checks all five supported process/voltage encodings. Unknown
encodings fail closed instead of using Linux's warning-and-default fallback.
It reads lane zero but emits group writes for TX and PCS. It does not change
lane routing or lane power masks. An already-correct PHY requires no writes.

The executor is boot-only and admits one attempt per instance. Its callback
contract requires exclusive ownership, retained power and DC-off. It preflights
both PHYs, rechecks each baseline, restores A then B, completes writes and reads
back the ready-state predicates. Failure retains resources for explicit recovery;
it does not undo partial writes, retry, release power, or reset the GPU.

`intel_gpu_native_combo_state` performs real volatile loads through the existing
BAR0 alias and is connected to native startup. Its `Power_Held` callback
must reflect the relevant retained hardware access guarantee, not merely a
request bit. Two matching reads are not an atomic snapshot. Do not instantiate
it with an unconditional success callback or assume pipe power implies PHY power.

Hosted tests use anonymous mappings at the expected virtual addresses, not a GPU:

- `combo_phy.gpr`: independent offsets/table expectations, all selector encodings,
  preservation, ordering, and invalid ready-state bits. SPARK proves software
  bounds, termination and the count contract, not electrical behavior.
- `combo_restore.gpr`: 52 callback fault positions, successful ordering, invalid
  second-PHY preflight, changed baseline, failed verification, no-op ready state,
  cleanup and no replay. The executor itself has no SPARK proof claim.
- `native_combo_state.gpr`: actual adapter loads for A/B, all first-pass sentinel
  positions, every post-entry power-check failure, changing samples and recovery.

Native power/mapping bindings and DC-exit integration are implemented. Real NUC
validation of restoration, visibility and firmware interaction is still required.
No test here establishes GPU firmware execution or hardware-accelerated 3D.

The native restore adapter now binds the ordered executor to volatile writes
through `Write_Address`, the native snapshot reader, and a same-BAR posting read
after an x86 memory fence. It checks retained power, disabled DC and complete
mapping availability throughout the operation. Those checks require trusted
native callbacks; they do not replace caller serialization or firmware handoff.
It is reached through native DC startup after successful engine reset.
`native_combo_restore.gpr` runs the adapter's
actual stores on anonymous host pages with independent emulated group-write/read
aliases; passing that test does not establish hardware visibility or PHY timing.

`intel_gpu_phy_pages` reserves three distinct mapping pages (MISC 0x64000,
PHY A 0x162000, PHY B 0x6c000), slots 29..31 and request label 0x0234 for the
broker binding. These are page-granularity grants, not per-register
capabilities. Its driver-side write selector admits exactly 17 restore offsets;
the hosted test checks all 2 MiB of byte offsets, including unaligned addresses.
SPARK proves selected addresses stay aligned and within the three-page alias.
The broker now recognizes the fixed-page request after checking the designated
Intel process, badge, retained display owner, frozen PCI claim, D0 state, exact
PCI identity and BAR-relative address. Each page is granted once. Grants are
page-granularity authority; the driver's register allowlist is not enforced by
the kernel within an already-granted page.

The driver-side `intel_gpu_phy_mapping` IPC routine now exists. Its hosted
fixture verifies all three physical-page/virtual-alias pairs, exact request
shape and tokens, bounded waits, startup deferral and malformed/failed replies.
Partial mappings remain retained and do not set Ready; an attempt is never
replayed. The fixture does not simulate kernel authority enforcement. Native
startup requests the mappings and captures read-only A/B snapshots, reporting
the process/voltage selector and planner outcome. The later DC transition invokes
restoration after engine reset; compilation/QEMU smoke cannot validate the NUC's registers.

## DC transition composition

`intel_gpu_dc_transition` wraps the existing DC-exit sequence with a captured,
settled baseline, guarded register callbacks and mandatory restoration. It checks
clock/buffer preservation both before and after the PHY callback. There is no
fallback success path for a missing baseline, lost power or failed restoration.
`dc_transition.gpr` exercises these composed paths with hosted callbacks; it does
not itself authorize hardware writes.

`intel_gpu_native_dc` now binds the composition to the native clock/buffer reader,
native PHY executor and DC_STATE_EN aliases. Its write guard allows only clearing
the documented enable/status bits, preserves unrelated latch bits, and rejects
all-ones reads. `Held` is set only after the whole composed transition succeeds.
Native startup now invokes it after successful engine reset, logging both the
beginning and outcome. A failed transition never publishes a DC-off reference;
resources remain retained and no automatic replay occurs. `native_dc.gpr` tests nine hosted
scenarios using actual loads/stores on anonymous register-alias fixtures; PHYs
start in a valid retained state for that test, while `native_combo_restore.gpr`
separately covers the nontrivial 17-write restore. Firmware synchronization and
hardware timing remain native-validation obligations, not fixture guarantees.

Further upstream ordering evidence: v6.16 `intel_display_power.c` initializes
combo PHYs before enabling PW1 in `icl_display_core_init`. Xe-LPD's power-map
comments assign DDI A/B to PG1 and DBUF registers to PW0. Thus PHY initialization
and the post-DC-transition restoration are distinct phases. Do not infer that
an arbitrary display pipe's reference alone grants all PHY or AUX accesses.
