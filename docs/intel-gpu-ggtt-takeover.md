# GGTT takeover: inherited mappings are not an allocator

## Current hardware evidence

NUC reports complete scanout inventory with one range and no overlap with the
upload interval FEE00000..FFFFF000. The zero-entry search reports EXHAUSTED,
4480 reads, 0 blocked candidates and 4480 nonzero observations. The first
nonzero index is FEE00; the user confirmed its full 64-bit value as
AB25AB25AB25AB25. This is not a claim that every entry was read: unsuccessful
candidate searches can stop at their first nonzero entry. A repeated nonzero
PTE is not proof of disposable scratch or of a valid firmware mapping.

## Read-path audit and next hardware discriminator

The current ADLN path decodes GGMS bits 7:6 into 2/4/8 MiB table sizes. Both
grant paths select BAR0 + 8 MiB. The writable mapping validates the granted
physical base and extent against that expectation before mapping disjoint
2 MiB chunks. Its virtual base is 64000000; the read-only inspection base is
60400000. Native reads bounds-check the index and retain ownership checks.
Index FEE00 addresses byte offset 7F7000, inside the reported 8 MiB table;
it does not cross a chunk boundary or the table end. These source checks do
not prove the runtime physical mappings are correct.

No matching AB25 fill was found in the kernel, runtime or service source
initialization paths inspected. An unrelated PHY programming constant ends
in AB25; that is not evidence of a GGTT write. Native disassembly of the
private probe driver uses an indexed 64-bit load, consistent with upstream
`gen8_get_pte` using `readq`. Access width alone is not an established cause.

The prepared read-only hardware probe compares index zero and the first
nonzero index using high/low/high 32-bit reads, a 64-bit read, and the
read-only alias. Await those results before selecting a fix:

- Different aliases implicate mapping/access state, not free-space policy.
- Different widths or changed high halves require an access/stability audit.
- Matching values still do not establish ownership or permit replacement.

The probe neither writes PTEs nor relaxes the exact-zero publication policy.

## Pinned upstream evidence

Linux v6.16 `gt/intel_ggtt.c`, `init_ggtt` and `ggtt_reserve_guc_top`, uses
software reservations for special regions and clears allocator holes. The
upper firmware-upload region is reserved before that clearing loop; it is
not guaranteed to contain zero entries afterward. The end guard is cleared
separately. `gen8_ggtt_clear_range` writes the driver's encoded scratch PTE,
not zero. Thus Linux's allocation ownership is not derived from PTE contents.

`display/intel_plane_initial.c`, `initial_plane_vma`, preserves the original
framebuffer mapping while establishing its replacement mapping. It explicitly
reserves the original extent to prevent overlap during relocation; if the new
placement fails it can pin at the original location. CuBit currently retains
firmware scanout rather than implementing this relocation path.

Sources inspected directly (not copied into the driver):
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_ggtt.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_plane_initial.c

## CuBit implementation requirements

Current reset-path audit: `Intel_GPU_Handoff.Execute` stops every engine in
the decoded inventory, prepares each, invokes the full-GT reset and settles,
then cancels preparation on every selected engine. Only complete reset plus
successful cleanup transitions to Reset_Held. `Intel_GPU_Native_Reset` retains
forcewake and uses a one-shot attempt. This establishes the intended sequencing
for inventoried engines; it does not independently establish that all firmware
consumers/platform reservations have been identified or that GGTT entries were
cleared. Native register access correctness and the hardware reset semantics
remain external to the sequencing tests.

The existing search is deliberately a conservative publication path, not a
completed GGTT takeover. Do not make it accept arbitrary nonzero PTEs, a guessed
scratch pattern, or merely a cleared present bit.

A separate takeover operation must establish and retain:

1. Exclusive device/address-space authority and the admitted GGTT mapping.
2. Quiesced old execution/firmware consumers, with reset success and engine
   scope checked; reset is not assumed to clear GGTT or disable display.
3. Stable, complete inventory of every present scanout/cursor and all other
   platform-reserved consumers. Unknown or unsupported enabled state rejects
   takeover. Power-held and model recognition alone are insufficient.
4. A reservation ledger covering retained display extents, low bias, firmware
   upload space and guard; acquisition is serialized against modesetting and
   publication. The guard requires a deliberate mapping policy, not omission.
5. Bounded replacement only inside an explicitly acquired range. Revalidate
   relevant inventory/owner state before the first write. Retain backing and
   mark partial publication quarantined before invoking any write callback.
6. Posted-write completion and the required GGTT translation invalidation,
   followed by a mapping/publication result distinct from firmware execution.

Reclamation must not free physical firmware/framebuffer memory merely because
its GGTT alias is replaced. Address-space ownership and physical-page ownership
are separate. Cross-device/epoch lifetime rules remain required for eventual
multi-GPU support.

Before native integration, test changed inventory, owner loss, reserved-range
overlap, read/write/invalidation failure, and repeated calls after partial
publication. A host fixture proves callback sequencing only; the NUC must
verify preserved scanout and then authenticated firmware/engine progress.
