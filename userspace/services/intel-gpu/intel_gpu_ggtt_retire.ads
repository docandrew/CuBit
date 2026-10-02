with Interfaces; use Interfaces;
with Intel_GPU_GGTT;
with Intel_GPU_GGTT_Reservations;
generic
   -- Exclusive current device/VA ownership, range not used by scanout or
   -- firmware, every consumer deregistered/drained and no future publication.
   -- Scratch backing is initialized, device-visible, independently retained.
   -- These callbacks are bounded/nonraising and must not reenter/mutate the
   -- ledger. They supply hardware facts, not a substitute for establishing them.
   with function Gate (First, Bytes : Unsigned_64) return Boolean;
   with procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                           OK : out Boolean);
   with procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean);
   -- True only after the platform's required translation/order completion,
   -- NOT merely successful issuance of Intel_GPU_GGTT_Invalidate.Issue.
   with procedure Invalidate_And_Wait (OK : out Boolean);
   with function Resolve_Page (Base, Offset : Unsigned_64) return Unsigned_64
     is Intel_GPU_GGTT.Linear_Page;
package Intel_GPU_GGTT_Retire is
   type Attempt is limited private;
   type Result is (Rejected, Quarantined, Detached);
   -- One exact, retained claim, at most16MiB. Preflight every page before any
   -- write. Replacement uses a separate scratch page, never zero/unowned RAM.
   -- Linux v6.16 intel_ggtt.c gen8_ggtt_clear_range provides the scratch-remap
   -- pattern; platform quiescence/invalidation remain explicit obligations.
   -- No rollback/retry after any attempt. Even Detached retains the claim and
   -- backing: other mappings/CPU views/DMA aliases must be retired separately.
   procedure Execute
     (Object : in out Attempt; Ledger : Intel_GPU_GGTT_Reservations.Ledger;
      First, DMA_Base, Bytes, Scratch_DMA : Unsigned_64; Status : out Result);
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
end Intel_GPU_GGTT_Retire;
