with Intel_GPU_GGTT;
generic
   -- Same hardware obligations as Intel_GPU_GGTT_Retire: exclusive cleanup
   -- authority, all users drained, no future publication, non-scanout range,
   -- separately retained scratch backing and completed platform invalidation.
   -- The owner serializes this entire call with every ledger operation.
   -- Callbacks must not raise, reenter, mutate the ledger or resume producers.
   with function Gate (First, Bytes : Unsigned_64) return Boolean;
   with procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                           OK : out Boolean);
   with procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean);
   with procedure Invalidate_And_Wait (OK : out Boolean);
   with function Resolve_Page (Base, Offset : Unsigned_64) return Unsigned_64
     is Intel_GPU_GGTT.Linear_Page;
package Intel_GPU_GGTT_Reservations.Reclamation with SPARK_Mode => Off is
   type Attempt is limited private;
   type Result is (Rejected, Quarantined, Released);
   -- Released means ONLY the exact GGTT address reservation is reusable.
   -- Backing, other GPU aliases, CPU grants and supervisor tickets are NOT
   -- released. No public bookkeeping-only release exists. Failed/uncertain
   -- transactions retain the claim. Attempts are never reset/replayed, even
   -- if a later allocation obtains this same address (ABA).
   procedure Execute
     (Object : in out Attempt; Book : in out Ledger;
      First, DMA_Base, Bytes, Scratch_DMA : Unsigned_64; Status : out Result);
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
end Intel_GPU_GGTT_Reservations.Reclamation;
