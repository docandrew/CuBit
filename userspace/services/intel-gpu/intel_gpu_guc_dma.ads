with Interfaces;
-- GuC CSS+code transfer only, not authentication or firmware readiness.
-- Caller owns the device exclusively, holds forcewake, has completed reset,
-- configured WOPCM/GuC preparation and RSA, and published a retained, coherent
-- GGTT mapping. Source is GPU virtual, NEVER a CPU virtual or physical address.
generic
   with function Read32 (Offset : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32);
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_GuC_DMA is
   type Phase is (Fresh, Consumed, Quarantined, Transferred);
   type Attempt is limited private;
   function Current (Object : Attempt) return Phase;
   type Result is (Rejected, Busy, Invalid_MMIO, Invalid_Clock,
                   Timed_Out, Cleanup_Failed, Complete);
   -- Nonraising, bounded, ordered MMIO callbacks. Now is monotonic us; Last
   -- denotes unavailable. WOPCM_Bytes is the admitted GuC region size, not a
   -- guessed capacity. Bytes includes CSS+code only, excluding RSA.
   -- Initial bring-up admits page-aligned GGTT source below 4GiB. Range checks
   -- do not establish ownership. No retry/release after writes, even on timeout.
   procedure Execute
     (Object : in out Attempt;
      Source, Bytes, WOPCM_Bytes : Interfaces.Unsigned_64;
      Poll_Limit : Positive; Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
   end record;
end Intel_GPU_GuC_DMA;
