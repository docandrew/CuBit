with Interfaces;
generic
   -- ADL-N main GT only. Caller owns quiesced engines, retained forcewake,
   -- and control MMIO. Must run before new GPU buffers/contexts are active.
   -- Serialized/non-reentrant callbacks. Not a live cache-policy migration.
   with function Owner_Ready return Boolean;
   with function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_ADLN_PAT is
   type Attempt is limited private;
   type Result is (Rejected, Ownership_Lost, Read_Failed, Write_Failed,
                   Readback_Failed, Ready);
   procedure Configure (Object : in out Attempt; Status : out Result);
   function Last_Index (Object : Attempt) return Natural;
   function Last_Raw (Object : Attempt) return Interfaces.Unsigned_32;
   -- Partial failure is retained, never rolled back/retried automatically.
   -- Ready means register readback matched, not a proof of DMA coherency.
private
   type Attempt is limited record
      Started : Boolean := False;
      Index : Natural range 0 .. 7 := 0;
      Raw : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_ADLN_PAT;
