with Interfaces;
generic
   -- Caller holds exclusive device/GPU-VA ownership for the entire call.
   -- The range is already reserved, excludes scanout, and backing is retained.
   -- These callbacks must be bounded and must not raise exceptions.
   with procedure Prepare_Buffer (Success : out Boolean);
   with procedure Read_PTE
     (Index : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_64;
      Success : out Boolean);
   with procedure Write_PTE
     (Index, Value : Interfaces.Unsigned_64; Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
package Intel_GPU_GGTT_Publish is
   type Phase is (Fresh, Consumed_No_Writes, Possibly_Published, Complete);
   type Attempt is limited private;
   function Current (Object : Attempt) return Phase;
   -- One attempt per retained allocation/reservation. No reset/copy/release
   -- operation is provided. Serialization and association with the actual
   -- allocation remain caller obligations; this is not an OS capability.
   type Result is (Rejected, Occupied, Read_Failed, Prepare_Failed,
                   Quarantined, Published);
   -- Rejected/Occupied/Read_Failed/Prepare_Failed: no writes this invocation.
   -- Rejection of a reused attempt does not undo its previous publication.
   -- Quarantined: at least one write may have reached the device. Never free
   -- or remap that backing on this result, including a failed first write.
   -- Published is mapping publication, NOT firmware authentication/execution.
   -- A zero PTE is necessary here but never sufficient to establish ownership.
   -- Firmware may reserve a range with zero entries: reservation is external.
   procedure Publish
     (Object : in out Attempt;
      Table_Bytes, GPU_Start, DMA_Start, Bytes : Interfaces.Unsigned_64;
      Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
   end record;
end Intel_GPU_GGTT_Publish;
