with Interfaces;
with Intel_GPU_GGTT_Reservations;
generic
   -- Mandatory owner-local exclusion check: current retained scanout and
   -- platform ranges, correct runtime/upload partition, and lifecycle gates.
   -- Called for search pages and the complete allocation before reservation
   -- and again after backing preparation. Must not mutate/reenter the ledger.
   -- Caller still serializes display/address-space mutation through writes.
   with function Range_Allowed
     (GPU_Start, Bytes : Interfaces.Unsigned_64) return Boolean;
   -- Caller holds exclusive device/GPU-VA ownership for the entire call.
   -- The ledger's aperture is platform-admitted; backing is retained.
   -- These callbacks must be bounded and must not raise exceptions.
   -- Invoked only after retaining this exact GPU extent and checking its PTEs.
   -- ADS pointers must use GPU_Start, not a speculative search result or DMA
   -- address. Initialize and make backing device-visible before returning True.
   -- Do not mutate/reenter Reservations from this callback.
   with procedure Prepare_Buffer
     (GPU_Start, Bytes : Interfaces.Unsigned_64; Success : out Boolean);
   with procedure Read_PTE
     (Index : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_64;
      Success : out Boolean);
   with procedure Write_PTE
     (Index, Value : Interfaces.Unsigned_64; Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
   -- Upload staging normally needs 1 MiB; ADS uses a 16 MiB allocation.
   -- Larger callers must opt in. Publication retains an absolute 16 MiB bound.
   Maximum_Bytes : Interfaces.Unsigned_64 := 1_048_576;
package Intel_GPU_GGTT_Publish is
   type Phase is (Fresh, Consumed_No_Writes, Possibly_Published, Complete);
   type Attempt is limited private;
   function Current (Object : Attempt) return Phase;
   type Search_Outcome is (Not_Searched, Search_Rejected, Search_Read_Failed,
                           Search_Exhausted, Search_Found);
   function Search_Name (Value : Search_Outcome) return String is
     (case Value is
         when Not_Searched => "NOT-SEARCHED",
         when Search_Rejected => "REJECTED",
         when Search_Read_Failed => "READ-FAILED",
         when Search_Exhausted => "EXHAUSTED",
         when Search_Found => "FOUND");
   type Search_Evidence is record
      Outcome : Search_Outcome := Not_Searched;
      Reads, Blocked, Nonzero : Interfaces.Unsigned_64 := 0;
      First_Nonzero_Index, First_Nonzero_Value : Interfaces.Unsigned_64 := 0;
   end record;
   function Search_Detail (Object : Attempt) return Search_Evidence;
   -- One attempt per retained allocation/reservation. No reset/copy/release
   -- operation is provided. Serialization and association with the actual
   -- allocation remain caller obligations; this is not an OS capability.
   type Result is (Rejected, Protected_Range, Reservation_Failed, Occupied, Read_Failed, Prepare_Failed,
                   Quarantined, Published);
   -- Rejected/Protected_Range/Reservation_Failed/Occupied/Read_Failed/Prepare_Failed:
   -- no writes this invocation. Every acquired reservation remains retained,
   -- including pre-write failures. A new Attempt cannot bypass the ledger.
   -- Rejection of a reused attempt does not undo its previous publication.
   -- Quarantined: at least one write may have reached the device. Never free
   -- or remap that backing on this result, including a failed first write.
   -- Published is mapping publication, NOT firmware authentication/execution.
   -- A zero PTE is necessary here but never sufficient to establish ownership.
   -- Firmware may reserve a range with zero entries: aperture admission is
   -- external. Use the same ledger for all publications to this aperture.
   procedure Publish
     (Object : in out Attempt;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      GPU_Start, DMA_Start, Bytes : Interfaces.Unsigned_64;
      Status : out Result);
   -- Search and publish under the same caller-held exclusive ownership.
   -- Search reads candidate PTEs, skipping retained software claims. Failure
   -- consumes the attempt without preparation, writes or invalidation. Callbacks
   -- must not reenter this attempt or mutate the ledger. Selected_Start
   -- records the proposed address even on later failure; only Published
   -- confirms publication. Acquired claims are retained on every outcome.
   procedure Publish_Available
     (Object : in out Attempt;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      DMA_Start, Bytes, Alignment : Interfaces.Unsigned_64;
      Selected_Start : out Interfaces.Unsigned_64;
      Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
      Search : Search_Evidence;
   end record;
end Intel_GPU_GGTT_Publish;
