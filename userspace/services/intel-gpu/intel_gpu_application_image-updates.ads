with Intel_GPU_ADLN_PPGTT;
generic
   with function Exclusive return Boolean;
   -- Owner holds submission exclusion, GPU flush completion, scheduling
   -- disable acknowledgments, reset serialization and required forcewake.
   -- This predicate checks those conditions; it does not establish them.
package Intel_GPU_Application_Image.Updates is
   procedure Insert_Leaf
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   -- Empty logical leaf only; Expected is its hardware scratch/fault fallback.
   -- Replacement must be a supported data PTE, not table or scratch backing.
   -- Caller authenticates data ownership and preflights the whole range and
   -- cache aliases. Same exclusion and invalidation obligations as removal.
   procedure Remove_Leaf
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   -- Exclusive in-place removal only. Validates retained table mapping and
   -- expected leaf, writes scratch/fault fallback, flushes and reads back.
   -- Caller still MUST complete TLB invalidation before metadata commit/reuse.
   procedure Publish_Tables
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Tables.Mappings; Success : out Boolean);
   -- Publication stage of VM_Update ONLY. Uses the root/context backing
   -- retained by Prepare, never a caller-supplied replacement root. Caller
   -- retains all generations, authenticates their session and expected epoch,
   -- and invalidates translations before resuming any context. Previous is
   -- the most recently published image; candidate CPU/DMA mappings are trusted.
   -- No allocation, reclamation, saved-context rewrite or implicit resume.
   -- Any failure permanently rejects further updates to this context object.
   function Failed (Object : State) return Boolean;
end Intel_GPU_Application_Image.Updates;
