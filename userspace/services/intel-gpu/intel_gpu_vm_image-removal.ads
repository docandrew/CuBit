generic
   with function Exclusive return Boolean;
   -- Authenticated VM owner holds completed GPU drain, disabled scheduling,
   -- submission/reset exclusion, and all table/data backing references.
   with procedure Write_Leaf
     (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   -- Compare the retained hardware leaf with Expected, then publish Replacement
   -- with the platform's visibility rules. No allocator or event reentrancy.
   with procedure Invalidate (Success : out Boolean);
   -- Return success only after completed hardware translation invalidation.
package Intel_GPU_VM_Image.Removal is
   type Controller is limited private;
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean);
   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean);
   -- Split form for VM_Update's Publish/Invalidate/Resume stages. Publish
   -- leaves metadata untouched and admission poisoned until Commit. The
   -- trusted coordinator supplies completed invalidation, never a client.
   -- A failed Commit consumes the pending receipt; it cannot be retried.
   procedure Execute
     (State : in out Controller; Object : in out Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean);
   -- Allocation-free removal from retained leaf tables. Entire range is
   -- validated before the first write. No directory changes or table frees.
   -- Object/Expected must remain stable through callbacks (serialized owner).
   -- Software metadata commits only after publication AND invalidation. Any
   -- failure after publication starts poisons State, retains metadata/backing,
   -- and requires the caller to quarantine the VM: never resume or retry it.
   -- Success does not retire CPU aliases, other GPU aliases or backing leases.
   -- Caller must also maintain its hardware-table ownership/retirement records.
   function Failed (State : Controller) return Boolean;
private
   type Controller is limited record
      Poisoned : Boolean := False;
      Pending : Boolean := False;
      Root, Epoch, First : Unsigned_64 := 0;
      Pages : Natural := 0;
   end record;
end Intel_GPU_VM_Image.Removal;
