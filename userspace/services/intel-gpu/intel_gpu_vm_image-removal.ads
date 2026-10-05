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
   generic
      with function Expected_Page (Ordinal : Positive) return Unsigned_64;
   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Accepted : out Boolean);
   -- Trusted retained-backing resolver; zero rejects. No array/callback is
   -- retained. Recheck exclusion and source identity after every callback;
   -- validate the complete range before enabling any hardware publication.
   procedure Start
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean);
   procedure Step (State : in out Controller; Object : Image);
   function Publishing (State : Controller) return Boolean;
   function Published (State : Controller) return Boolean;
   -- Start validates the entire range without hardware writes. Expected need
   -- not survive Start: the exact sealed source stays immutable through Commit.
   -- Step performs at most one compare/write callback; owner, root and epoch
   -- are checked before/after it. Premature Commit or callback reentry consumes
   -- the attempt, retaining metadata/backing. Start/Commit still walk the range.
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
      Active, Executing : Boolean := False;
      Cursor : Natural := 0;
      Root, Epoch, First : Unsigned_64 := 0;
      Pages : Natural := 0;
   end record;
end Intel_GPU_VM_Image.Removal;
