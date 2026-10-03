generic
   with function Exclusive return Boolean;
   -- Trusted owner holds GPU drain, disabled scheduling, submission/reset
   -- exclusion and table/data references throughout this synchronous call.
   with procedure Write_Leaf
     (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
package Intel_GPU_VM_Image.Insertion is
   subtype Controller is Insertion_Receipt;
   function Range_Reusable
     (Object : Image; GPU, Bytes : Unsigned_64) return Boolean;
   -- Dispatch hint only: existing directories and logically empty leaves.
   -- Does not establish backing, owner, cache policy or publication authority.
   function Can_Reuse
     (State : Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access) return Boolean;
   -- Metadata-only eligibility, without callbacks or publication authority.
   -- Publish repeats this check and additionally requires Exclusive.
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean);
   -- Split VM_Update stages. Commit consumes the pending receipt even on
   -- failure. Invalidation_Completed is trusted coordinator evidence, not IPC.
   -- A receipt owns exact encoded leaves: no borrowed Data pointer/hash.
   -- One workspace per serialized updater, bounded by VM leaf capacity;
   -- keep it out of the service stack for large VM capacities.
   procedure Execute
     (State : in out Controller; Object : in out Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   -- Reuse existing empty leaf entries only: no allocation or directory writes.
   -- Missing directories reject before any external effect. Caller may then
   -- use the separately validated growth path, never after Failed becomes true.
   -- Compare/write callback establishes hardware visibility; Invalidate must
   -- confirm hardware completion. Object/Data remain immutable to callbacks.
   -- Serialized, non-reentrant owner only. Metadata commits after invalidation;
   -- any failure from first write onward permanently poisons this controller,
   -- retains backing and requires VM quarantine. No rollback/reuse authority.
   function Failed (State : Controller) return Boolean;
end Intel_GPU_VM_Image.Insertion;
