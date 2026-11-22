generic
   with function Exclusive return Boolean;
   -- Trusted owner holds GPU drain, disabled scheduling, submission/reset
   -- exclusion and table/data references throughout the transaction, including
   -- every yield between Start, Step, invalidation and Commit.
   with procedure Write_Leaf
     (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
package Intel_GPU_VM_Image.Insertion is
   subtype Controller is Insertion_Receipt;
   procedure Begin_Prepare
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access; Accepted : out Boolean);
   function Preparing (State : Controller) return Boolean;
   function Captured (State : Controller) return Boolean;
   generic
      with function Data_Page (Ordinal : Positive) return Unsigned_64;
   procedure Capture_Step
     (State : in out Controller; Object : Image; Accepted : out Boolean);
   procedure Finish_Prepare
     (State : in out Controller; Object : Image; Accepted : out Boolean);
   -- Begin retains transaction identity, not caller storage. Each capture step
   -- reads at most32 pages into owned receipt storage, rechecking source/owner
   -- after every callback. No hardware writes before Finish_Prepare validates
   -- all retained pages. Each Finish_Prepare call performs at most32 validation
   -- actions (one descriptor, route, leaf or cache-alias check each); Accepted
   -- means the step succeeded, not publication readiness. Continue while
   -- Preparing, then require Publishing before performing hardware writes.
   generic
      with function Data_Page (Ordinal : Positive) return Unsigned_64;
   function Can_Reuse_From_Pages
     (State : Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access) return Boolean;
   -- Metadata-only hint. Resolver/source must remain stable during this call;
   -- no authority or input storage survives it. Start repeats validation.
   generic
      with function Data_Page (Ordinal : Positive) return Unsigned_64;
   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   -- Read each trusted retained-backing page once into existing growable
   -- receipt storage. Validate those exact words before any hardware write.
   -- Recheck exclusion/source after callbacks; no borrowed array survives.
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
   procedure Start
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   procedure Step (State : in out Controller; Object : Image);
   function Publication_Table (State : Controller; Object : Image) return Natural;
   function Publication_Matches
     (State : Controller; Object : Image; Table_DMA : Unsigned_64;
      Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64) return Boolean;
   -- Exact current retained write, not merely a validated table ordinal.
   -- Validated descriptor ordinal only during this receipt's write callback,
   -- while source identity/exclusion still match. Zero outside that scope.
   -- Routing hint only: caller must authenticate its exact retained table
   -- allocation and match the callback DMA before writing through a CPU map.
   function Publishing (State : Controller) return Boolean;
   function Published (State : Controller) return Boolean;
   -- Start validates and retains exact encoded leaves without hardware writes.
   -- Each Step performs at most32 route descriptor comparisons and invokes at
   -- most one compare/write callback. A route search may yield without writing.
   -- Ownership and source
   -- identity are rechecked across yields and after callbacks. Published only
   -- permits invalidation, never submission. A premature Commit consumes the
   -- attempt; partial or uncertain writes cannot be retried. Start validation
   -- and Commit metadata work still scale with the requested range.
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean);
   procedure Begin_Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean);
   procedure Commit_Step
     (State : in out Controller; Object : in out Image;
      Complete : out Boolean);
   function Committing (State : Controller) return Boolean;
   -- At most 32 descriptor comparisons and one leaf adoption per Commit_Step.
   -- The owner must preserve
   -- exclusion throughout; the image is invalid/hidden until the last step.
   -- Neither partial metadata nor a completed hardware
   -- publication authorizes submissions. Loss of identity/exclusion invalidates
   -- the image and quarantines the receipt. The descriptor walk resumes across
   -- turns and is reused for adjacent leaves within the same PT region.
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
