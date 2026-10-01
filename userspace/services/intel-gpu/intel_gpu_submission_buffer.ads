with Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_VM_Image;
generic
   with function Owner_Ready return Boolean;
package Intel_GPU_Submission_Buffer is
   package VM is new Intel_GPU_VM_Image (4);
   type Buffer_State is limited private;
   -- Single serialized startup caller, before device publication. Never retry
   -- after a partial write or flush failure; backing remains retained.
   -- Allocation identifies an exclusive retained backing extent, obtained
   -- from the trusted buffer pool, not an application. Its size covers the
   -- whole image, including private page tables and completion storage.
   -- GGTT_Start/Bytes must be the reserved context+ring extent provided by
   -- the existing publication callback, not an arbitrary caller-chosen VA.
   procedure Initialize
     (Object : in out Buffer_State;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Interfaces.Unsigned_64;
      Success : out Boolean);
   function Initialized_GPU_Start (Object : Buffer_State) return Interfaces.Unsigned_64;
   generic
      with function Completed_Read_Owner return Boolean;
   function Completed_Pixel_View (Object : Buffer_State)
      return Intel_GPU_Buffer_Reply.Backing;
   -- Bootstrap-only exact16KiB linear64x64 BGRA target, excluding ALL command,
   -- context, completion, shader and table pages. Caller establishes completed
   -- rendering/cache visibility and acknowledged disable with no future writer.
   -- This is a retained backing view, NOT a grant or authorization to forward.
   -- Grant owner must revalidate lifetime and export read-only; parent backing
   -- remains retained until every reader has retired. No copying/allocation.
   type Root_Mapping is record
      CPU, DMA : Interfaces.Unsigned_64 := 0;
   end record;
   function Retained_Boot_Root (Object : Buffer_State) return Root_Mapping;
   -- Receipt for successful bootstrap preparation only, not live ownership
   -- or permission to write. External-VM contexts return zero: their owner
   -- retains the root through Application_Image instead.
   procedure Prepare_Boot_Update
     (Object : Buffer_State; Candidate : in out VM.Image;
      Backing : VM.Backing_Pages; Success : out Boolean);
   -- Trusted serialized caller retains/authorizes fresh backing. Copies the
   -- retained sealed bootstrap VM into an offline candidate only. No memory
   -- writes, root replacement, quiescence, invalidation or GPU resume here.
   -- Failure leaves Candidate unusable by this call's contract; even a sealed
   -- candidate is not publication authority. Live updates need separate gates.
   -- Application path: VM root has already been materialized separately.
   -- Owner_Ready must cover its session and all retained context/VM backing.
   -- All VM pages must be disjoint from Allocation (not only the root).
   -- No bootstrap tables or marker batch are created; zero root rejects.
   procedure Initialize_For_VM
     (Object : in out Buffer_State;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes, Root_DMA : Interfaces.Unsigned_64;
      Success : out Boolean);
private
   type Buffer_State is limited record
      Attempted : Boolean := False;
      GPU_Address : Interfaces.Unsigned_64 := 0;
      Boot_VM : VM.Image;
      Boot_Root : Root_Mapping;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      Update_Attempted, Update_Failed : Boolean := False;
   end record;
end Intel_GPU_Submission_Buffer;
