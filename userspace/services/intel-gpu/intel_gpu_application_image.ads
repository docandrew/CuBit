with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_Submission_Buffer;
with Intel_GPU_Buffer_Reply;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
   with function Owner_Ready return Boolean;
   with function Flush_Page (CPU : Unsigned_64) return Boolean;
package Intel_GPU_Application_Image is
   package Tables is new Intel_GPU_VM_Materialize (VM, Owner_Ready, Flush_Page);
   type State is limited private;
   -- Trusted, serialized preparation only: no GGTT publication or GuC
   -- registration. Owner covers session, reserved GGTT range and all backing.
   -- Backing must identify the actual retained CPU/DMA mappings; numerical
   -- disjointness cannot establish authority or detect hidden aliases.
   -- Reserved table and mapped data DMA pages must be disjoint from this
   -- private allocation; checked before writes. One attempt; on failure
   -- retain all potentially written pages.
   -- GPU_Start remains zero until BOTH images pass visibility/readback checks
   -- and the final ownership check. This is not permission to publish them.
   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64; Success : out Boolean;
      Scratch : Tables.Scratch_Mappings := [others => (0, 0)]);
   function GPU_Start (Object : State) return Unsigned_64;
   function Retained_Root (Object : State) return Tables.Page_Mapping;
   -- Exact root backing encoded into the prepared context, retained across
   -- subsequent VM generations. Zero until both images pass preparation.
   -- This receipt is not current ownership, GPU publication, or permission
   -- to modify the root; the update coordinator must establish those gates.
private
   function Allocation_Disjoint
     (Object : VM.Image; Allocation : Intel_GPU_Buffer_Reply.Backing) return Boolean;
   function Overlap (Page, First, Bytes : Unsigned_64) return Boolean;
   package Contexts is new Intel_GPU_Submission_Buffer (Owner_Ready);
   type State is limited record
      Attempted : Boolean := False;
      Publication_Attempted : Boolean := False;
      Retirement_Attempted : Boolean := False;
      Address_Released, Receipt_Forgotten : Boolean := False;
      Published : Unsigned_64 := 0;
      Prepared : Unsigned_64 := 0;
      Root : Tables.Page_Mapping := (0, 0);
      Scratch : Tables.Scratch_Mappings := [others => (0, 0)];
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      Update_Failed, Updating : Boolean := False;
      Table_State : Tables.State;
      Context_State : Contexts.Buffer_State;
   end record;
end Intel_GPU_Application_Image;
