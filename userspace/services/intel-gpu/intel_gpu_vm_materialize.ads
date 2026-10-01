with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
   with function Owner_Ready return Boolean;
   with function Flush_Page (CPU : Unsigned_64) return Boolean;
package Intel_GPU_VM_Materialize is
   type Page_Mapping is record
      CPU, DMA : Unsigned_64 := 0;
   end record;
   type Mappings is array (VM.Page_Number) of Page_Mapping;
   type State is limited private;
   -- Trusted owner supplies exclusive retained CPU mappings of these exact
   -- DMA pages; numeric checks cannot prove that relationship or authority.
   -- Pages must not alias the source image or another writer/device. Caller
   -- serializes this entire operation. Callbacks are bounded and nonraising.
   -- Flush_Page must complete cache visibility/ordering before returning True.
   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Mappings;
      Root : out Unsigned_64; Success : out Boolean);
   procedure Publish_Update
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Mappings; Stable_Root : Page_Mapping;
      Success : out Boolean);
   -- One attempt, including rejected preflight. Owner must hold submission
   -- exclusion, completed GPU flush/drain and acknowledged disable for EVERY
   -- context using this root throughout the call. Previous is the last
   -- published image; Stable_Root is its retained hardware root (which may
   -- differ from Previous.Root_DMA after an earlier update).
   -- Verify old root, prepare/flush/read back fresh candidate backing, verify
   -- old root again, then replace/flush/read back the stable root entries.
   -- Candidate backing must be fresh and disjoint from Previous and the root;
   -- CPU/DMA mapping authority and exclusion remain caller obligations.
   -- This DOES modify a hardware-referenced root. Success is not permission
   -- to resume: translation invalidation must follow. On any failure retain
   -- both generations and quarantine; partial root writes are not rolled back.
   -- One attempt. Writes EVERY word of used pages (including zero holes),
   -- flushes, then volatile-verifies backing. Root is zero on any failure;
   -- partial backing remains retained and cannot be retried through this state.
   -- Success is not hardware publication: the owning context must still check
   -- its lifetime before publishing the root and retain backing until retired.
private
   type State is limited record
      Attempted : Boolean := False;
   end record;
end Intel_GPU_VM_Materialize;
