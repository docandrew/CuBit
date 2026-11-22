with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_Table_Mappings;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
   with function Owner_Ready return Boolean;
   with function Flush_Page (CPU : Unsigned_64) return Boolean;
package Intel_GPU_VM_Materialize is
   subtype Page_Mapping is Intel_GPU_Table_Mappings.Page_Mapping;
   subtype Mapping_View is Intel_GPU_Table_Mappings.Mappings;
   subtype Mappings is Mapping_View (VM.Page_Number);
   type Scratch_Mappings is array (Intel_GPU_PPGTT_Scratch.Level) of Page_Mapping;
   type State is limited private;
   -- Trusted owner supplies exclusive retained CPU mappings of these exact
   -- DMA pages; numeric checks cannot prove that relationship or authority.
   -- Pages must not alias the source image or another writer/device. Caller
   -- serializes this entire operation. Callbacks are bounded and nonraising.
   -- Flush_Page must complete cache visibility/ordering before returning True.
   -- Backing must start at ordinal1 and cover every used page. It need not
   -- include unused quota entries. Short/shifted views reject before writes.
   -- Optional scratch backing is exclusive and not yet GPU-referenced.
   -- Data is zeroed; fallback tables are filled, flushed and verified before
   -- normal tables. Never call this to reinitialize a live VM's scratch.
   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Mapping_View;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]);
   generic
      with function Lookup (Ordinal : Positive) return Page_Mapping;
   procedure Prepare_From_Mappings
     (Object : in out State; Source : VM.Image; Mapping_Count : Natural;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]);
   -- No caller-sized mapping array. Lookup is a trusted, bounded, nonraising
   -- resolver over an immutable authenticated mapping inventory. It may revoke
   -- ownership but must not mutate/reenter Source or State. While Owner_Ready
   -- holds, each ordinal must retain the same CPU/DMA association throughout
   -- this operation. Numeric validation cannot establish this association.
   -- Recheck owner, source root and revision after every lookup, including
   -- alias preflight and each later write/flush/readback resolution.
   -- No allocation/release authority is granted; all mappings remain retained
   -- on failure. Complete preflight remains quadratic in the used-page count.
   procedure Publish_Update
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Mapping_View; Stable_Root : Page_Mapping;
      Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]);
   generic
      with function Lookup (Ordinal : Positive) return Page_Mapping;
   procedure Publish_Update_From_Mappings
     (Object : in out State; Previous, Candidate : VM.Image;
      Mapping_Count : Natural; Stable_Root : Page_Mapping;
      Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]);
   -- Same stable authenticated inventory contract as Prepare_From_Mappings.
   -- Both source generations remain immutable under exclusion. Every lookup
   -- rechecks their roots/revisions and rejects the retained root CPU alias.
   -- Success still requires caller-completed TLB invalidation before resume.
   -- Scratch descriptor must be identical across both generations; verify
   -- retained tables without rewriting them or touching GPU-written data.
   -- Caller retains the original exclusive CPU/DMA mappings and visibility.
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
