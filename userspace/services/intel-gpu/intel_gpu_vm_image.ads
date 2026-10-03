with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_VM_Table_Store;
generic
   Capacity : Positive;
   Bootstrap_Tables : Positive := Capacity;
package Intel_GPU_VM_Image is
   pragma Compile_Time_Error
     (Capacity < 4 or Capacity > 4096, "invalid page-table capacity");
   -- Offline four-level PPGTT image, not live VM_BIND. Serialized owner only.
   -- The owner authorizes and retains every DMA page, copies the sealed image
   -- into its exclusive backing, and establishes visibility before publishing
   -- the root. No client-supplied physical addresses, allocation or MMIO here.
   -- Reuse the hardware encoder; unlike Initial_VM this spans raw 48-bit VA.
   subtype Page_Number is Positive range 1 .. Capacity;
   type Backing_Pages is array (Page_Number) of Unsigned_64;
   type Data_Pages is array (Positive range <>) of Unsigned_64;
   type Image is limited private;
   function Metadata_Capacity (Object : Image) return Positive;
   procedure Extend_Metadata (Object : in out Image; Base, Bytes : Unsigned_64;
                              Accepted : out Boolean);
   -- Trusted committed CPU table-mirror reservation, not GPU table backing.
   -- Serialized owner retains it for the image lifetime. Extending storage
   -- changes neither mappings, source revision, nor GPU publication authority.
   type Insertion_Receipt is limited private;
   type Growth_Receipt is limited private;
   -- Callback-independent storage for one directory-growth transaction. The
   -- service retains it across asynchronous allocation; Writer owns transitions.
   -- Storage for the insertion child, owned by a serialized VM updater.
   -- Library-level placement avoids a capacity-sized service stack frame.
   procedure Initialize
     (Object : in out Image; Backing : Backing_Pages; Accepted : out Boolean;
      Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0]);
   -- One initialization attempt per incarnation; backing must be distinct, valid
   -- DMA pages. It must not be recycled while this image/context exists.
   -- Optional private scratch pages must all be valid, distinct and disjoint
   -- from tables/data. All-zero selects faulting absent entries. Caller owns
   -- and zeroes scratch data and exports all three scratch tables BEFORE root
   -- publication. Never share writable scratch with another protection domain.
   procedure Prepare_Update
     (Target : in out Image; Source : Image; Backing : Backing_Pages;
      Accepted : out Boolean);
   -- One-attempt offline copy of a sealed source into fresh, disjoint table
   -- backing. Rebase directory pointers, preserve leaf addresses/attributes,
   -- leave Target mutable and Source untouched. Owner retains BOTH generations
   -- and all data backing. This does not publish a root, quiesce a context,
   -- invalidate translations, or authorize old-backing/address reuse.
   -- The trusted caller must serialize and establish actual backing authority.
   -- A used target can only be reused through Snapshots.Forget_Retired after
   -- independent retirement confirmation, never by retrying Initialize.
   procedure Map_Page
     (Object : in out Image; GPU, DMA : Unsigned_64;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   -- Rejection leaves the image unchanged, including capacity. Rejects zero
   -- VA, non-page values, existing mappings and ALL table/data aliases, even
   -- unused table backing. Data aliases within this authorized VM are allowed
   -- only with the same cache policy. Cross-VM/CPU alias policy still belongs
   -- to the backing owner; this offline builder cannot establish it.
   -- Read-only remains rejected by the existing Gen12 erratum policy.
   procedure Map_Pages
     (Object : in out Image; GPU : Unsigned_64; Data : Data_Pages;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   -- Map consecutive GPU pages to possibly noncontiguous authorized DMA pages.
   -- Entire operation succeeds or leaves all entries/capacity unchanged. Data
   -- must remain immutable during this serialized call. Empty lists rejected.
   procedure Seal (Object : in out Image; Accepted : out Boolean);
   procedure Seal_Update (Object : in out Image; Accepted : out Boolean);
   -- Only a Prepare_Update successor may seal with no mapped data pages.
   -- Retained directories remain valid; absent leaves use the selected
   -- faulting or private-scratch policy (never an application BO mapping).
   -- Initial context preparation still requires the nonempty Seal operation.
   -- Empty sealing does not authorize submission, publication or reclamation.
   procedure Unmap_Pages
     (Object : in out Image; GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean);
   -- Offline only: all leaves must exist and match the owner's expected DMA
   -- pages. Preflight the whole range before removing anything. Retain empty
   -- directory pages for reuse; never release backing or modify a sealed VM.
   -- Success is NOT permission to recycle physical pages or GPU addresses
   -- in a published context; that requires the separate live-update protocol.
   function Sealed (Object : Image) return Boolean;
   function Used (Object : Image) return Natural;
   function Root_DMA (Object : Image) return Unsigned_64;
   function Revision (Object : Image) return Unsigned_64;
   function Direct_Successor (Object, Candidate : Image) return Boolean;
   -- Local metadata incarnation, advanced on initialization/preparation and
   -- adoption. Never a hardware completion or allocation authority.
   function Page_DMA (Object : Image; Page : Page_Number) return Unsigned_64;
   -- Only allocated tables have a Page_DMA identity. Reserved capacity is
   -- not part of the live tree and must not be compared with that accessor.
   function Used_Tables_Match (Object : Image; Backing : Backing_Pages) return Boolean;
   function Leaf_Table (Object : Image; Page : Page_Number) return Boolean;
   -- Valid allocated PT page, never a directory or unused reserved capacity.
   -- True only for a valid page-aligned DMA extent disjoint from every
   -- reserved table page AND every mapped data page in this valid image.
   -- Numeric isolation only; the backing owner must exclude hidden aliases.
   function DMA_Disjoint (Object : Image; First, Bytes : Unsigned_64) return Boolean;
   generic
      with function Conflicts (Page : Unsigned_64) return Boolean;
   function Backing_Disjoint (Object : Image) return Boolean;
   -- Walk reserved tables and mapped pages once; the owner supplies its
   -- backing-range predicate. No contiguous-allocation assumption.
   function Entry_Value
     (Object : Image; Page : Page_Number;
      Index : Intel_GPU_ADLN_PPGTT.Table_Index) return Unsigned_64;
   -- Hardware export, including fallback entries. Lookup reports explicit
   -- application mappings only and returns zero for scratch-backed holes.
   function Scratch_DMA
     (Object : Image; L : Intel_GPU_PPGTT_Scratch.Level) return Unsigned_64;
   function Scratch_Entry
     (Object : Image; L : Intel_GPU_PPGTT_Scratch.Table_Level) return Unsigned_64;
   -- Every one of the512 words in scratch table L has Scratch_Entry(L).
   function Lookup (Object : Image; GPU : Unsigned_64) return Unsigned_64;
private
   type Growth_Phase is (Idle, Failed, Fill_Child, Flush_Child, Verify_Child,
                         Read_Parent, Write_Parent, Flush_Parent, Verify_Parent,
                         Publication_Done);
   type Growth_Link is record
      Parent_DMA, Child_DMA, Expected, Value, Fill : Unsigned_64 := 0;
      Index : Intel_GPU_ADLN_PPGTT.Table_Index := 0;
   end record;
   type Growth_Links is array (Page_Number) of Growth_Link;
   type Growth_Receipt is limited record
      Begun, Done : Boolean := False;
      Commit_Tried, Adopted : Boolean := False;
      Root, Epoch, GPU, Bytes, Hardware_Root : Unsigned_64 := 0;
      Count : Natural range 0 .. Capacity := 0;
      Pages : Data_Pages (1 .. Capacity) := [others => 0];
      First_Adopted : Natural range 0 .. Capacity := 0;
      Phase : Growth_Phase := Idle;
      Plan : Growth_Links;
      Cursor : Page_Number := 1;
      Word : Intel_GPU_ADLN_PPGTT.Table_Index := 0;
   end record;
   type Insertion_Receipt is limited record
      Poisoned : Boolean := False;
      Pending : Boolean := False;
      Root, Epoch, First : Unsigned_64 := 0;
      Pages : Natural range 0 .. Capacity * 512 := 0;
      Words : Data_Pages (1 .. Capacity * 512) := [others => 0];
   end record;
   package Table_Storage is new Intel_GPU_VM_Table_Store (Bootstrap_Tables, Capacity);
   type Page_Levels is array (Page_Number) of Intel_GPU_PPGTT_Scratch.Level;
   type Image is limited record
      Attempted, Valid, Frozen : Boolean := False;
      -- Set only by the trusted retirement child after exact receipt disposal.
      -- Cleared before any new incarnation attempt, including failed attempts.
      Retired_Receipt : Boolean := False;
      Mapped_Pages : Natural range 0 .. Capacity * 512 := 0;
      Count : Natural range 0 .. Capacity := 0;
      -- Logical predecessor only. Hardware's stable root is owned separately
      -- by Application_Image; this does not authorize publication or reuse.
      Predecessor_Root : Unsigned_64 := 0;
      Epoch, Predecessor_Epoch : Unsigned_64 := 0;
      DMA : Backing_Pages := [others => 0];
      Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0];
      Levels : Page_Levels := [others => 0];
      Tables : Table_Storage.Store;
   end record;
   function Raw_Word (Object : Image; Page : Page_Number;
                      Index : Intel_GPU_ADLN_PPGTT.Table_Index) return Unsigned_64;
   function Raw_Page (Object : Image; Page : Page_Number) return Table_Storage.Page;
   procedure Set_Raw_Word (Object : in out Image; Page : Page_Number;
                          Index : Intel_GPU_ADLN_PPGTT.Table_Index; Value : Unsigned_64);
   procedure Clear_Table (Object : in out Image; Page : Page_Number);
end Intel_GPU_VM_Image;
