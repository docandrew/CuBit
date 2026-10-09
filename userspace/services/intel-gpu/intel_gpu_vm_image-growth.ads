generic
package Intel_GPU_VM_Image.Growth is
   type Plan_Status is (Invalid_Range, Occupied, Ready);
   type Requirements is record
      Status : Plan_Status := Invalid_Range;
      Additional_Tables : Natural := 0;
      Fits_Reserved : Boolean := False;
      Required_Tables : Natural := 0;
      Fits_Quota : Boolean := False;
      -- Exact total image target, distinct from currently committed metadata.
      -- Ready with Fits_Quota and not Fits_Reserved means metadata can grow;
      -- it does NOT authorize allocation/publication before that growth.
   end record;
   type Inspection_Status is (Idle, Scanning, Complete, Stale);
   type Inspection is limited private;
   procedure Start_Inspection
     (State : in out Inspection; Object : Image; GPU, Bytes : Unsigned_64;
      Accepted : out Boolean);
   procedure Step_Inspection (State : in out Inspection; Object : Image);
   procedure Cancel_Inspection (State : in out Inspection);
   function Inspection_State (State : Inspection) return Inspection_Status;
   function Inspection_Result (State : Inspection; Object : Image) return Requirements;
   -- Each step performs at most32 directory/descriptor/leaf actions (a missing
   -- subtree action counts at most3 prefixes). No allocation, IO or callbacks.
   -- Sealed images only. Keep the exact image incarnation alive and serialized across steps. Root,
   -- revision and sealed state are checked on each step and result retrieval.
   -- Results are planning evidence only, never ownership/publication authority.
   -- Active inspections cannot be overwritten by another Start. Synchronous
   -- Inspect adapters below still run all steps before returning. Offline
   -- mutation has no live revision increment; offline inspection must not yield.
   function Inspect (Object : Image; GPU, Bytes : Unsigned_64) return Requirements;
   type Offline_Requirements is record
      Topology : Requirements;
      Additional_Backing : Natural := 0;
   end record;
   function Inspect_Offline (Object : Image; GPU, Bytes : Unsigned_64)
     return Offline_Requirements;
   -- Unsealed images only. Additional_Tables counts missing directories;
   -- Additional_Backing subtracts already reserved unused table pages. The
   -- metadata Fits_Reserved flag is independent of physical backing. Read-only
   -- planning, not authorization of BO data or allocation, and not a live
   -- publication plan. Owner must serialize or revalidate the image revision
   -- after asynchronous allocation, register backing provenance, then append
   -- offline backing before normal bind validation/mapping.
   type Node is record
      Existing_Parent : Natural := 0;
      New_Parent : Natural := 0;
      -- Exactly one parent reference is nonzero. New_Parent is a 1-based
      -- ordinal in this plan, independent of Nodes' array lower bound.
      Index : Intel_GPU_ADLN_PPGTT.Table_Index := 0;
      Level : Intel_GPU_PPGTT_Scratch.Level := 0;
   end record;
   type Node_List is array (Positive range <>) of Node;
   type Description_Status is (Inactive, Checking, Emitting, Described, Rejected);
   type Description is limited private;
   procedure Start_Description
     (State : in out Description; Object : Image; GPU, Bytes : Unsigned_64;
      Output_Capacity : Natural; Accepted : out Boolean);
   procedure Cancel_Description (State : in out Description);
   function Description_State (State : Description) return Description_Status;
   function Description_Valid (State : Description; Object : Image) return Boolean;
   function Description_Count (State : Description; Object : Image) return Natural;
   generic
      with function Authorized return Boolean;
      with procedure Emit (Ordinal : Positive; Item : Node; Accepted : out Boolean);
   procedure Step_Description (State : in out Description; Object : Image);
   -- One inspection quantum OR at most32 traversal/emission actions per step.
   -- No emissions until full range/capacity preflight succeeds. A failed or
   -- cancelled plan has count0 even when private staging contains a prefix.
   -- Retain the same image and callback destinations across turns. Callbacks
   -- must be serialized/non-reentrant and preserve Object; cancellation and
   -- authority loss during callbacks are checked before further emission.
   -- Callback duration is outside the work bound. No GPU writes are allowed.
   generic
      with function Authorized return Boolean;
      with procedure Emit (Ordinal : Positive; Item : Node; Accepted : out Boolean);
   procedure Describe_Into
     (Object : Image; GPU, Bytes : Unsigned_64; Output_Capacity : Natural;
      Count : out Natural; Accepted : out Boolean);
   -- Emit into private staging only, never publish to the GPU here. Full
   -- geometry/capacity preflight precedes callbacks; failure may leave an
   -- uncommitted emitted prefix, but Count=0 and Accepted=False. Serialized
   -- non-reentrant callbacks must preserve Object. Recheck source identity
   -- and authority after each callback; traversal scratch is constant-sized.
   procedure Describe
     (Object : Image; GPU, Bytes : Unsigned_64; Nodes : out Node_List;
      Count : out Natural; Accepted : out Boolean);
   -- No DMA addresses: allocator supplies individually owned backing later.
   -- Existing_Parent=1 means the actual retained root, NOT necessarily the
   -- image's historical root DMA. Native adapter must resolve that identity.
   -- Parents precede children; this is a topology order, not permission to
   -- publish uninitialized nodes. Insufficient output capacity has no partial
   -- plan. Object must remain stable throughout this serialized call.
   -- Read-only, bounded topology plan for a sealed 4KiB-page VM.
   -- Directory traversal is once per leaf-table span (at most512 pages), not
   -- once per4KiB page. All existing leaves in the range are checked; absent
   -- subtrees are counted without scanning nonexistent leaves. No huge-page
   -- mappings or relaxed collision checks are implied by this traversal unit.
   -- Counts each absent directory prefix once, including below absent parents.
   -- No backing allocation, data authorization, publication or reuse authority.
   -- Fits_Reserved is capacity evidence, not permission to use reserved pages.
private
   type Prefixes is array (1 .. 3) of Unsigned_64;
   type Inspection is limited record
      State : Inspection_Status := Idle;
      Frozen : Boolean := False;
      Root, Epoch, Address : Unsigned_64 := 0;
      Remaining, Span, Leaf, Missing : Natural := 0;
      Depth : Positive range 1 .. 4 := 1;
      Current : Page_Number := 1;
      Candidate : Natural range 0 .. Capacity + 1 := 0;
      Last : Prefixes := [others => Unsigned_64'Last];
      Value : Requirements;
   end record;
   type Node_Ordinals is array (1 .. 3) of Natural;
   type Description is limited record
      State : Description_Status := Inactive;
      Query : Inspection;
      Address : Unsigned_64 := 0;
      Remaining, Limit, Made : Natural := 0;
      Depth : Positive range 1 .. 4 := 1;
      Missing_Depth : Natural range 0 .. 3 := 0;
      Current : Page_Number := 1;
      Candidate : Natural range 0 .. Capacity + 1 := 0;
      Last_Prefix : Prefixes := [others => Unsigned_64'Last];
      Last_Node : Node_Ordinals := [others => 0];
      Cancelled : Boolean := False;
   end record;
end Intel_GPU_VM_Image.Growth;
