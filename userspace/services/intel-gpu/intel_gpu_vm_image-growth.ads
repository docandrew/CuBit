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
end Intel_GPU_VM_Image.Growth;
