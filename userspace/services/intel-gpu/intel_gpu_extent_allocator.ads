with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Record_Store;
with Intel_GPU_Extent_Directory;
generic
   -- Single serialized supervisor, one stable device/process incarnation.
   with function Owner_Ready return Boolean;
   -- Allocate one retained order9 block mapped at CPU. The adapter must use
   -- the same target incarnation and configured DMA limit for every call.
   -- The default production adapter retains its below4GiB policy.
   -- Return zero/invalid on failure; ambiguous results must never be freed.
   with function Allocate (CPU : Unsigned_64) return Unsigned_64;
package Intel_GPU_Extent_Allocator is
   package E renames Intel_GPU_Physical_Extents;
   type Pool is limited private;
   -- Trusted device policy, before any allocation. This reserves/allocates
   -- nothing and does not change CPU/GPU/IOMMU mapping authority. Defaults
   -- retain the current NUC policy; clients must negotiate matching limits
   -- before production adapters select a larger arena.
   procedure Configure_Heap
     (Object : in out Pool; Byte_Quota, DMA_Limit : Unsigned_64;
      Accepted : out Boolean);
   -- Physical allocation adapters must use this same frozen ceiling. Reading
   -- policy neither establishes ownership nor authorizes an allocation.
   function DMA_Ceiling (Object : Pool) return Unsigned_64;
   function Extent_Capacity (Object : Pool) return Positive;
   function Required_Extent_Metadata (Object : Pool) return Natural;
   -- Nonzero only when the last Step_Buffer paused before a physical callback
   -- for directory capacity. Saved-request adapters may grow metadata then
   -- resume that same request; this is not an allocation retry indication.
   -- Separate retained CPU metadata, never pixel backing. Same lifetime and
   -- disjointness obligations as Extend_Records; initialize the pool first.
   procedure Extend_Extents
     (Object : in out Pool; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   function Record_Capacity (Object : Pool) return Positive;
   -- Trusted committed CPU metadata in the supervisor's address space, not
   -- the driver's GPU arena. Caller retains a disjoint stable mapping for the
   -- pool lifetime. Growth changes bookkeeping only, never physical authority.
   procedure Extend_Records
     (Object : in out Pool; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   type Budget is record
      Known : Boolean := False;
      Capacity, Committed, Retained, Available : Unsigned_64 := 0;
      Unassigned_Slots : Natural := 0;
   end record;
   -- Capacity is the bounded arena policy ceiling, Committed is retained
   -- physical backing, Retained is live assigned slice bytes, and Available
   -- is remaining allocation quota (NOT a promise of free physical RAM).
   -- Unknown before first backing acquisition or after loss/uncertainty.
   -- Retiring slices does not reduce Committed: physical blocks stay retained.
   function Memory_Budget (Object : Pool) return Budget;
   procedure Acquire
     (Object : in out Pool; CPU_Base : Unsigned_64;
      Backing : out Intel_GPU_Extent_Directory.Borrowed_View; Success : out Boolean;
      Required_Bytes : Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.Default_Heap.Byte_Quota);
   -- Read-only snapshot: querying addresses must never cause backing growth.
   function Snapshot (Object : Pool) return Intel_GPU_Extent_Directory.Borrowed_View;
   -- Internal borrowed prefix, never a wire capability. Pool and metadata
   -- must outlive every view. Quarantine invalidates views without freeing RAM.
   -- Assign a disjoint CPU-virtual slice of the retained arena. Physical
   -- adjacency is not required. Same live slot/size/identity/generation is
   -- idempotent. First generation is1; retired slots require exactly the next
   -- generation, never wrap. Retiring releases only a slice, not physical RAM.
   -- All-or-nothing publication, not rollback. Partial backing is retained;
   -- failed or revoked pools never retry. Success does not zero RAM, publish
   -- GPU PTEs, or permit a client grant. Caller must clear reused storage and
   -- complete its visibility transition before exposing it to a new client.
   procedure Acquire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success : out Boolean);
   procedure Step_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count; Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success, Pending : out Boolean);
   -- Serialized caller retains the same request until Pending=False. At most
   -- one new physical block per call; no slice is published before fully backed.
   -- Trusted supervisor integration only. All_References_Retired must include
   -- GPU mappings/TLBs, contexts, CPU grants/borrows and pending publications.
   -- Not an app assertion or scheduling-disable flag. Exact generation prevents
   -- stale retirement from freeing a newer allocation in the same slot.
   -- Final generation is retained permanently rather than wrapping/reusing it.
   procedure Retire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Positive; Generation : Unsigned_32;
      All_References_Retired : Boolean; Success : out Boolean);
private
   type Entry_Record is record
      Offset, Bytes : Unsigned_64 := 0;
      Generation : Unsigned_32 := 0;
      Previous, Next : Natural := 0;
   end record;
   package Records is new Intel_GPU_Record_Store (Entry_Record, (others => <>));
   type Pool is limited record
      Attempted, Broken, Configured : Boolean := False;
      Limit : Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.Default_Heap.Byte_Quota;
      DMA_Limit : Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.Default_Heap.DMA_Limit;
      CPU : Unsigned_64 := 0;
      Directory : aliased Intel_GPU_Extent_Directory.Directory;
      Metadata_Required : Natural := 0;
      Identity, Used : Unsigned_64 := 0;
      First_Extent : Natural := 0;
      -- Maintained only with successful record publication/allocation/retirement.
      -- Budget queries must not walk the potentially large metadata registry.
      Unassigned : Natural := Intel_GPU_Buffer_Reply.Layout.Bootstrap_Slots;
      Items : Records.Store;
   end record;
end Intel_GPU_Extent_Allocator;
