with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply;
generic
   -- Single serialized supervisor, one stable device/process incarnation.
   with function Owner_Ready return Boolean;
   -- Allocate one retained order9 block mapped at CPU. The adapter must use
   -- the same target incarnation and below4GiB DMA policy for every call.
   -- Return zero/invalid on failure; ambiguous results must never be freed.
   with function Allocate (CPU : Unsigned_64) return Unsigned_64;
package Intel_GPU_Extent_Allocator is
   package E renames Intel_GPU_Physical_Extents;
   type Pool is limited private;
   type Budget is record
      Known : Boolean := False;
      Capacity, Retained, Available : Unsigned_64 := 0;
      Unassigned_Slots : Natural := 0;
   end record;
   -- Supervisor snapshot, not a reservation or public Mesa heap yet. Counts
   -- all private/application slices; closed names do not reclaim these bytes.
   -- Unknown before arena acquisition or after loss/uncertainty. Never reports
   -- system RAM or virtual aperture size as allocatable GPU backing.
   function Memory_Budget (Object : Pool) return Budget;
   procedure Acquire
     (Object : in out Pool; CPU_Base : Unsigned_64;
      Backing : out E.Map; Success : out Boolean);
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
      Index : Intel_GPU_Buffer_Reply.Layout.Slot;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success : out Boolean);
   -- Trusted supervisor integration only. All_References_Retired must include
   -- GPU mappings/TLBs, contexts, CPU grants/borrows and pending publications.
   -- Not an app assertion or scheduling-disable flag. Exact generation prevents
   -- stale retirement from freeing a newer allocation in the same slot.
   -- Final generation is retained permanently rather than wrapping/reusing it.
   procedure Retire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Intel_GPU_Buffer_Reply.Layout.Slot; Generation : Unsigned_32;
      All_References_Retired : Boolean; Success : out Boolean);
private
   type Entry_Record is record
      Offset, Bytes : Unsigned_64 := 0;
      Generation : Unsigned_32 := 0;
   end record;
   type Entries is array (Intel_GPU_Buffer_Reply.Layout.Slot) of Entry_Record;
   type Pool is limited record
      Attempted, Broken : Boolean := False;
      CPU : Unsigned_64 := 0;
      Bases : E.Addresses := [others => 0];
      Mapping : E.Map;
      Identity, Used : Unsigned_64 := 0;
      Items : Entries;
   end record;
end Intel_GPU_Extent_Allocator;
