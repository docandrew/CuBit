with Interfaces; use Interfaces;
with Intel_GPU_Record_Growth;
with Intel_GPU_Buffer_Reply;
generic
   with package Growth is new Intel_GPU_Record_Growth (<>);
   with function Owner_Ready return Boolean;
   with function Save_Reply return Boolean;
   with procedure Acquire
     (Index : Positive; Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success, Pending : out Boolean);
   with procedure Respond
     (Index : Positive; Generation : Unsigned_32;
      Buffer : Intel_GPU_Buffer_Reply.Extent_View; Success : Boolean);
package Intel_GPU_Allocation_Growth is
   -- Serialized, authenticated supervisor requests only. Save_Reply retains
   -- one dedicated capability; Respond consumes it even on delivery failure.
   -- Neither callback may recursively admit another request. Allocated backing
   -- is retained on lost delivery; no automatic rollback or replay.
   type Dispatcher is limited private;
   procedure Configure
     (Object : in out Dispatcher; Metadata_Bytes : Unsigned_64;
      Record_Quota : Positive; Accepted : out Boolean);
   function Pending (Object : Dispatcher) return Boolean;
   -- Uncommitted record identities permitted by policy, not reserved RAM or
   -- guaranteed allocations. Zero after quarantine or metadata-byte exhaustion.
   -- Committed_Records is the trusted allocator's current typed capacity.
   function Growth_Allowance
     (Object : Dispatcher; Committed_Records : Positive) return Natural;
   function Record_Budget
     (Object : Dispatcher; Committed_Records : Positive;
      Unused_Records : Natural) return Natural;
   procedure Begin_Request
     (Object : in out Dispatcher; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32; Accepted : out Boolean);
   -- One metadata phase OR one bounded backing step per service turn. Pending
   -- backing retains the saved reply and request; only a terminal step responds.
   procedure Step (Object : in out Dispatcher);
private
   type Dispatcher is limited record
      Controller : Growth.Controller;
      Configured, Active, Reject, Revoked : Boolean := False;
      Limit : Positive := 1;
      Metadata_Limit : Unsigned_64 := 0;
      Index : Positive := 1;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count := 1;
      Generation : Unsigned_32 := 0;
   end record;
end Intel_GPU_Allocation_Growth;
