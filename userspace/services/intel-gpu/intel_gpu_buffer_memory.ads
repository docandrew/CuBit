with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with CuBit.Messages;
with Interfaces;
with Intel_GPU_Extent_Replies;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Directory;
with Intel_GPU_Record_Store;
with Intel_GPU_Metadata_Arena;
generic
   with function Owner_Ready return Boolean;
   with package Extent_Storage is new Intel_GPU_Metadata_Arena (<>);
package Intel_GPU_Buffer_Memory is
   use type Interfaces.Unsigned_64;
   type Pool is limited private;
   -- Trusted policy matching the supervisor, before the first request. This
   -- does not reserve CPU VA, grant DMA authority or request physical backing.
   procedure Configure_Heap
     (Object : in out Pool; Byte_Quota, DMA_Limit, Metadata_Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean);
   function Record_Capacity (Object : Pool) return Positive;
   -- Trusted disjoint committed CPU metadata, retained for the pool lifetime.
   -- No allocation/retirement may be in flight; growth cannot revive a failed
   -- pool or change DMA authority, current backing, or submission identities.
   procedure Extend_Records
     (Object : in out Pool; Base, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean);
   type Allocation_Stage is (Idle, Owner_Check, Submit_Request, Awaiting_Reply,
                            Validate_Reply, Validate_Backing, Zero_Backing,
                            Flush_Backing, Readback_Backing, Granted, Denied,
                            Awaiting_Retirement, Retired, Awaiting_Extent_Metadata);
   function Last_Stage (Object : Pool) return Allocation_Stage;
   -- Observational only; retained when cancelled/failed. Not a readiness gate.
   -- One pool per driver endpoint incarnation; it owns completion tokens
   -- high-word 0x49475000 and the pinned supervisor endpoint in slot15.
   -- Low word is a non-repeating submission serial, including retries and
   -- extent fetches. Exhaustion fails closed; one pool per endpoint lifetime.
   -- Event-loop API: at most one allocation per pool in flight. Start never
   -- waits. Route matching completions here and call Tick even when no reply
   -- arrives. Unrelated completions remain the caller's responsibility.
   -- Complete and subsequent Tick calls initialize at most 64KiB each
   -- (zero/flush/readback). Result remains unavailable until all chunks pass.
   -- Overlap validation examines at most 64 backing records per call, before
   -- any RAM initialization. Cancel quarantines
   -- the pool; late completions cannot publish or reuse its retained backing.
   procedure Start
     (Object : in out Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Pages : Intel_GPU_Buffer_Backing.Page_Count; Started : out Boolean);
   procedure Complete
     (Object : in out Pool; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean);
   -- Trusted serialized coordinator only, never an app-supplied assertion.
   -- Requires closed admission and confirmed GPU/TLB/CPU grant retirement.
   -- Success here means request submitted, not retirement completed. Only
   -- Retirement_Confirmed for this exact slot/generation certifies the ack;
   -- Last_Stage is diagnostic only and must not authorize reuse.
   -- Unknown outcome quarantines the pool; retirement requests are not retried.
   procedure Retire
     (Object : in out Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Generation : Interfaces.Unsigned_32; All_References_Retired : Boolean;
      Started : out Boolean);
   function Retirement_Confirmed
     (Object : Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Generation : Interfaces.Unsigned_32) return Boolean;
   procedure Tick (Object : in out Pool);
   procedure Cancel (Object : in out Pool);
   function Pending (Object : Pool) return Boolean;
   -- Local validation/initialization can advance without a completion or timer.
   -- Service other event-loop work, then Tick again instead of sleeping.
   function Local_Work_Pending (Object : Pool) return Boolean;
   function Result (Object : Pool) return Intel_GPU_Buffer_Reply.Backing;
   -- Serialized caller, one pinned supervisor endpoint15, no other nonlogger
   -- request in flight. One attempt per slot generation. Uncertain failures quarantine
   -- this pool; no retry or free. Readiness must identify one driver incarnation.
   function Acquire
     (Object : in out Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Pages : Intel_GPU_Buffer_Backing.Page_Count) return Intel_GPU_Buffer_Reply.Backing;
   -- Success includes zeroing, cache flush and volatile readback, not GPU
   -- publication or permission to delegate a client-visible handle/grant.
private
   type Backing_Record is record
      Attempted : Boolean := False;
      Generation : Interfaces.Unsigned_32 := 1;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   end record;
   package Records is new Intel_GPU_Record_Store (Backing_Record, (others => <>));
   type Pool is limited record
      Broken : Boolean := False;
      Configured, Waiting_Metadata : Boolean := False;
      Heap_Limit : Interfaces.Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.Byte_Quota;
      DMA_Limit : Interfaces.Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.DMA_Limit;
      Metadata_Limit : Interfaces.Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.Metadata_Bytes;
      Metadata_Published : Interfaces.Unsigned_64 := 0;
      Extent_Metadata : Extent_Storage.Arena;
      Stage : Allocation_Stage := Idle;
      Items : Records.Store;
      Active : Boolean := False;
      Serial : Interfaces.Unsigned_32 := 0;
      Completion_Token : Interfaces.Unsigned_64 := 0;
      Index : Intel_GPU_Buffer_Backing.Slot := 1;
      Generation : Interfaces.Unsigned_32 := 1;
      Retiring : Boolean := False;
      Pages : Intel_GPU_Buffer_Backing.Page_Count := 1;
      Started_At, Previous : Interfaces.Unsigned_64 := 0;
      Current : Intel_GPU_Buffer_Reply.Backing;
      Initializing : Boolean := False;
      Validating : Boolean := False;
      Validated_Records : Natural := 0;
      Initialization_Offset : Interfaces.Unsigned_64 := 0;
      Initialization_Backing : Intel_GPU_Buffer_Reply.Backing;
      Fetching : Boolean := False;
      Extent_Index : Natural := 0;
      Arena_ID, Requested_CPU, Requested_Bytes : Interfaces.Unsigned_64 := 0;
      Assembly : Intel_GPU_Extent_Replies.Assembly;
      Mapping : Intel_GPU_Extent_Directory.Borrowed_View;
   end record;
end Intel_GPU_Buffer_Memory;
