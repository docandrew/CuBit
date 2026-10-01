with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with CuBit.Messages;
with Interfaces;
with Intel_GPU_Extent_Replies;
with Intel_GPU_Physical_Extents;
generic
   with function Owner_Ready return Boolean;
package Intel_GPU_Buffer_Memory is
   type Pool is limited private;
   type Allocation_Stage is (Idle, Owner_Check, Submit_Request, Awaiting_Reply,
                            Validate_Reply, Validate_Backing, Zero_Backing,
                            Flush_Backing, Readback_Backing, Granted, Denied);
   function Last_Stage (Object : Pool) return Allocation_Stage;
   -- Observational only; retained when cancelled/failed. Not a readiness gate.
   -- One pool per driver endpoint incarnation; it owns completion tokens
   -- 0x49475001..0x49475010 and the pinned supervisor endpoint in slot15.
   -- Event-loop API: at most one allocation per pool in flight. Start never
   -- waits. Route matching completions here and call Tick even when no reply
   -- arrives. Unrelated completions remain the caller's responsibility.
   -- Complete performs bounded zero/flush/readback work before returning;
   -- it avoids IPC waits, not memory initialization cost. Cancel quarantines
   -- the pool; late completions cannot publish or reuse its retained backing.
   procedure Start
     (Object : in out Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Pages : Intel_GPU_Buffer_Backing.Page_Count; Started : out Boolean);
   procedure Complete
     (Object : in out Pool; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean);
   procedure Tick (Object : in out Pool);
   procedure Cancel (Object : in out Pool);
   function Pending (Object : Pool) return Boolean;
   function Result (Object : Pool) return Intel_GPU_Buffer_Reply.Backing;
   -- Serialized caller, one pinned supervisor endpoint15, no other nonlogger
   -- request in flight. One attempt per slot. Uncertain failures quarantine
   -- this pool; no retry or free. Readiness must identify one driver incarnation.
   function Acquire
     (Object : in out Pool; Index : Intel_GPU_Buffer_Backing.Slot;
      Pages : Intel_GPU_Buffer_Backing.Page_Count) return Intel_GPU_Buffer_Reply.Backing;
   -- Success includes zeroing, cache flush and volatile readback, not GPU
   -- publication or permission to delegate a client-visible handle/grant.
private
   type Attempts is array (Intel_GPU_Buffer_Backing.Slot) of Boolean;
   type Backings is array (Intel_GPU_Buffer_Backing.Slot) of Intel_GPU_Buffer_Reply.Backing;
   type Pool is limited record
      Broken : Boolean := False;
      Stage : Allocation_Stage := Idle;
      Attempted : Attempts := [others => False];
      Items : Backings;
      Active : Boolean := False;
      Index : Intel_GPU_Buffer_Backing.Slot := 1;
      Pages : Intel_GPU_Buffer_Backing.Page_Count := 1;
      Started_At, Previous : Interfaces.Unsigned_64 := 0;
      Current : Intel_GPU_Buffer_Reply.Backing;
      Fetching : Boolean := False;
      Extent_Index : Intel_GPU_Physical_Extents.Block_Index := 0;
      Arena_ID, Requested_CPU, Requested_Bytes : Interfaces.Unsigned_64 := 0;
      Assembly : Intel_GPU_Extent_Replies.Assembly;
      Mapping : Intel_GPU_Physical_Extents.Map;
   end record;
end Intel_GPU_Buffer_Memory;
