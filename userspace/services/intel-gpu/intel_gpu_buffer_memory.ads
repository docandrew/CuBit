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
                            Flush_Backing, Readback_Backing, Granted, Denied,
                            Awaiting_Retirement, Retired);
   function Last_Stage (Object : Pool) return Allocation_Stage;
   -- Observational only; retained when cancelled/failed. Not a readiness gate.
   -- One pool per driver endpoint incarnation; it owns completion tokens
   -- high-word 0x49475000 and the pinned supervisor endpoint in slot15.
   -- Low word is a non-repeating submission serial, including retries and
   -- extent fetches. Exhaustion fails closed; one pool per endpoint lifetime.
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
   type Attempts is array (Intel_GPU_Buffer_Backing.Slot) of Boolean;
   type Generations is array (Intel_GPU_Buffer_Backing.Slot) of Interfaces.Unsigned_32;
   type Backings is array (Intel_GPU_Buffer_Backing.Slot) of Intel_GPU_Buffer_Reply.Backing;
   type Pool is limited record
      Broken : Boolean := False;
      Stage : Allocation_Stage := Idle;
      Attempted : Attempts := [others => False];
      Slot_Generations : Generations := [others => 1];
      Items : Backings;
      Active : Boolean := False;
      Serial : Interfaces.Unsigned_32 := 0;
      Completion_Token : Interfaces.Unsigned_64 := 0;
      Index : Intel_GPU_Buffer_Backing.Slot := 1;
      Generation : Interfaces.Unsigned_32 := 1;
      Retiring : Boolean := False;
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
