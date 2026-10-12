with Interfaces; use Interfaces;
with CuBit.Messages;
--  How long this process's calls to the GPU service wait for a reply.
--  Unbounded (the named Wait_Forever) unless the process sets a bound:
--  Desktop does before its renderer starts, so a stalled or deadlocked driver
--  costs it the GPU path, never the whole UI (IPC-004, docs/development-backlog.md).
package Native_GPU_Calls is
   subtype Timeout_Ms is Unsigned_64 range 1 .. 60_000;

   procedure Bound_Calls (Milliseconds : Timeout_Ms)
     with Export, Convention => C, External_Name => "cubit_intel_bound_calls";

   --  capCall to the GPU service with the configured deadline. A timeout
   --  returns REPLY_TIMEOUT (the outcome is unknown) and is counted. Once a
   --  bounded call (or a bounded wait, Note_Timeout) has timed out, the
   --  service counts as stalled: every later call returns REPLY_TIMEOUT at
   --  once, without calling, so a stalled driver costs at most one bound in
   --  total rather than one per call (NUC startup hang, 2026-10-11).
   function Call
     (Slot : CuBit.Messages.CapabilitySlot; Msg : in out CuBit.Messages.Message)
      return CuBit.Messages.MessageTag;

   --  The bound in milliseconds, or 0 while unbounded. It bounds these
   --  calls only: GPU completion waits (Native_GPU_Queue.Wait) carry their
   --  Vulkan caller's own deadline, and a hung GPU fails the context by the
   --  driver's watchdog.
   function Bound_Ms return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_intel_call_bound_ms";

   --  A bounded wait (or open) that a forever request ended by the bound:
   --  counted with the timed-out calls under Label.
   procedure Note_Timeout (Label : Unsigned_32);
   --  The deadline for a call made now: the bound, or Wait_Forever; already
   --  passed once the service counts as stalled.
   function Call_Deadline return Unsigned_64;
   --  A bounded call or wait has timed out (see Call).
   function Service_Stalled return Boolean;
   --  Calls that timed out, and the request label of the latest one.
   function Timeouts return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_call_timeouts";
   function Last_Timed_Out_Label return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_last_timed_out_label";
end Native_GPU_Calls;
