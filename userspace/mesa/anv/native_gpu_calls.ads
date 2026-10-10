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
   --  returns REPLY_TIMEOUT (the outcome is unknown) and is counted.
   function Call
     (Slot : CuBit.Messages.CapabilitySlot; Msg : in out CuBit.Messages.Message)
      return CuBit.Messages.MessageTag;

   --  The bound in milliseconds, or 0 while unbounded. It bounds these
   --  calls only: GPU completion waits (Native_GPU_Queue.Wait) carry their
   --  Vulkan caller's own deadline, and a hung GPU fails the context by the
   --  driver's watchdog.
   function Bound_Ms return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_intel_call_bound_ms";

   --  Calls that timed out, and the request label of the latest one.
   function Timeouts return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_call_timeouts";
   function Last_Timed_Out_Label return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_last_timed_out_label";
end Native_GPU_Calls;
