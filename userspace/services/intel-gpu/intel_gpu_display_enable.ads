with Interfaces;
with Intel_GPU_Display_Topology;
generic
   Item : Intel_GPU_Display_Topology.Request_Well;
   with function Read_32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write_32
     (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   with procedure Post_Enable (Success : out Boolean);
   with procedure Pre_Disable (Success : out Boolean);
package Intel_GPU_Display_Enable is
   type Result is (Ready, Rejected, Invalid_MMIO, Invalid_Clock,
                   Deadline_Expired, Poll_Exhausted, Write_Failed,
                   Request_Changed, Post_Enable_Failed, Released, Pre_Disable_Failed);
   -- Failed attempts permanently consume the instance. Caller owns the
   -- device, serialization, ancestor/DC-off references, and stable baseline.
   -- Prerequisites_Ready must represent those validated facts, not IPC input.
   -- Bounded non-raising callbacks; ordered uncached MMIO. Post_Enable must
   -- perform required platform IRQ/VGA work, not simply return success.
   -- No rollback: request/workaround writes may persist on every failure.
   -- Added is conservative after a possible request write, including failure.
   -- Each ACK/fuse phase is bounded by 1 ms and Poll_Limit samples. This
   -- does not bound callback execution or total scheduling latency.
   procedure Execute
     (Prerequisites_Ready : Boolean; Poll_Limit : Positive;
      Added : out Boolean; Status : out Result);
   -- Only after Ready. The caller must release descendants first and ensure
   -- no inherited consumer relies on our added request. An inherited request
   -- is left untouched (including no Pre_Disable call). For an added request,
   -- Pre_Disable handles IRQ/VGA prerequisites; clear only our request bit and
   -- verify it cleared. State may stay on because other requesters exist.
   -- Released permits reuse. Any failure/exception prohibits further access.
   procedure Release (Status : out Result);
end Intel_GPU_Display_Enable;
