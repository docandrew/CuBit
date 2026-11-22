with Interfaces;
-- Exclusive authenticated domain owner only. Bounded, ordered, non-raising
-- callbacks are required; a software deadline cannot preempt a stalled MMIO.
generic
   with function Read_32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write_32 (Offset, Value : Interfaces.Unsigned_32);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_Forcewake_Fallback is
   type Result is (Recovered, Invalid_MMIO, Invalid_Clock, Timed_Out,
                   Poll_Exhausted, Ack_Unchanged);
   procedure Recover
     (Request_Register, Ack_Register, Expected : Interfaces.Unsigned_32;
      Poll_Limit : Positive; Status : out Result;
      Timeout_Us : Interfaces.Unsigned_64 := 50_000);
   -- Expected must be 0 or 1. Up to ten bit15 toggles, 10*pass us delay.
   -- One time budget spans all passes. Poll budget independently bounds each
   -- wait/delay when the clock stalls. Recovered requires original ACK plus
   -- fallback-clear confirmation. On failure, ownership remains uncertain.
end Intel_GPU_Forcewake_Fallback;
