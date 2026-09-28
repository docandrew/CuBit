with Interfaces;
-- ADL-N single-engine stop and pending-MI_FORCE_WAKE drain. Caller holds all
-- required forcewake and exclusive submission/reset ownership. Ordered,
-- bounded, non-raising callbacks; Now measures monotonic microseconds; Last
-- means unavailable. For physical intervals <1us, timestamp difference must
-- overstate elapsed time by at most 2us (all error sources included).
generic
   with function Read_Mode return Interfaces.Unsigned_32;
   with function Read_Pending return Interfaces.Unsigned_32;
   with function Read_Power return Interfaces.Unsigned_32;
   with procedure Write_Mode (Value : Interfaces.Unsigned_32);
   with procedure Write_Prefetch (Value : Interfaces.Unsigned_32);
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_Engine_Stop is
   type Result is (Stopped, Timed_Out, Invalid_MMIO, Invalid_Clock);
   -- On failure stop/prefetch requests remain set: caller must quarantine.
   -- No automatic restart of unknown firmware work and no reset on failure.
   procedure Stop (Poll_Limit : Positive; Status : out Result;
                   Timeout_Us : Interfaces.Unsigned_64 := 100_000);
end Intel_GPU_Engine_Stop;
