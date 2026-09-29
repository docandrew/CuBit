with Interfaces;
with Intel_GPU_GuC_Status;
generic
   -- Ordered, bounded, nonraising callbacks under retained forcewake.
   with function Read_Status return Interfaces.Unsigned_32;
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_GuC_Wait is
   type Result is (Firmware_Ready, Device_Failed, Invalid_Clock, Timed_Out);
   procedure Execute (Poll_Limit : Positive; Status : out Result;
     Last_Raw : out Interfaces.Unsigned_32;
     Last_State : out Intel_GPU_GuC_Status.State);
   -- Read-only; call only after this instance's successful firmware transfer.
   -- Caller must exclude reset/reload; stale READY is not proof of a new boot.
   -- Now is monotonic microseconds; Last denotes unavailable. Poll bound also
   -- ensures termination when the counter stops. No retry/reset is performed.
end Intel_GPU_GuC_Wait;
