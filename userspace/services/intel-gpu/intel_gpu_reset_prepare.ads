with Interfaces;
-- One admitted engine's RING_RESET_CTL, not a GPU reset or ownership transfer.
-- Caller holds required forcewake domains, excludes concurrent register users,
-- and has stopped submission/engines including applicable hardware workarounds.
-- Callbacks are bounded, ordered and non-raising; clock units are microseconds.
generic
   with function Read_Control return Interfaces.Unsigned_32;
   with procedure Write_Control (Value : Interfaces.Unsigned_32);
   with procedure Pause;
   with function Now return Interfaces.Unsigned_64;
package Intel_GPU_Reset_Prepare is
   type Result is (Ready, Timed_Out, Invalid_MMIO, Invalid_Clock);
   procedure Prepare (Poll_Limit : Positive; Status : out Result;
                      Timeout_Us : Interfaces.Unsigned_64 := 700);
   -- Must be called on every selected engine on exit, even after preparation
   -- failure. A failed cleanup does not permit memory release or reset retry.
   procedure Cancel;
end Intel_GPU_Reset_Prepare;
