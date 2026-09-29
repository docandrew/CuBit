with Interfaces;
generic
   with function Read_Control return Interfaces.Unsigned_32;
   with procedure Write_Control (Value : Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Pause;
package Intel_GPU_DC_Write is
   type Outcome is (Stable_Register, Rejected, Invalid_MMIO, Write_Failed,
                    Read_Budget_Exhausted, Write_Budget_Exhausted);
   type Report is record
      Status : Outcome := Rejected;
      Reads, Writes : Natural := 0;
      Consecutive : Natural range 0 .. 7 := 0;
   end record;
   -- Low-level DC_STATE_EN (0x45504) write verification ONLY, not a DC-off
   -- reference. Caller owns ordered MMIO, serialization, DMC prerequisites,
   -- the safely constructed complete register value, and all exit repair.
   -- DC3CO status clearing/200us delay, clock/buffer checks and PHY restoration
   -- are deliberately not implied by Stable_Register. Callbacks are bounded,
   -- non-raising; an exception or any attempt permanently consumes the instance.
   -- The independently finite read budget handles intermittent matches; write
   -- budget includes the initial write. No rollback or automatic restoration.
   procedure Execute
     (Authorized : Boolean; Target : Interfaces.Unsigned_32;
      Read_Limit, Write_Limit : Positive; Result : out Report);
end Intel_GPU_DC_Write;
