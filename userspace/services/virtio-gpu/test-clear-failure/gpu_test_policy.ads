with Interfaces;
package GPU_Test_Policy is
   Delay_First_Output_Ms : constant Interfaces.Unsigned_64 := 0;
   --  Allow startup clearing, then reject subsequent clears on either head.
   --  A build-only fixture; no client opcode enables this in production.
   Reject_Client_Clear : constant Boolean := True;
   --  LAT-002 test mode "vblank": answer each presented frame only at the
   --  next point of a virtual Vblank_Period_Us grid, with hardware-latch
   --  timing, as intel-gpu answers at the latch it observed. 0: off.
   Vblank_Period_Us : constant Interfaces.Unsigned_64 := 0;
end GPU_Test_Policy;
