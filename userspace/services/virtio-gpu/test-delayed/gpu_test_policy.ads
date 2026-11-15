with Interfaces;
package GPU_Test_Policy is
   --  Hold head 0 between transfer and scanout, after its DMA command completes.
   Delay_First_Output_Ms : constant Interfaces.Unsigned_64 := 250;
   Reject_Client_Clear : constant Boolean := False;
   --  LAT-002 test mode "vblank": answer each presented frame only at the
   --  next point of a virtual Vblank_Period_Us grid, with hardware-latch
   --  timing, as intel-gpu answers at the latch it observed. 0: off.
   Vblank_Period_Us : constant Interfaces.Unsigned_64 := 0;
end GPU_Test_Policy;
