with Interfaces;
package GPU_Test_Policy is
   Delay_First_Output_Ms : constant Interfaces.Unsigned_64 := 0;
   Reject_Client_Clear : constant Boolean := False;
   --  LAT-002 test mode "vblank": answer each presented frame only at the
   --  next point of a virtual 60 Hz grid, with hardware-latch timing, as
   --  intel-gpu answers at the latch it observed (headless display-latency
   --  with CUBIT_GPU_TEST_MODE=vblank reproduces the laptop's
   --  one-frame-in-flight-until-latch pacing).
   Vblank_Period_Us : constant Interfaces.Unsigned_64 := 16_667;
   --  Test mode "init-failure" (GPU-014): stop at start-up, as a driver
   --  whose device will not initialize does, so the renderer report must
   --  say Driver_Failed. False in every other build.
   Fail_At_Init : constant Boolean := False;
end GPU_Test_Policy;
