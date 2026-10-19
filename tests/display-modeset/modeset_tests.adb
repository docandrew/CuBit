--  The HDMI modeset sequencer against a register model: 1080p60 firmware
--  output on transcoder B / DDI B / DPLL0 changed to 3840x2160@60 (HDMI 2.0,
--  scrambled), with i915's ordering rules checked on every write; then
--  injected failures (a PLL that does not lock once, a pipe that never
--  starts, a FIFO underrun) and the rollback to the firmware mode.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with EDID_Fixtures;
with Modeset_Model; use Modeset_Model;
with Intel_GPU_Display_Registers;
with Intel_GPU_Modeset_Plan;
with Intel_GPU_HDMI_Modeset;
procedure Modeset_Tests is
   package R renames Intel_GPU_Display_Registers;
   package P renames Intel_GPU_Modeset_Plan;
   package C renames CuBit.Monitor_EDID.Catalog;
   package M is new Intel_GPU_HDMI_Modeset
     (Modeset_Model.Read, Modeset_Model.Write, Modeset_Model.Now_Us, Modeset_Model.Pause,
      Modeset_Model.Write_TMDS_Config, Modeset_Model.Log);
   use type M.Outcome, M.Step;
   Output : constant P.Output := (R.B, R.Port_B, R.DPLL0);
   UHD : constant C.Mode_List := C.Modes (EDID_Fixtures.UHD_HDMI_Base, EDID_Fixtures.UHD_HDMI_Extensions);

   procedure Firmware_1080p is
   begin
      Reset;
      Set (16#46010#, 16#8C00_0000# or 16#4000_0000#);
      Set (R.DPLL_CFGCR0 (R.DPLL0), 464 + 16#400# * 2 ** 10);
      Set (R.DPLL_CFGCR1 (R.DPLL0), 3 * 2 ** 10 + 16#200# + 2 * 2 ** 6 + 1 * 2 ** 2);
      Set (R.ICL_DPCLKA_CFGCR0, 2 ** 10);  --  DDI A gated, DDI B on DPLL0
      Set (16#46144#, 2 * 2 ** 28);
      Set (16#61000#, 16#0897_077F#); Set (16#61004#, 16#0897_077F#); Set (16#61008#, 16#0803_07D7#);
      Set (16#6100C#, 16#0464_0437#); Set (16#61010#, 16#0464_0437#); Set (16#61014#, 16#0440_043B#);
      Set (16#6101C#, 16#077F_0437#);
      Set (16#61400#, 16#9003_0006#);
      Set (16#71008#, 16#C000_0000#);
      Set (R.DDI_Buffer_Control (R.Port_B), 16#8000_0006#);
      Set (16#61200#, 16#9000_1000#);
      Set (16#71180#, 16#8400_0000#); Set (16#71188#, 120); Set (16#71190#, 16#0437_077F#);
      Set (16#7119C#, 16#0100_0000#); Set (16#7127C#, 16#03DF_0000#);
      Set (16#71240#, 16#8000_C010#);
   end Firmware_1080p;

   procedure Check_Applied is
      Current : P.Configuration;
      Wanted : P.Target_Result;
      Result : M.Outcome;
      Failed : M.Step;
   begin
      Firmware_1080p;
      Current := M.Capture (Output);
      pragma Assert (Current.Function_Word = 16#9003_0006# and then not Current.Write_SCDC);
      Wanted := P.Target (Current, UHD.Items (1).Timing, 15_360, 19_200, True, True);
      pragma Assert (Wanted.Ready);
      M.Change (Output, Current, Wanted.Value, Result, Failed);
      pragma Assert (Result = M.Applied and then Failed = M.None);
      pragma Assert (Violations = 0);
      pragma Assert (Get (R.DPLL_CFGCR0 (R.DPLL0)) = Wanted.Value.CFGCR0);
      pragma Assert (Get (16#61000#) = 16#112F_0EFF# and then Get (16#6101C#) = 16#0EFF_086F#);
      pragma Assert ((Get (16#61400#) and 16#11#) = 16#11#);  --  high TMDS ratio + scrambling
      pragma Assert (Get (16#71188#) = 240 and then Get (16#71190#) = 16#086F_0EFF#);
      pragma Assert (Get (16#61200#) = 0);  --  infoframes off
      pragma Assert (SCDC_Value = 3);
      --  Level 0 nearly the whole 992-block allocation; deeper levels off.
      pragma Assert ((Get (16#71240#) and 16#FFF#) = 900 and then Get (16#71244#) = 0);
      --  Back to the firmware mode (what Settings' revert would do).
      M.Change (Output, Wanted.Value, Current, Result, Failed);
      pragma Assert (Result = M.Applied and then Violations = 0 and then SCDC_Value = 0);
      pragma Assert (Get (16#61400#) = 16#9003_0006# and then Get (16#71190#) = 16#0437_077F#);
   end Check_Applied;

   procedure Check_Rollback is
      Current : P.Configuration;
      Wanted : P.Target_Result;
      Result : M.Outcome;
      Failed : M.Step;
   begin
      Firmware_1080p;
      Lock_Fault_Once := True;
      Current := M.Capture (Output);
      Wanted := P.Target (Current, UHD.Items (1).Timing, 15_360, 19_200, True, True);
      M.Change (Output, Current, Wanted.Value, Result, Failed);
      pragma Assert (Result = M.Rolled_Back and then Failed = M.PLL_Lock);
      pragma Assert (Violations = 0);
      pragma Assert (Get (R.DPLL_CFGCR0 (R.DPLL0)) = Current.CFGCR0);
      pragma Assert (Get (16#61000#) = 16#0897_077F# and then Get (16#71190#) = 16#0437_077F#);
      pragma Assert (Get (16#61400#) = 16#9003_0006# and then Get (16#71008#) = 16#C000_0000#);
      pragma Assert (Get (16#61200#) = 16#9000_1000#);  --  firmware infoframes back
      pragma Assert ((Get (R.DDI_Buffer_Control (R.Port_B)) and 16#8000_0000#) /= 0);

      Firmware_1080p;
      Pipe_Never_Starts := True;
      Current := M.Capture (Output);
      M.Change (Output, Current, Wanted.Value, Result, Failed);
      pragma Assert (Result = M.Rollback_Failed and then Failed = M.Pipe_On);

      Firmware_1080p;
      Underrun_On_Enable := True;
      Current := M.Capture (Output);
      M.Change (Output, Current, Wanted.Value, Result, Failed);
      pragma Assert (Result /= M.Applied and then Failed = M.Underrun);
   end Check_Rollback;
begin
   Check_Applied;
   Check_Rollback;
   Put_Line ("PASS: HDMI modeset 1080p60 -> 2160p60 -> 1080p60, ordering rules, rollback on failure");
end Modeset_Tests;
