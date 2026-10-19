with Interfaces; use Interfaces;
with Intel_GPU_Modeset_Plan;
--  DISP-006/008: change the mode of the HDMI output the firmware lit (combo
--  PHY port, its transcoder and combo PLL), and put the firmware mode back
--  when any step fails. Order from i915 v6.16 for display version 13:
--
--  Disable (hsw_crtc_disable, intel_ddi_post_disable_hdmi, combo_pll_disable):
--    plane 1 off (latched) -> TRANSCONF off, wait for the pipe to stop ->
--    TRANS_DDI_FUNC_CTL off -> sink scrambling off (SCDC) -> infoframes off
--    -> DDI_BUF_CTL off, wait idle -> TRANS_CLK_SEL none -> DDI clock off
--    (DPCLKA) -> PLL disable, wait unlock -> PLL power off, wait.
--  Enable (combo_pll_enable, intel_ddi_pre_enable_hdmi, hsw_crtc_enable,
--  intel_ddi_enable, intel_enable_ddi_hdmi):
--    PLL power on, wait -> CFGCR0/1 -> PLL enable, wait lock -> DPCLKA select
--    then clock on (two writes) -> TRANS_CLK_SEL -> infoframes -> PIPESRC and
--    timings -> plane 1 (stride, position, size, offset, watermarks, control,
--    surface) -> TRANS_DDI_FUNC_CTL on -> TRANSCONF on, wait running -> sink
--    SCDC TMDS configuration (TMDS still off) -> 2 ms -> DDI_BUF_CTL on,
--    wait active -> frame counter and scanline moving, no FIFO underrun.
--
--  Every register step is logged with the value written and what was read
--  back, so one hardware boot log shows exactly where a failure happened.
generic
   --  Display MMIO read (absolute offset; read-only BAR map).
   with function Read (Offset : Unsigned_32) return Unsigned_32;
   --  Allow-listed display MMIO write (absolute offset); Written False when
   --  the page is not mapped or the register is not this output's.
   with procedure Write (Offset, Value : Unsigned_32; Written : out Boolean);
   with function Now_Us return Unsigned_64;
   --  About one millisecond, never a spin.
   with procedure Pause;
   --  Write the sink's SCDC TMDS_Config over the port's DDC.
   with procedure Write_TMDS_Config (Value : Unsigned_8; Done : out Boolean);
   with procedure Log (Text : String);
package Intel_GPU_HDMI_Modeset is
   package P renames Intel_GPU_Modeset_Plan;

   --  The output's live configuration (the firmware's, before a change).
   function Capture (Item : P.Output) return P.Configuration;

   type Step is
     (None, Plane_Off, Pipe_Off, Function_Off, Sink_Scrambling_Off, Infoframes_Off, Buffer_Off,
      Transcoder_Clock_Off, Port_Clock_Off, PLL_Disable, PLL_Power_Off,
      PLL_Power_On, PLL_Configure, PLL_Lock, Port_Clock_Select, Port_Clock_On, Transcoder_Clock,
      Infoframes, Timings, Plane, Function_On, Pipe_On, Sink_TMDS_Config, Buffer_On,
      Frame_Counter, Scanline, Underrun);
   function Name (Item : Step) return String;

   type Outcome is (Applied, Rolled_Back, Rollback_Failed);
   --  Change Item from Current to Wanted. On a failed step, put Current
   --  back (Rolled_Back) and say which step failed; Rollback_Failed means
   --  the firmware mode could not be restored either (the output is dark).
   procedure Change
     (Item : P.Output; Current, Wanted : P.Configuration;
      Result : out Outcome; Failed : out Step);
end Intel_GPU_HDMI_Modeset;
