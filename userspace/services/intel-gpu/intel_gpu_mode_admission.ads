with CuBit.Monitor_EDID;
with CuBit.Monitor_EDID.Catalog;
with Intel_GPU_Display_Registers;
--  Which EDID modes this display engine can drive on the lit output
--  (DISP-005): the monitor's claims intersected with the source's HDMI/TMDS
--  rules, the current CD clock and the plane limits. Every refusal has a
--  typed reason (shown in logs and Settings), never a silent drop. Rules
--  from i915 v6.16 for display version 13: intel_hdmi.c
--  (intel_hdmi_source_max_tmds_clock, hdmi_port_clock_valid,
--  intel_hdmi_mode_valid), intel_cdclk.c (intel_pixel_rate_to_cdclk: two
--  pixels per CD clock). EDID extents (at most 4095) are inside the
--  universal planes' 5120 x 4320 (skl_universal_plane.c).
package Intel_GPU_Mode_Admission with SPARK_Mode, Pure is
   package E renames CuBit.Monitor_EDID;
   package C renames CuBit.Monitor_EDID.Catalog;
   package R renames Intel_GPU_Display_Registers;
   use type E.Sync_Kind;

   --  How the lit port carries pixels (TRANS_DDI_FUNC_CTL mode select).
   type Link_Kind is (HDMI_Link, DVI_Link, DisplayPort_Link);

   --  kHz.
   subtype Clock_kHz is Natural range 0 .. 2 ** 31 - 1;
   --  Display version 13 HDMI TMDS ceiling.
   Source_Max_TMDS : constant Clock_kHz := 600_000;
   --  Single-link DVI.
   DVI_Max_TMDS : constant Clock_kHz := 165_000;
   --  Above this an HDMI 2.0 link scrambles and uses the 1/40 clock ratio
   --  (sink SCDC required).
   Scrambling_Threshold : constant Clock_kHz := 340_000;
   --  An HDMI sink that states no TMDS limit: HDMI 1.4's 340 MHz.
   HDMI_Default_Max_TMDS : constant Clock_kHz := 340_000;
   --  Combo PHY HDMI clock hole (exclusive bounds).
   Combo_Hole_Low : constant Clock_kHz := 500_000;
   Combo_Hole_High : constant Clock_kHz := 533_200;
   Pixels_Per_CDCLK : constant := 2;
   --  ADL-P's highest CD clock is 652.8 MHz; the range leaves room.
   subtype CD_Clock_kHz is Natural range 0 .. 2_000_000;

   type Facts is record
      Port : R.DDI_Port := R.Port_A;
      Link : Link_Kind := HDMI_Link;
      Sink : C.Sink_Limits;
      --  The CD clock now; modes needing more are refused (CuBit does not
      --  change CDCLK yet).
      CDCLK : CD_Clock_kHz := 0;
   end record;

   type Verdict is
     (Admitted,
      Not_Digital_Separate_Sync,
      Type_C_Port,
      DisplayPort_Link_Unsupported,
      Above_Source_TMDS,
      Above_Sink_TMDS,
      In_Combo_Clock_Hole,
      Needs_Scrambling_Without_SCDC,
      Needs_Higher_CDCLK);
   function Name (Item : Verdict) return String is
     (case Item is
        when Admitted => "admitted",
        when Not_Digital_Separate_Sync => "sync-not-digital-separate",
        when Type_C_Port => "type-c-port-unsupported",
        when DisplayPort_Link_Unsupported => "displayport-link-unsupported",
        when Above_Source_TMDS => "above-source-tmds",
        when Above_Sink_TMDS => "above-sink-tmds",
        when In_Combo_Clock_Hole => "in-combo-phy-clock-hole",
        when Needs_Scrambling_Without_SCDC => "needs-scrambling-sink-lacks-scdc",
        when Needs_Higher_CDCLK => "needs-higher-cdclk");

   --  kHz of an EDID timing (its clock is in 10 kHz).
   function Pixel_kHz (Mode : E.Timing) return Clock_kHz is (Natural (Mode.Clock) * 10);

   --  The link a TMDS port will carry. Firmware without a VBT often leaves
   --  an HDMI monitor in DVI mode (NUC, 2026-10-19); the modeset programs
   --  HDMI whenever the EDID says the sink is HDMI, so admission judges that.
   function TMDS_Link (Sink : CuBit.Monitor_EDID.Catalog.Sink_Limits) return Link_Kind is
     (if Sink.HDMI then HDMI_Link else DVI_Link);

   function Sink_Max_TMDS (Item : Facts) return Clock_kHz is
     (if Item.Link = DVI_Link or else not Item.Sink.HDMI then DVI_Max_TMDS
      elsif Item.Sink.Max_TMDS = 0 then HDMI_Default_Max_TMDS
      else Item.Sink.Max_TMDS);

   function Judge (Mode : E.Timing; Item : Facts) return Verdict is
     (if Mode.Sync.Kind /= E.Digital_Separate then Not_Digital_Separate_Sync
      elsif not R.Is_Combo (Item.Port) then Type_C_Port
      elsif Item.Link = DisplayPort_Link then DisplayPort_Link_Unsupported
      elsif Pixel_kHz (Mode) > Source_Max_TMDS then Above_Source_TMDS
      elsif Pixel_kHz (Mode) > Sink_Max_TMDS (Item) then Above_Sink_TMDS
      elsif Pixel_kHz (Mode) > Combo_Hole_Low and then Pixel_kHz (Mode) < Combo_Hole_High
      then In_Combo_Clock_Hole
      elsif Pixel_kHz (Mode) > Scrambling_Threshold and then not Item.Sink.SCDC
      then Needs_Scrambling_Without_SCDC
      elsif Pixel_kHz (Mode) > Pixels_Per_CDCLK * Item.CDCLK then Needs_Higher_CDCLK
      else Admitted)
     with Post =>
       (if Judge'Result = Admitted then
          Pixel_kHz (Mode) <= Source_Max_TMDS and then
          Pixel_kHz (Mode) <= Sink_Max_TMDS (Item) and then
          Pixel_kHz (Mode) <= Pixels_Per_CDCLK * Item.CDCLK and then
          not (Pixel_kHz (Mode) > Combo_Hole_Low and then Pixel_kHz (Mode) < Combo_Hole_High) and then
          (Pixel_kHz (Mode) <= Scrambling_Threshold or else Item.Sink.SCDC) and then
          R.Is_Combo (Item.Port) and then Item.Link /= DisplayPort_Link and then
          Mode.Sync.Kind = E.Digital_Separate);

   --  One verdict per mode of a catalog list.
   type Verdict_Array is array (Positive range <>) of Verdict;
   function Judge_All (List : C.Mode_List; Item : Facts) return Verdict_Array
     with Post => Judge_All'Result'First = 1 and then Judge_All'Result'Last = List.Count and then
       (for all I in 1 .. List.Count => Judge_All'Result (I) = Judge (List.Items (I).Timing, Item));

   --  The best admitted mode: the monitor's preferred mode when admitted,
   --  else the admitted mode with the most pixels, then the highest clock.
   --  Zero when none is admitted.
   function Best (List : C.Mode_List; Verdicts : Verdict_Array) return Natural
     with Pre => Verdicts'First = 1 and then Verdicts'Last = List.Count,
          Post => Best'Result <= List.Count and then
            (if Best'Result /= 0 then Verdicts (Best'Result) = Admitted);
end Intel_GPU_Mode_Admission;
