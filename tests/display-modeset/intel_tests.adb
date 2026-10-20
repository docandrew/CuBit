--  Intel display units against i915-derived values: transcoder timing words,
--  DDI function / PLL / routing / clock decoders, mode admission, and the
--  GMBUS sequencer against a register model (EDID reads, SCDC writes, NAK,
--  timeout, unwritable page).
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID;
with CuBit.Monitor_EDID.Catalog;
with EDID_Fixtures;
with GMBUS_Model;
with Intel_GPU_Display_Registers;
with Intel_GPU_Transcoder_Timing;
with Intel_GPU_Display_Decode;
with Intel_GPU_Mode_Admission;
with Intel_GPU_GMBUS;
with Intel_GPU_Combo_WRPLL;
procedure Intel_Tests is
   package E renames CuBit.Monitor_EDID;
   package C renames CuBit.Monitor_EDID.Catalog;
   package R renames Intel_GPU_Display_Registers;
   package T renames Intel_GPU_Transcoder_Timing;
   package D renames Intel_GPU_Display_Decode;
   package A renames Intel_GPU_Mode_Admission;
   package W renames Intel_GPU_Combo_WRPLL;
   package G is new Intel_GPU_GMBUS (GMBUS_Model.Read, GMBUS_Model.Write, GMBUS_Model.Now_Us);
   use type T.Timing, D.DDI_Function, D.Signal_Mode, D.Bits_Per_Component, D.P_Divider, D.K_Divider, D.Crystal;
   use type R.DDI_Port, R.Combo_PLL, A.Verdict, A.Link_Kind, G.Status;

   procedure Check_Timing is
      UHD : constant C.Mode_List := C.Modes (EDID_Fixtures.UHD_HDMI_Base, EDID_Fixtures.UHD_HDMI_Extensions);
      M : constant T.Timing := T.From_EDID (UHD.Items (1).Timing);
      W : constant T.Words := T.Encode (M);
   begin
      --  i915 for 3840x2160@60 (CTA VIC 97): htotal 4400, hsync 4016..4104,
      --  vtotal 2250, vsync 2168..2178; words store each field minus one.
      pragma Assert (W.H_Total = 16#112F_0EFF#);
      pragma Assert (W.H_Blank = 16#112F_0EFF#);
      pragma Assert (W.H_Sync = 16#1007_0FAF#);
      pragma Assert (W.V_Total = 16#08C9_086F#);
      pragma Assert (W.V_Sync = 16#0881_0877#);
      pragma Assert (W.Source = 16#0EFF_086F#);
      pragma Assert (T.Decode (W) = M and then T.Plausible (M));
      pragma Assert (T.Refresh_Millihertz (M, 594_000) = 60_000);
      --  Garbage words decode (never raise); an all-ones read is implausible.
      pragma Assert (not T.Plausible (T.Decode ((others => Unsigned_32'Last))));
   end Check_Timing;

   procedure Check_Decode is
      F : D.DDI_Function;
      Div : D.Divider_Decoding;
   begin
      --  An HDMI 8 bpc function on DDI B, +H/+V sync, 4 lanes field (HDMI
      --  ignores it), as the NUC GOP might leave it.
      F := D.Decode_Function (16#9003_0006#);
      pragma Assert (F.Enabled and then F.Port.Selected and then F.Port.Port = R.Port_B);
      pragma Assert (F.Mode = D.HDMI and then F.Depth = D.BPC_8 and then F.Hsync_High and then F.Vsync_High);
      pragma Assert (F.Lanes = 4 and then not F.Scrambling);
      pragma Assert (D.Encode_Function (F) = 16#9003_0006#);
      F := D.Decode_Function (16#A200_0000#);
      pragma Assert (F.Port.Port = R.Port_TC1 and then F.Mode = D.DP_SST);
      pragma Assert (not D.Decode_Function (0).Port.Selected);
      --  A 148.5 MHz HDMI PLL from i915's WRPLL search with a 19.2 MHz
      --  reference: DCO 8910 MHz = (464 + 0x800/0x8000) * 19.2, P=3 K=2 Q=2
      --  (8910 / (5 * 3 * 2 * 2) = 148.5).
      Div := D.Decode_Dividers (464 + 16#800# * 2 ** 10, 2 * 2 ** 10 + 16#200# + 2 * 2 ** 6 + 2 * 2 ** 2);
      pragma Assert (Div.Valid and then Div.Value.P = D.P3 and then Div.Value.K = D.K2 and then Div.Value.Q_Mode);
      pragma Assert (D.DCO_kHz (Div.Value, 19_200, False) = 8_910_000);
      pragma Assert (D.Port_Clock_kHz (Div.Value, 19_200, False) = 148_500);
      --  The same PLL as programmed with a 38.4 MHz crystal (half fraction).
      Div := D.Decode_Dividers (464 + 16#400# * 2 ** 10, 2 * 2 ** 10 + 16#200# + 2 * 2 ** 6 + 2 * 2 ** 2);
      pragma Assert (D.Port_Clock_kHz (Div.Value, 19_200, True) = 148_500);
      Div := D.Decode_Dividers (464 + 16#800# * 2 ** 10, 2 * 2 ** 10 + 16#200# + 2 * 2 ** 6 + 2 * 2 ** 2);
      pragma Assert (D.Encode_CFGCR0 (Div.Value) = 464 + 16#800# * 2 ** 10);
      pragma Assert (not D.Decode_Dividers (0, 0).Valid);
      --  Routing: DDI B clock on, DPLL1 selected.
      pragma Assert (D.Decode_Routing (2#0100#, R.Port_B).Clock_On);
      pragma Assert (D.Decode_Routing (2#0100#, R.Port_B).PLL = R.DPLL1);
      pragma Assert (not D.Decode_Routing (2 ** 11, R.Port_B).Clock_On);
      --  38.4 MHz crystal, DE PLL ratio 16 locked, CD2X /1: 307.2 MHz.
      pragma Assert (D.Decode_Crystal (2 * 2 ** 29) = D.Crystal_38_4_MHz);
      pragma Assert (D.CDCLK_kHz (0, 16#4000_0010#, D.Crystal_38_4_MHz) = 307_200);
      pragma Assert (D.CDCLK_kHz (0, 16#0000_0010#, D.Crystal_38_4_MHz) = 0);
      pragma Assert (D.PLL_Reference (D.Crystal_38_4_MHz) = 19_200);
   end Check_Decode;

   procedure Check_Round_Trips is
      F : D.DDI_Function;
      Count : Natural := 0;
   begin
      for S in T.Count loop
         for F_Bits in 0 .. 16 loop
            declare
               Finish : constant T.Count := Natural'Min (2 ** F_Bits, T.Count'Last);
            begin
               pragma Assert (T.Low (T.Pack (S, Finish)) = S and then T.High (T.Pack (S, Finish)) = Finish);
               pragma Assert (T.Low (T.Pack (Finish, S)) = Finish and then T.High (T.Pack (Finish, S)) = S);
            end;
         end loop;
      end loop;
      for Port in R.DDI_Port loop
         for Mode in D.HDMI .. D.DP_MST loop
            for Depth in D.BPC_8 .. D.BPC_12 loop
               for Flags in 0 .. 31 loop
                  for Lanes in D.Lane_Count loop
                     F := (Enabled => Flags mod 2 = 1, Port => (True, Port), Mode => Mode, Depth => Depth,
                           Hsync_High => Flags / 2 mod 2 = 1, Vsync_High => Flags / 4 mod 2 = 1,
                           High_TMDS_Rate => Flags / 8 mod 2 = 1, Scrambling => Flags / 16 mod 2 = 1,
                           Lanes => Lanes);
                     pragma Assert (D.Decode_Function (D.Encode_Function (F)) = F);
                     Count := Count + 1;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Put_Line ("PASS: timing field and DDI function round trips," & Count'Image & " functions");
   end Check_Round_Trips;

   --  WRPLL dividers: i915's values for the common clocks, and for every
   --  TMDS clock (10 kHz steps) the programmed PLL reproduces the clock to
   --  within the 2**-15 fraction's rounding, with and without the halved
   --  fraction workaround.
   procedure Check_WRPLL is
      R594 : constant W.Search_Result := W.Search (594_000, 19_200, False);
      R148 : constant W.Search_Result := W.Search (148_500, 19_200, False);
      Halved : constant W.Search_Result := W.Search (594_000, 19_200, True);
      Checked : Natural := 0;
      type References is array (1 .. 2) of D.Reference_kHz;
      Both : constant References := [19_200, 24_000];
   begin
      pragma Assert (R594.Found and then R594.Dividers.Integer_Part = 464 and then
                     R594.Dividers.Fraction = 16#800# and then R594.Dividers.P = D.P3 and then
                     R594.Dividers.K = D.K1 and then not R594.Dividers.Q_Mode);
      pragma Assert (Halved.Found and then Halved.Dividers.Fraction = 16#400#);
      pragma Assert (R148.Found and then R148.Dividers.P = D.P2 and then R148.Dividers.K = D.K2 and then
                     R148.Dividers.Q_Mode and then R148.Dividers.Ratio = 3);
      pragma Assert (D.Encode_CFGCR1 (R148.Dividers) = 3 * 2 ** 10 + 16#200# + 2 * 2 ** 6 + 1 * 2 ** 2);
      for Clock in 2_500 .. 60_000 loop
         for Reference of Both loop
            for H in Boolean loop
               declare
                  Result : constant W.Search_Result := W.Search (Clock * 10, Reference, H);
               begin
                  if Result.Found then
                     declare
                        Back : constant Natural := D.Port_Clock_kHz (Result.Dividers, Reference, H);
                     begin
                        pragma Assert (abs (Back - Clock * 10) <= 2);
                        Checked := Checked + 1;
                     end;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      Put_Line ("PASS: WRPLL search," & Checked'Image & " clock/reference/workaround cases");
   end Check_WRPLL;

   procedure Check_Admission is
      UHD : constant C.Mode_List := C.Modes (EDID_Fixtures.UHD_HDMI_Base, EDID_Fixtures.UHD_HDMI_Extensions);
      Sink : constant C.Sink_Limits := C.Limits (EDID_Fixtures.UHD_HDMI_Base, EDID_Fixtures.UHD_HDMI_Extensions);
      Old : constant C.Sink_Limits := C.Limits (EDID_Fixtures.UHD_HDMI14_Base, EDID_Fixtures.UHD_HDMI14_Extensions);
      Fast : constant A.Facts := (R.Port_B, A.HDMI_Link, Sink, 307_200);
      Slow : constant A.Facts := (R.Port_B, A.HDMI_Link, Sink, 192_000);
      V : constant A.Verdict_Array := A.Judge_All (UHD, Fast);
      Modes_1080 : E.Timing := UHD.Items (1).Timing;
   begin
      pragma Assert (V (1) = A.Admitted and then A.Best (UHD, V) = 1);
      pragma Assert (A.Judge (UHD.Items (1).Timing, Slow) = A.Needs_Higher_CDCLK);
      pragma Assert (A.Judge (UHD.Items (1).Timing, (R.Port_B, A.HDMI_Link, Old, 307_200)) = A.Above_Sink_TMDS);
      pragma Assert (A.Judge (UHD.Items (1).Timing, (R.Port_TC1, A.HDMI_Link, Sink, 307_200)) = A.Type_C_Port);
      pragma Assert (A.Judge (UHD.Items (1).Timing, (R.Port_A, A.DVI_Link, Sink, 307_200)) = A.Above_Sink_TMDS);
      --  Firmware left an HDMI monitor in DVI mode (NUC, 2026-10-19): the
      --  link follows the sink, so 4K60 is admitted; a DVI sink stays DVI.
      pragma Assert (A.TMDS_Link (Sink) = A.HDMI_Link);
      pragma Assert (A.Judge (UHD.Items (1).Timing, (R.Port_B, A.TMDS_Link (Sink), Sink, 307_200)) = A.Admitted);
      pragma Assert
        (A.TMDS_Link (C.Limits (EDID_Fixtures.DVI_1080_Base, EDID_Fixtures.DVI_1080_Extensions)) = A.DVI_Link);
      --  The combo PHY hole: 510 MHz is refused, 533.25 MHz is not a hole.
      Modes_1080.Clock := 51_000;
      pragma Assert (A.Judge (Modes_1080, Fast) = A.In_Combo_Clock_Hole);
      Modes_1080.Clock := 53_325;
      pragma Assert (A.Judge (Modes_1080, Fast) = A.Admitted);
      --  Above 340 MHz without SCDC: refused.
      pragma Assert (A.Judge (UHD.Items (1).Timing,
                     (R.Port_B, A.HDMI_Link, (Sink with delta SCDC => False), 307_200))
                     = A.Needs_Scrambling_Without_SCDC);
      --  With a slow CDCLK the best admitted mode is the largest that fits.
      declare
         VS : constant A.Verdict_Array := A.Judge_All (UHD, Slow);
         B : constant Natural := A.Best (UHD, VS);
      begin
         pragma Assert (B /= 0 and then UHD.Items (B).Timing.Width = 3840 and then
                        UHD.Items (B).Timing.Clock = 29_700);
      end;
   end Check_Admission;

   procedure Check_GMBUS is
      UHD : constant C.Block := EDID_Fixtures.UHD_HDMI_Base;
      Ext : constant C.Block := EDID_Fixtures.UHD_HDMI_Extensions (1);
      Image : GMBUS_Model.Byte_Array (0 .. 511) := [others => 0];
      Data : G.Bytes (1 .. 128);
      Small : G.Bytes (1 .. 5);
      S : G.Status;
   begin
      for I in 0 .. 127 loop
         Image (I) := UHD (I);
         Image (128 + I) := Ext (I);
         Image (256 + I) := Unsigned_8 (I);
      end loop;
      GMBUS_Model.Reset (Image, 2);
      G.Read_EDID_Block (G.Pin_DDI_B, 0, Data, S);
      pragma Assert (S = G.OK and then GMBUS_Model.Released and then GMBUS_Model.Stops = 1);
      for I in 0 .. 127 loop pragma Assert (Data (I + 1) = UHD (I)); end loop;
      G.Read_EDID_Block (G.Pin_DDI_B, 1, Data, S);
      pragma Assert (S = G.OK);
      for I in 0 .. 127 loop pragma Assert (Data (I + 1) = Ext (I)); end loop;
      --  Block 2 uses the E-DDC segment pointer.
      G.Read_EDID_Block (G.Pin_DDI_B, 2, Data, S);
      pragma Assert (S = G.OK and then Data (11) = 10);
      --  Odd lengths read every byte.
      G.Read_Indexed (G.Pin_DDI_B, G.DDC_EDID, 8, Small, S);
      pragma Assert (S = G.OK and then Small (1) = UHD (8) and then Small (5) = UHD (12));
      --  SCDC: set TMDS_Config (0x20) to scrambling + 1/40 ratio, read it back.
      G.Write_Bytes (G.Pin_DDI_B, G.SCDC, [16#20#, 16#03#], S);
      pragma Assert (S = G.OK and then GMBUS_Model.SCDC_Register (16#20#) = 3);
      G.Read_Indexed (G.Pin_DDI_B, G.SCDC, 16#20#, Small (1 .. 1), S);
      pragma Assert (S = G.OK and then Small (1) = 3);
      pragma Assert (GMBUS_Model.Protocol_Errors = 0);
      --  Nothing on another pin: NAK, controller cleared and released.
      G.Read_EDID_Block (G.Pin_DDI_A, 0, Data, S);
      pragma Assert (S = G.NAK and then GMBUS_Model.Released);
      G.Read_EDID_Block (G.Pin_DDI_B, 0, Data, S);
      pragma Assert (S = G.OK);
      --  A stuck controller times out; an ungranted page reports it.
      GMBUS_Model.No_Ready := True;
      G.Read_EDID_Block (G.Pin_DDI_B, 0, Data, S);
      pragma Assert (S = G.Timeout_Ready);
      GMBUS_Model.Reset (Image, 2);
      GMBUS_Model.Not_Writable := True;
      G.Read_EDID_Block (G.Pin_DDI_B, 0, Data, S);
      pragma Assert (S = G.Not_Writable);
      pragma Assert (GMBUS_Model.Protocol_Errors = 0);
   end Check_GMBUS;
begin
   Check_Timing;
   Check_Decode;
   Check_Round_Trips;
   Check_WRPLL;
   Check_Admission;
   Check_GMBUS;
   Put_Line ("PASS: Intel display timing, decoders, mode admission and GMBUS");
end Intel_Tests;
