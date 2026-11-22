package body Intel_GPU_Display_Readout is
   use type D.Signal_Mode, D.Crystal;

   function Hex (Value : Unsigned_32) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
      Rest : Unsigned_32 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digits_Text (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Text;
   end Hex;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Port_Name (Port : R.DDI_Port) return String is
     (case Port is
        when R.Port_A => "A", when R.Port_B => "B", when R.Port_C => "C",
        when R.Port_TC1 => "TC1", when R.Port_TC2 => "TC2", when R.Port_TC3 => "TC3",
        when R.Port_TC4 => "TC4");
   function Mode_Name (Mode : D.Signal_Mode) return String is
     (case Mode is
        when D.HDMI => "HDMI", when D.DVI => "DVI", when D.DP_SST => "DP-SST",
        when D.DP_MST => "DP-MST", when D.Unknown => "unknown");
   function Depth_Name (Depth : D.Bits_Per_Component) return String is
     (case Depth is
        when D.BPC_8 => "8", when D.BPC_10 => "10", when D.BPC_6 => "6", when D.BPC_12 => "12",
        when D.Unknown => "unknown");
   function Crystal_Name (Item : D.Crystal) return String is
     (case Item is
        when D.Crystal_24_MHz => "24 MHz", when D.Crystal_19_2_MHz => "19.2 MHz",
        when D.Crystal_38_4_MHz => "38.4 MHz", when D.Unknown => "unknown");

   procedure Capture (Item : out Snapshot) is
   begin
      Item := (others => <>);
      for X in R.Transcoder loop
         declare
            Base : constant Unsigned_32 := R.Transcoder_Base (X);
            S : Transcoder_State renames Item.Transcoders (X);
         begin
            S.Configuration := Read_MMIO (R.TRANSCONF + R.Pipe_Stride * R.Transcoder'Pos (X));
            S.Running := S.Configuration /= Unsigned_32'Last and then
              (S.Configuration and R.Transcoder_Enable) /= 0;
            S.Function_Word := Read_MMIO (Base + R.TRANS_DDI_FUNC_CTL);
            S.Function_Of := D.Decode_Function
              ((if S.Function_Word = Unsigned_32'Last then 0 else S.Function_Word));
            S.Words := (H_Total => Read_MMIO (Base + R.TRANS_HTOTAL),
                        H_Blank => Read_MMIO (Base + R.TRANS_HBLANK),
                        H_Sync => Read_MMIO (Base + R.TRANS_HSYNC),
                        V_Total => Read_MMIO (Base + R.TRANS_VTOTAL),
                        V_Blank => Read_MMIO (Base + R.TRANS_VBLANK),
                        V_Sync => Read_MMIO (Base + R.TRANS_VSYNC),
                        Source => Read_MMIO (Base + R.PIPESRC));
            S.Timing := T.Decode (S.Words);
         end;
      end loop;
      for P in R.DDI_Port loop
         Item.Buffers (P) := Read_MMIO (R.DDI_Buffer_Control (P));
      end loop;
      for P in R.Combo_PLL loop
         Item.PLLs (P).Enable_Word := Read_MMIO (R.DPLL_Enable (P));
         Item.PLLs (P).CFGCR0 := Read_MMIO (R.DPLL_CFGCR0 (P));
         Item.PLLs (P).CFGCR1 := Read_MMIO (R.DPLL_CFGCR1 (P));
         Item.PLLs (P).Status := D.Decode_Enable
           ((if Item.PLLs (P).Enable_Word = Unsigned_32'Last then 0 else Item.PLLs (P).Enable_Word));
         Item.PLLs (P).Dividers := D.Decode_Dividers (Item.PLLs (P).CFGCR0, Item.PLLs (P).CFGCR1);
      end loop;
      Item.Routing_Word := Read_MMIO (R.ICL_DPCLKA_CFGCR0);
      Item.DSSM := Read_MMIO (R.SKL_DSSM);
      Item.CDCLK_Control := Read_MMIO (R.CDCLK_CTL);
      Item.DE_PLL := Read_MMIO (R.BXT_DE_PLL_ENABLE);
      Item.Crystal := (if Item.DSSM = Unsigned_32'Last then D.Unknown else D.Decode_Crystal (Item.DSSM));
      Item.CDCLK_kHz := (if Item.DE_PLL = Unsigned_32'Last then 0
                         else D.CDCLK_kHz (Item.CDCLK_Control, Item.DE_PLL, Item.Crystal));
   end Capture;

   function Lit (Item : Snapshot; Pipe : R.Transcoder) return Lit_Output is
      S : Transcoder_State renames Item.Transcoders (Pipe);
      F : D.DDI_Function renames S.Function_Of;
   begin
      if not S.Running or else not F.Enabled or else not F.Port.Selected then
         return (Found => False);
      end if;
      declare
         Port : constant R.DDI_Port := F.Port.Port;
         Routing : D.Routing;
         Pixel : D.Clock_kHz := 0;
      begin
         if R.Is_Combo (Port) then
            Routing := D.Decode_Routing (Item.Routing_Word, Port);
            if Routing.Clock_On and then not Routing.Other_PLL and then
              Item.PLLs (Routing.PLL).Status.Locked and then Item.PLLs (Routing.PLL).Dividers.Valid and then
              Item.Crystal /= D.Unknown and then F.Mode in D.HDMI | D.DVI
            then
               --  8 bpc TMDS: the port clock is the pixel clock.
               Pixel := D.Port_Clock_kHz (Item.PLLs (Routing.PLL).Dividers.Value,
                                          D.PLL_Reference (Item.Crystal), D.Fraction_Halved (Item.Crystal));
            end if;
         end if;
         return (Found => True, Port => Port, Mode => F.Mode, Timing => S.Timing,
                 PLL => Routing.PLL, Pixel_kHz => Pixel);
      end;
   end Lit;

   procedure Describe (Item : Snapshot) is
      function Axis_Text (A : T.Axis) return String is
        (Image (A.Active) & " total=" & Image (A.Total) & " blank=" & Image (A.Blank_Start) & "-" &
         Image (A.Blank_End) & " sync=" & Image (A.Sync_Start) & "-" & Image (A.Sync_End));
   begin
      for X in R.Transcoder loop
         declare
            S : Transcoder_State renames Item.Transcoders (X);
            F : D.DDI_Function renames S.Function_Of;
            Name : constant String := R.Transcoder'Image (X);
         begin
            Log ("intel-gpu: display readout transcoder " & Name & " transconf=" & Hex (S.Configuration) &
                 " ddi_func=" & Hex (S.Function_Word) & " running=" & Boolean'Image (S.Running) &
                 " function=" & Boolean'Image (F.Enabled));
            if S.Running or else F.Enabled then
               Log ("intel-gpu: display readout transcoder " & Name & " port=" &
                    (if F.Port.Selected then Port_Name (F.Port.Port) else "none") &
                    " mode=" & Mode_Name (F.Mode) & " bpc=" & Depth_Name (F.Depth) &
                    " sync=" & (if F.Hsync_High then "+h" else "-h") & (if F.Vsync_High then "+v" else "-v") &
                    " lanes=" & Image (F.Lanes) & " scrambling=" & Boolean'Image (F.Scrambling) &
                    " high_tmds=" & Boolean'Image (F.High_TMDS_Rate));
               Log ("intel-gpu: display readout transcoder " & Name & " h " & Axis_Text (S.Timing.Horizontal) &
                    " v " & Axis_Text (S.Timing.Vertical) & " src=" & Image (S.Timing.Source_Width) & "x" &
                    Image (S.Timing.Source_Height) & " plausible=" & Boolean'Image (T.Plausible (S.Timing)));
               Log ("intel-gpu: display readout transcoder " & Name & " words " & Hex (S.Words.H_Total) & " " &
                    Hex (S.Words.H_Blank) & " " & Hex (S.Words.H_Sync) & " " & Hex (S.Words.V_Total) & " " &
                    Hex (S.Words.V_Blank) & " " & Hex (S.Words.V_Sync) & " " & Hex (S.Words.Source));
            end if;
         end;
      end loop;
      for P in R.DDI_Port loop
         if Item.Buffers (P) /= Unsigned_32'Last and then (Item.Buffers (P) and R.DDI_Buffer_Enable) /= 0 then
            Log ("intel-gpu: display readout DDI " & Port_Name (P) & " buf_ctl=" & Hex (Item.Buffers (P)) &
                 " idle=" & Boolean'Image ((Item.Buffers (P) and R.DDI_Buffer_Idle) /= 0));
         end if;
      end loop;
      for P in R.Combo_PLL loop
         declare
            S : PLL_State renames Item.PLLs (P);
         begin
            Log ("intel-gpu: display readout " & R.Combo_PLL'Image (P) & " enable=" & Hex (S.Enable_Word) &
                 " locked=" & Boolean'Image (S.Status.Locked) & " cfgcr0=" & Hex (S.CFGCR0) &
                 " cfgcr1=" & Hex (S.CFGCR1) &
                 (if S.Dividers.Valid and then Item.Crystal /= D.Unknown then
                    " dco_khz=" & Image (D.DCO_kHz (S.Dividers.Value, D.PLL_Reference (Item.Crystal),
                                                     D.Fraction_Halved (Item.Crystal))) &
                    " port_khz=" & Image (D.Port_Clock_kHz (S.Dividers.Value, D.PLL_Reference (Item.Crystal),
                                                            D.Fraction_Halved (Item.Crystal)))
                  else " dividers=invalid"));
         end;
      end loop;
      Log ("intel-gpu: display readout dpclka_cfgcr0=" & Hex (Item.Routing_Word) & " crystal=" &
           Crystal_Name (Item.Crystal) & " cdclk_ctl=" & Hex (Item.CDCLK_Control) & " de_pll=" &
           Hex (Item.DE_PLL) & " cdclk_khz=" & Image (Item.CDCLK_kHz));
   end Describe;
end Intel_GPU_Display_Readout;
