package body Intel_GPU_Display_Decode with SPARK_Mode is

   function Field (Word : Unsigned_32; Shift : Natural; Mask : Unsigned_32) return Unsigned_32 is
     (Shift_Right (Word, Shift) and Mask)
     with Pre => Shift < 32;

   function Decode_Function (Word : Unsigned_32) return DDI_Function is
      Port_Field : constant Unsigned_32 := Field (Word, R.DDI_Select_Shift, R.DDI_Select_Mask);
      Mode_Field : constant Unsigned_32 := Field (Word, R.DDI_Mode_Shift, R.DDI_Mode_Mask);
      BPC_Field : constant Unsigned_32 := Field (Word, R.DDI_BPC_Shift, R.DDI_BPC_Mask);
      Width_Field : constant Unsigned_32 :=
        Field (Word, R.DDI_Port_Width_Shift, R.DDI_Port_Width_Mask);
      Result : DDI_Function;
   begin
      Result.Enabled := (Word and R.DDI_Function_Enable) /= 0;
      if Port_Field in 1 .. Unsigned_32 (R.DDI_Port'Pos (R.DDI_Port'Last)) + 1 then
         Result.Port := (Selected => True, Port => R.DDI_Port'Val (Port_Field - 1));
      end if;
      Result.Mode := (case Mode_Field is
                        when 0 => HDMI, when 1 => DVI, when 2 => DP_SST, when 3 => DP_MST,
                        when others => Unknown);
      Result.Depth := (case BPC_Field is
                         when 0 => BPC_8, when 1 => BPC_10, when 2 => BPC_6, when 3 => BPC_12,
                         when others => Unknown);
      Result.Hsync_High := (Word and R.DDI_Hsync_High) /= 0;
      Result.Vsync_High := (Word and R.DDI_Vsync_High) /= 0;
      Result.High_TMDS_Rate := (Word and R.DDI_High_TMDS_Char_Rate) /= 0;
      Result.Scrambling := (Word and R.DDI_HDMI_Scrambling) /= 0;
      Result.Lanes := Lane_Count (Width_Field + 1);
      return Result;
   end Decode_Function;

   function Encode_Function (Item : DDI_Function) return Unsigned_32 is
      Mode : constant Unsigned_32 :=
        (case Item.Mode is
           when HDMI => 0, when DVI => 1, when DP_SST => 2, when DP_MST => 3, when Unknown => 0);
      Depth : constant Unsigned_32 :=
        (case Item.Depth is
           when BPC_8 => 0, when BPC_10 => 1, when BPC_6 => 2, when BPC_12 => 3, when Unknown => 0);
   begin
      return (if Item.Enabled then R.DDI_Function_Enable else 0) +
        Port_Code (Item.Port.Port) * 2 ** R.DDI_Select_Shift +
        Mode * 2 ** R.DDI_Mode_Shift + Depth * 2 ** R.DDI_BPC_Shift +
        (if Item.Vsync_High then R.DDI_Vsync_High else 0) +
        (if Item.Hsync_High then R.DDI_Hsync_High else 0) +
        (if Item.High_TMDS_Rate then R.DDI_High_TMDS_Char_Rate else 0) +
        Unsigned_32 (Item.Lanes - 1) * 2 ** R.DDI_Port_Width_Shift +
        (if Item.Scrambling then R.DDI_HDMI_Scrambling else 0);
   end Encode_Function;

   function Decode_Dividers (CFGCR0, CFGCR1 : Unsigned_32) return Divider_Decoding is
      P_Field : constant Unsigned_32 := Field (CFGCR1, R.PDiv_Shift, R.PDiv_Mask);
      K_Field : constant Unsigned_32 := Field (CFGCR1, R.KDiv_Shift, R.KDiv_Mask);
      Item : PLL_Dividers;
   begin
      case P_Field is
         when 1 => Item.P := P2;
         when 2 => Item.P := P3;
         when 4 => Item.P := P5;
         when 8 => Item.P := P7;
         when others => return (Valid => False);
      end case;
      case K_Field is
         when 1 => Item.K := K1;
         when 2 => Item.K := K2;
         when 4 => Item.K := K3;
         when others => return (Valid => False);
      end case;
      Item.Integer_Part := DCO_Integer (CFGCR0 and R.DCO_Integer_Mask);
      Item.Fraction := DCO_Fraction (Field (CFGCR0, R.DCO_Fraction_Shift, R.DCO_Fraction_Mask));
      Item.Q_Mode := (CFGCR1 and R.QDiv_Mode) /= 0;
      Item.Ratio := Q_Ratio (Field (CFGCR1, R.QDiv_Ratio_Shift, R.QDiv_Ratio_Mask));
      return (True, Item);
   end Decode_Dividers;

   function Decode_Routing (Word : Unsigned_32; Port : R.DDI_Port) return Routing is
      PHY : constant Natural := R.DDI_Port'Pos (Port);
      Select_Field : constant Unsigned_32 :=
        Field (Word, R.DPCLKA_Select_Bits * PHY, R.DPCLKA_Select_Mask);
   begin
      return (Clock_On => (Word and Shift_Left (1, R.DPCLKA_Clock_Off_First + PHY)) = 0,
              PLL => (if Select_Field = 1 then R.DPLL1 else R.DPLL0),
              Other_PLL => Select_Field > 1);
   end Decode_Routing;

   function Decode_Crystal (DSSM : Unsigned_32) return Crystal is
     (case Field (DSSM, R.DSSM_Refclk_Shift, R.DSSM_Refclk_Mask) is
        when 0 => Crystal_24_MHz, when 1 => Crystal_19_2_MHz, when 2 => Crystal_38_4_MHz,
        when others => Unknown);

   function CDCLK_kHz (CDCLK_CTL, DE_PLL : Unsigned_32; Item : Crystal) return Clock_kHz is
      Ratio : constant Natural := Natural (DE_PLL and R.DE_PLL_Ratio_Mask);
      --  CD2X divider codes 0..3 divide the VCO by 2, 3, 4 and 8.
      Divisor : constant array (Unsigned_32 range 0 .. 3) of Positive := [2, 3, 4, 8];
   begin
      if Item = Unknown or else (DE_PLL and R.DE_PLL_Lock) = 0 then
         return 0;
      end if;
      return Ratio * Crystal_kHz (Item) /
        Divisor (Field (CDCLK_CTL, R.CD2X_Divider_Shift, R.CD2X_Divider_Mask));
   end CDCLK_kHz;
end Intel_GPU_Display_Decode;
