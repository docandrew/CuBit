with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
--  Decoded display-engine state words (DISP-002 readout) and the words a
--  modeset writes, as values. No MMIO. Field layouts: Intel_GPU_Display_
--  Registers (i915 names). Unknown encodings decode to Unknown, never to a
--  guess.
package Intel_GPU_Display_Decode with SPARK_Mode, Pure is
   package R renames Intel_GPU_Display_Registers;

   ---------------------------------------------------------------------------
   --  TRANS_DDI_FUNC_CTL.
   ---------------------------------------------------------------------------
   type Port_Selection (Selected : Boolean := False) is record
      case Selected is
         when True => Port : R.DDI_Port := R.Port_A;
         when False => null;
      end case;
   end record;
   type Signal_Mode is (HDMI, DVI, DP_SST, DP_MST, Unknown);
   type Bits_Per_Component is (BPC_8, BPC_10, BPC_6, BPC_12, Unknown);
   subtype Lane_Count is Positive range 1 .. 8;
   type DDI_Function is record
      Enabled : Boolean := False;
      Port : Port_Selection;
      Mode : Signal_Mode := Unknown;
      Depth : Bits_Per_Component := Unknown;
      Hsync_High, Vsync_High : Boolean := False;
      High_TMDS_Rate, Scrambling : Boolean := False;
      Lanes : Lane_Count := 1;
   end record;
   function Decode_Function (Word : Unsigned_32) return DDI_Function;
   function Encode_Function (Item : DDI_Function) return Unsigned_32
     with Pre => Item.Port.Selected and then Item.Mode /= Unknown and then Item.Depth /= Unknown;
   --  Decode_Function (Encode_Function (F)) = F for every such F: tested
   --  exhaustively (all ports, modes, depths, flags and lane counts).

   function Port_Code (Port : R.DDI_Port) return Unsigned_32 is
     (Unsigned_32 (R.DDI_Port'Pos (Port)) + 1);

   ---------------------------------------------------------------------------
   --  Combo PLL: enable word and CFGCR0/1 (i915 icl_ddi_combo_pll_get_freq).
   ---------------------------------------------------------------------------
   type PLL_Status is record
      Enabled, Locked, Powered, Power_Requested : Boolean := False;
   end record;
   function Decode_Enable (Word : Unsigned_32) return PLL_Status is
     ((Enabled => (Word and R.PLL_Enable) /= 0, Locked => (Word and R.PLL_Lock) /= 0,
       Powered => (Word and R.PLL_Power_State) /= 0,
       Power_Requested => (Word and R.PLL_Power_Enable) /= 0));

   --  P divider codes 1/2/4/8 mean 2/3/5/7; K codes 1/2/4 mean 1/2/3.
   type P_Divider is (P2, P3, P5, P7);
   type K_Divider is (K1, K2, K3);
   P_Value : constant array (P_Divider) of Positive := [2, 3, 5, 7];
   K_Value : constant array (K_Divider) of Positive := [1, 2, 3];
   P_Code : constant array (P_Divider) of Unsigned_32 := [1, 2, 4, 8];
   K_Code : constant array (K_Divider) of Unsigned_32 := [1, 2, 4];
   subtype Q_Ratio is Natural range 0 .. 255;
   subtype DCO_Integer is Natural range 0 .. 1023;
   subtype DCO_Fraction is Natural range 0 .. 16#7FFF#;
   type PLL_Dividers is record
      Integer_Part : DCO_Integer := 0;
      Fraction : DCO_Fraction := 0;
      P : P_Divider := P2;
      K : K_Divider := K1;
      --  Q mode off: Q = 1 (Ratio ignored).
      Q_Mode : Boolean := False;
      Ratio : Q_Ratio := 0;
   end record;
   type Divider_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : PLL_Dividers;
         when False => null;
      end case;
   end record;
   function Decode_Dividers (CFGCR0, CFGCR1 : Unsigned_32) return Divider_Decoding;
   function Encode_CFGCR0 (Item : PLL_Dividers) return Unsigned_32 is
     (Unsigned_32 (Item.Integer_Part) + Unsigned_32 (Item.Fraction) * 2 ** R.DCO_Fraction_Shift);
   function Encode_CFGCR1 (Item : PLL_Dividers) return Unsigned_32 is
     (Unsigned_32 (Item.Ratio) * 2 ** R.QDiv_Ratio_Shift +
      (if Item.Q_Mode then R.QDiv_Mode else 0) +
      K_Code (Item.K) * 2 ** R.KDiv_Shift + P_Code (Item.P) * 2 ** R.PDiv_Shift);

   --  The PLL's reference: 19.2 or 24 MHz (a 38.4 MHz crystal is halved by
   --  the PLL, i915 icl_wrpll_ref_clock).
   subtype Reference_kHz is Positive range 19_200 .. 24_000;
   --  DCO frequency in kHz and the port (TMDS or link symbol) clock:
   --  DCO / (5 * P * Q * K). Zero for a Q-mode ratio of zero. Halved: the
   --  CFGCR0 fraction holds half the DCO fraction (Display WA #22010492432:
   --  TGL, ADL-S and ADL-P with a 38.4 MHz reference; i915
   --  ehl_combo_pll_div_frac_wa_needed).
   subtype Clock_kHz is Natural range 0 .. 2 ** 31 - 1;
   function DCO_kHz (Item : PLL_Dividers; Reference : Reference_kHz; Halved : Boolean)
      return Clock_kHz is
     (Item.Integer_Part * Reference +
        Item.Fraction * (if Halved then 2 else 1) * (Reference / 64) / (R.DCO_Fraction_One / 64));
   function Port_Clock_kHz (Item : PLL_Dividers; Reference : Reference_kHz; Halved : Boolean)
      return Clock_kHz is
     (if Item.Q_Mode and then Item.Ratio = 0 then 0
      else DCO_kHz (Item, Reference, Halved) /
        (5 * P_Value (Item.P) * K_Value (Item.K) * (if Item.Q_Mode then Item.Ratio else 1)));

   ---------------------------------------------------------------------------
   --  Clock routing of a combo DDI (ICL_DPCLKA_CFGCR0).
   ---------------------------------------------------------------------------
   type Routing is record
      Clock_On : Boolean := False;
      PLL : R.Combo_PLL := R.DPLL0;
      --  The select field names a PLL other than DPLL0/1.
      Other_PLL : Boolean := False;
   end record;
   function Decode_Routing (Word : Unsigned_32; Port : R.DDI_Port) return Routing
     with Pre => R.Is_Combo (Port);

   ---------------------------------------------------------------------------
   --  Reference clock (SKL_DSSM) and CD clock (CDCLK_CTL + DE PLL ratio).
   ---------------------------------------------------------------------------
   type Crystal is (Crystal_24_MHz, Crystal_19_2_MHz, Crystal_38_4_MHz, Unknown);
   function Decode_Crystal (DSSM : Unsigned_32) return Crystal;
   Crystal_kHz : constant array (Crystal range Crystal_24_MHz .. Crystal_38_4_MHz) of Positive :=
     [24_000, 19_200, 38_400];
   function PLL_Reference (Item : Crystal) return Reference_kHz is
     (case Item is
        when Crystal_24_MHz => 24_000,
        when Crystal_19_2_MHz | Crystal_38_4_MHz | Unknown => 19_200)
     with Pre => Item /= Unknown;
   function Fraction_Halved (Item : Crystal) return Boolean is (Item = Crystal_38_4_MHz);
   --  CDCLK = DE PLL ratio * crystal / CD2X divider (i915 icl_get_cdclk):
   --  zero when the DE PLL is unlocked or the crystal is unknown.
   function CDCLK_kHz (CDCLK_CTL, DE_PLL : Unsigned_32; Item : Crystal) return Clock_kHz;
end Intel_GPU_Display_Decode;
