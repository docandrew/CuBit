with Interfaces; use Interfaces;
--  Display-engine registers outside the planes and cursor, as MMIO offsets
--  in the register BAR, for Alder Lake-P/N display version 13 (ADL-N is
--  SUBPLATFORM(alderlake_p, alderlake_n)). Offsets and fields are from the
--  Intel PRM (TGL/ADL-P Vol 2c) as Linux v6.16 i915 uses them (i915_reg.h,
--  intel_display_reg_defs.h, intel_dpll_mgr.c, intel_gmbus_regs.h); names
--  follow i915 so a reader can cross-check. Constants only: no access.
package Intel_GPU_Display_Registers with SPARK_Mode, Pure is
   subtype Register_Offset is Unsigned_32 range 0 .. 16#1F_FFFC#;
   Page_Bytes : constant := 4_096;

   ---------------------------------------------------------------------------
   --  Transcoders A..D, each feeding the pipe of the same letter (display
   --  version 12+ has no separate eDP transcoder).
   ---------------------------------------------------------------------------
   type Transcoder is (A, B, C, D);
   Transcoder_A_Base : constant := 16#6_0000#;
   Transcoder_Stride : constant := 16#1000#;
   function Transcoder_Base (Item : Transcoder) return Register_Offset is
     (Transcoder_A_Base + Transcoder_Stride * Transcoder'Pos (Item));

   --  Offsets within a transcoder's page.
   TRANS_HTOTAL : constant := 16#00#;
   TRANS_HBLANK : constant := 16#04#;
   TRANS_HSYNC : constant := 16#08#;
   TRANS_VTOTAL : constant := 16#0C#;
   TRANS_VBLANK : constant := 16#10#;
   TRANS_VSYNC : constant := 16#14#;
   PIPESRC : constant := 16#1C#;
   TRANS_VSYNCSHIFT : constant := 16#28#;
   TRANS_MULT : constant := 16#2C#;
   TRANS_DDI_FUNC_CTL : constant := 16#400#;
   TRANS_DDI_FUNC_CTL2 : constant := 16#404#;

   --  TRANS_DDI_FUNC_CTL fields.
   DDI_Function_Enable : constant Unsigned_32 := 16#8000_0000#;
   --  Bits 30:27 select the DDI as port number + 1 (zero: none).
   DDI_Select_Shift : constant := 27;
   DDI_Select_Mask : constant Unsigned_32 := 16#F#;
   DDI_Mode_Shift : constant := 24;
   DDI_Mode_Mask : constant Unsigned_32 := 7;
   DDI_BPC_Shift : constant := 20;
   DDI_BPC_Mask : constant Unsigned_32 := 7;
   DDI_Vsync_High : constant Unsigned_32 := 16#2_0000#;
   DDI_Hsync_High : constant Unsigned_32 := 16#1_0000#;
   DDI_High_TMDS_Char_Rate : constant Unsigned_32 := 16#10#;
   DDI_Port_Width_Shift : constant := 1;
   DDI_Port_Width_Mask : constant Unsigned_32 := 7;
   DDI_HDMI_Scrambling : constant Unsigned_32 := 16#1#;

   --  TRANSCONF lives in the pipe's page (Intel_GPU_Pipe_Pages): 0x70008.
   TRANSCONF : constant := 16#70008#;
   PIPE_MISC : constant := 16#70030#;
   Pipe_Stride : constant := 16#1000#;
   Transcoder_Enable : constant Unsigned_32 := 16#8000_0000#;
   Transcoder_State : constant Unsigned_32 := 16#4000_0000#;

   ---------------------------------------------------------------------------
   --  DDIs (ports). ADL-P/N wires A and B on combo PHYs and TC1..TC4 on
   --  Dekel Type-C PHYs (i915 PORT_TC1 = PORT_D).
   ---------------------------------------------------------------------------
   type DDI_Port is (Port_A, Port_B, Port_C, Port_TC1, Port_TC2, Port_TC3, Port_TC4);
   function Is_Combo (Port : DDI_Port) return Boolean is (Port in Port_A .. Port_B);
   DDI_BUF_CTL_A : constant := 16#6_4000#;
   DDI_Stride : constant := 16#100#;
   function DDI_Buffer_Control (Port : DDI_Port) return Register_Offset is
     (DDI_BUF_CTL_A + DDI_Stride * DDI_Port'Pos (Port));
   DDI_Buffer_Enable : constant Unsigned_32 := 16#8000_0000#;
   DDI_Buffer_Idle : constant Unsigned_32 := 16#80#;
   DDI_Buffer_Width_Shift : constant := 1;
   DDI_Buffer_Width_Mask : constant Unsigned_32 := 7;

   --  TRANS_CLK_SEL (one per transcoder, display page 0x46000): bits 31:28
   --  select the DDI clock as port number + 1.
   TRANS_CLK_SEL_A : constant := 16#4_6140#;
   Clock_Select_Stride : constant := 4;
   Transcoder_Clock_Shift : constant := 28;

   ---------------------------------------------------------------------------
   --  Combo PLLs DPLL0/DPLL1 (intel_dpll_mgr.c icl_dpll_write, TGL regs).
   ---------------------------------------------------------------------------
   type Combo_PLL is (DPLL0, DPLL1);
   DPLL_ENABLE_0 : constant := 16#4_6010#;
   DPLL_Enable_Stride : constant := 4;
   function DPLL_Enable (PLL : Combo_PLL) return Register_Offset is
     (DPLL_ENABLE_0 + DPLL_Enable_Stride * Combo_PLL'Pos (PLL));
   PLL_Enable : constant Unsigned_32 := 16#8000_0000#;
   PLL_Lock : constant Unsigned_32 := 16#4000_0000#;
   PLL_Power_Enable : constant Unsigned_32 := 16#0800_0000#;
   PLL_Power_State : constant Unsigned_32 := 16#0400_0000#;

   TGL_DPLL0_CFGCR0 : constant := 16#16_4284#;
   TGL_DPLL0_CFGCR1 : constant := 16#16_4288#;
   DPLL_CFGCR_Stride : constant := 8;
   function DPLL_CFGCR0 (PLL : Combo_PLL) return Register_Offset is
     (TGL_DPLL0_CFGCR0 + DPLL_CFGCR_Stride * Combo_PLL'Pos (PLL));
   function DPLL_CFGCR1 (PLL : Combo_PLL) return Register_Offset is
     (TGL_DPLL0_CFGCR1 + DPLL_CFGCR_Stride * Combo_PLL'Pos (PLL));
   --  CFGCR0: DCO integer 9:0, fraction 24:10 (units of 1/2**15).
   DCO_Integer_Mask : constant Unsigned_32 := 16#3FF#;
   DCO_Fraction_Shift : constant := 10;
   DCO_Fraction_Mask : constant Unsigned_32 := 16#7FFF#;
   DCO_Fraction_One : constant := 16#8000#;
   --  CFGCR1: Q divider ratio 17:10, Q mode 9, K divider 8:6, P divider
   --  5:2 (one-hot codes), central frequency select 1:0 (zero on TGL+:
   --  CFSELOVRD normal crystal).
   QDiv_Ratio_Shift : constant := 10;
   QDiv_Ratio_Mask : constant Unsigned_32 := 16#FF#;
   QDiv_Mode : constant Unsigned_32 := 16#200#;
   KDiv_Shift : constant := 6;
   KDiv_Mask : constant Unsigned_32 := 7;
   PDiv_Shift : constant := 2;
   PDiv_Mask : constant Unsigned_32 := 16#F#;

   --  DDI clock routing (ICL_DPCLKA_CFGCR0): per combo PHY a 2-bit DPLL
   --  select at bits 2*phy and a clock-off bit at 10 + phy.
   ICL_DPCLKA_CFGCR0 : constant := 16#16_4280#;
   DPCLKA_Select_Bits : constant := 2;
   DPCLKA_Select_Mask : constant Unsigned_32 := 3;
   DPCLKA_Clock_Off_First : constant := 10;

   ---------------------------------------------------------------------------
   --  Reference and CD clocks.
   ---------------------------------------------------------------------------
   SKL_DSSM : constant := 16#5_1004#;
   DSSM_Refclk_Shift : constant := 29;
   DSSM_Refclk_Mask : constant Unsigned_32 := 7;
   CDCLK_CTL : constant := 16#4_6000#;
   CD2X_Divider_Shift : constant := 22;
   CD2X_Divider_Mask : constant Unsigned_32 := 3;
   BXT_DE_PLL_ENABLE : constant := 16#4_6070#;
   DE_PLL_Ratio_Mask : constant Unsigned_32 := 16#FF#;
   DE_PLL_Lock : constant Unsigned_32 := 16#4000_0000#;

   ---------------------------------------------------------------------------
   --  GMBUS (PCH display, intel_gmbus_regs.h).
   ---------------------------------------------------------------------------
   GMBUS0 : constant := 16#C_5100#;
   GMBUS1 : constant := 16#C_5104#;
   GMBUS2 : constant := 16#C_5108#;
   GMBUS3 : constant := 16#C_510C#;
   GMBUS4 : constant := 16#C_5110#;
   GMBUS5 : constant := 16#C_5120#;
   GMBUS_Page : constant := 16#C_5000#;
end Intel_GPU_Display_Registers;
