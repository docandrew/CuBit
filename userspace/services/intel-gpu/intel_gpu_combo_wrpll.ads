with Intel_GPU_Display_Decode;
--  Combo PHY PLL dividers for an HDMI/DVI TMDS clock (i915 v6.16
--  intel_dpll_mgr.c icl_calc_wrpll, icl_wrpll_get_multipliers,
--  icl_wrpll_params_populate): AFE clock = 5 x TMDS clock; the DCO must lie
--  in [7998, 10000] MHz and the search keeps the divider whose DCO is
--  nearest the middle; the divider splits into P x Q x K.
package Intel_GPU_Combo_WRPLL with SPARK_Mode, Pure is
   package D renames Intel_GPU_Display_Decode;
   DCO_Min_kHz : constant := 7_998_000;
   DCO_Max_kHz : constant := 10_000_000;
   DCO_Mid_kHz : constant := (DCO_Min_kHz + DCO_Max_kHz) / 2;
   AFE_Per_TMDS : constant := 5;
   --  TMDS clocks the search accepts (25 MHz .. 600 MHz HDMI).
   subtype TMDS_kHz is Positive range 25_000 .. 600_000;

   type Search_Result (Found : Boolean := False) is record
      case Found is
         when True =>
            Dividers : D.PLL_Dividers;
            DCO_kHz : D.Clock_kHz;
         when False => null;
      end case;
   end record;

   --  Halved: store half the fraction (Display WA #22010492432, see
   --  Intel_GPU_Display_Decode.Fraction_Halved).
   function Search (Clock : TMDS_kHz; Reference : D.Reference_kHz; Halved : Boolean)
      return Search_Result
     with Post =>
       (if Search'Result.Found then
          Search'Result.DCO_kHz in DCO_Min_kHz .. DCO_Max_kHz and then
          (Search'Result.Dividers.Q_Mode = (Search'Result.Dividers.Ratio /= 1)) and then
          (if Search'Result.Dividers.Q_Mode then Search'Result.Dividers.Ratio >= 2));
end Intel_GPU_Combo_WRPLL;
