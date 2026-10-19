package body Intel_GPU_Combo_WRPLL with SPARK_Mode is
   type Divider_Index is range 1 .. 46;
   Dividers : constant array (Divider_Index) of Positive :=
     [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24, 28, 30, 32, 36, 40,
      42, 44, 48, 50, 52, 54, 56, 60, 64, 66, 68, 70, 72, 76, 78, 80,
      84, 88, 90, 92, 96, 98, 100, 102, 3, 5, 7, 9, 15, 21];
   Fraction_Bits : constant := 15;

   type Split is record
      P : D.P_Divider := D.P2;
      Q : Natural range 1 .. 51 := 1;
      K : D.K_Divider := D.K1;
   end record;

   --  icl_wrpll_get_multipliers (every divider of the table takes one of
   --  these branches; the clamps only make the function total).
   subtype Divider_Value is Positive range 2 .. 102;
   function Multipliers (Divider : Divider_Value) return Split is
     (if Divider mod 2 = 0 then
        (if Divider = 2 then (D.P2, 1, D.K1)
         elsif Divider mod 4 = 0 then (D.P2, Divider / 4, D.K2)
         elsif Divider mod 6 = 0 then (D.P3, Divider / 6, D.K2)
         elsif Divider mod 5 = 0 then (D.P5, Natural'Max (1, Divider / 10), D.K2)
         else (D.P7, Natural'Max (1, Divider / 14), D.K2))
      else
        (if Divider = 3 then (D.P3, 1, D.K1)
         elsif Divider = 5 then (D.P5, 1, D.K1)
         elsif Divider = 7 then (D.P7, 1, D.K1)
         elsif Divider = 9 then (D.P3, 1, D.K3)
         elsif Divider = 15 then (D.P5, 1, D.K3)
         else (D.P7, 1, D.K3)));

   function Search (Clock : TMDS_kHz; Reference : D.Reference_kHz; Halved : Boolean)
      return Search_Result
   is
      AFE : constant Positive := Clock * AFE_Per_TMDS;
      Best : Natural := 0;
      Best_DCO : Natural := 0;
      Best_Centrality : Natural := Natural'Last;
   begin
      for I in Divider_Index loop
         pragma Loop_Invariant (if Best /= 0 then Best_DCO in DCO_Min_kHz .. DCO_Max_kHz);
         pragma Loop_Invariant (Best = 0 or else Best in Divider_Value);
         declare
            DCO : constant Long_Long_Integer := Long_Long_Integer (AFE) * Long_Long_Integer (Dividers (I));
         begin
            if DCO in DCO_Min_kHz .. DCO_Max_kHz and then
              abs (DCO - DCO_Mid_kHz) < Long_Long_Integer (Best_Centrality)
            then
               Best_Centrality := Natural (abs (DCO - DCO_Mid_kHz));
               Best := Dividers (I);
               Best_DCO := Natural (DCO);
            end if;
         end;
      end loop;
      if Best = 0 then
         return (Found => False);
      end if;
      declare
         S : constant Split := Multipliers (Best);
         --  DCO in units of the reference / 2**15.
         Scaled : constant Long_Long_Integer :=
           Long_Long_Integer (Best_DCO) * 2 ** Fraction_Bits / Long_Long_Integer (Reference);
         Fraction : constant Natural := Natural (Scaled mod 2 ** Fraction_Bits);
         Result : D.PLL_Dividers;
      begin
         Result.Integer_Part := D.DCO_Integer (Scaled / 2 ** Fraction_Bits);
         --  DIV_ROUND_CLOSEST (fraction, 2).
         Result.Fraction := (if Halved then (Fraction + 1) / 2 else Fraction);
         Result.P := S.P;
         Result.K := S.K;
         Result.Q_Mode := S.Q /= 1;
         Result.Ratio := S.Q;
         return (Found => True, Dividers => Result, DCO_kHz => Best_DCO);
      end;
   end Search;
end Intel_GPU_Combo_WRPLL;
