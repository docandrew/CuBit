------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_RTO with SPARK_Mode is

   function Clamp (Value : Unsigned_32) return RTO_Value is
     (if Value < Minimum_RTO then Minimum_RTO
      elsif Value > Maximum_RTO then Maximum_RTO
      else Value);

   procedure Update (E : in out Estimator; R : Sample) is
      Deviation : Sample;
      Spread    : Unsigned_32;
   begin
      if not E.Measured then
         E.SRTT := R;
         E.RTTVAR := R / First_RTTVAR_Divisor;
         E.Measured := True;
      else
         Deviation := (if E.SRTT > R then E.SRTT - R else R - E.SRTT);
         --  RTTVAR := (1 - beta) RTTVAR + beta |SRTT - R|
         E.RTTVAR := ((Beta_Divisor - 1) * E.RTTVAR + Deviation) / Beta_Divisor;
         --  SRTT := (1 - alpha) SRTT + alpha R
         E.SRTT := ((Alpha_Divisor - 1) * E.SRTT + R) / Alpha_Divisor;
      end if;
      Spread := Unsigned_32'Max (Clock_Granularity, K * E.RTTVAR);
      E.RTO := Clamp (E.SRTT + Spread);
   end Update;

   procedure Back_Off (E : in out Estimator) is
   begin
      E.RTO := Unsigned_32'Min (Backoff_Factor * E.RTO, Maximum_RTO);
   end Back_Off;
end TCP_RTO;
