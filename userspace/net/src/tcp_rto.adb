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
         E.RTTVAR := R / 2;
         E.Measured := True;
      else
         Deviation := (if E.SRTT > R then E.SRTT - R else R - E.SRTT);
         --  RTTVAR := 3/4 RTTVAR + 1/4 |SRTT - R|
         E.RTTVAR := (3 * E.RTTVAR + Deviation) / 4;
         --  SRTT := 7/8 SRTT + 1/8 R
         E.SRTT := (7 * E.SRTT + R) / 8;
      end if;
      Spread := Unsigned_32'Max (Clock_Granularity, 4 * E.RTTVAR);
      E.RTO := Clamp (E.SRTT + Spread);
   end Update;

   procedure Back_Off (E : in out Estimator) is
   begin
      E.RTO := Unsigned_32'Min (2 * E.RTO, Maximum_RTO);
   end Back_Off;
end TCP_RTO;
