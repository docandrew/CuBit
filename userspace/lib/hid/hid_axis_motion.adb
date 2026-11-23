pragma Ada_2022;
package body HID_Axis_Motion with SPARK_Mode is
   procedure Scale
     (Delta_Counts : Delta_Value; Gain : HID_Axis_Resolution.Gain;
      Reversed : Boolean; Previous : Motion;
      Displacement, Remainder : out Motion)
   is
      Product : constant Long_Long_Integer := Delta_Counts * Gain.N;
      Total : constant Long_Long_Integer :=
        (if Reversed then -Product else Product) + Long_Long_Integer (Previous);
   begin
      Displacement := Motion (Total / Gain.D);
      Remainder := Motion (Total rem Gain.D);
   end Scale;
end HID_Axis_Motion;
