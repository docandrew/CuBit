pragma Ada_2022;
with HID_Axis_Resolution;
with HID_Motion_Fragments;
package HID_Axis_Motion with SPARK_Mode is
   subtype Delta_Value is Long_Long_Integer range -(2 ** 32 - 1) .. 2 ** 32 - 1;
   subtype Motion is HID_Motion_Fragments.Motion;
   procedure Scale
     (Delta_Counts : Delta_Value; Gain : HID_Axis_Resolution.Gain;
      Reversed : Boolean; Previous : Motion;
      Displacement, Remainder : out Motion)
   with Pre => abs Long_Long_Integer (Previous) < Gain.D,
     Post =>
       (if Reversed then (-Delta_Counts) * Gain.N else Delta_Counts * Gain.N)
          + Long_Long_Integer (Previous) =
          Long_Long_Integer (Displacement) * Gain.D + Long_Long_Integer (Remainder)
       and then abs Long_Long_Integer (Remainder) < Gain.D;
end HID_Axis_Motion;
