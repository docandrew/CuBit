pragma Ada_2022;
with Interfaces;
package HID_Axis_Resolution with SPARK_Mode is
   subtype Unit_Exponent is Integer range -8 .. 7;
   subtype Span is Long_Long_Integer range 1 .. 2 ** 32 - 1;
   subtype Numerator is Long_Long_Integer range 1 .. 2 ** 30 - 1;
   subtype Denominator is Long_Long_Integer range 1 .. 2 ** 62 - 1;
   type Gain is record
      N : Numerator := 1;
      D : Denominator := 1;
   end record;
   type Status is (Normalized, Caller_Units, Unsupported_Unit, Capacity_Exceeded);
   -- Pointer policy: 1000 unaccelerated motion units per inch. HID SI linear
   -- length is centimetres; English linear length is inches (HID 6.2.2.7).
   Units_Per_Inch : constant := 1000;
   procedure Resolve
     (Logical_Span, Physical_Span : Span; Unit : Interfaces.Unsigned_32;
      Exponent : Unit_Exponent; Multiplier : Gain;
      Value : out Gain; Outcome : out Status);
   -- Exact rational arithmetic: cancel factors before multiplying. Unit zero
   -- retains the explicit caller calibration. Unsupported dimensions and ratios
   -- exceeding the named integer domains are reported, never rounded/clipped.
end HID_Axis_Resolution;
