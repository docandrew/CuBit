pragma Ada_2022;

package body Charts_Palette with SPARK_Mode is
   Channel_Bits : constant := 8;
   Channel_Mask : constant := 16#FF#;

   function Channel (C : Color; Shift : Natural) return Unsigned_32 is
     (Shift_Right (C, Shift) and Channel_Mask)
     with Pre => Shift in 0 | Channel_Bits | 2 * Channel_Bits, Post => Channel'Result <= Channel_Mask;

   function Mix (Over, Under : Color; Alpha_Percent : Percentage) return Color is
      A : constant Unsigned_32 := Unsigned_32 (Alpha_Percent);
      Result : Unsigned_32 := 0;
   begin
      for Index in 0 .. 2 loop
         declare
            Shift : constant Natural := Index * Channel_Bits;
            Mixed : constant Unsigned_32 :=
              (Channel (Over, Shift) * A + Channel (Under, Shift) * (Percent - A) + Percent / 2) / Percent;
         begin
            pragma Assert (Mixed <= Channel_Mask);
            Result := Result or Shift_Left (Mixed, Shift);
         end;
      end loop;
      return Result and 16#FF_FFFF#;
   end Mix;

   function Step_For (Value, Top : Unsigned_64) return Ramp_Index is
     (if Value >= Top then Ramp_Steps
      else Ramp_Index (1 + Value * Unsigned_64 (Ramp_Steps - 1) / Top));
end Charts_Palette;
