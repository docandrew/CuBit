package body HPET_Counter with SPARK_Mode is
   function Microseconds (Ticks : Unsigned_64; Period_FS : Tick_Period)
     return Unsigned_64 is
      Whole : constant Unsigned_64 := Ticks / 1_000_000_000;
      Fraction : constant Unsigned_64 := Ticks mod 1_000_000_000;
   begin
      -- Direct Ticks*Period overflows after modest uptimes. Split first.
      pragma Assert (Whole <= 18_446_744_073);
      pragma Assert (Fraction <= 999_999_999);
      pragma Assert (Whole <= Unsigned_64'Last / Period_FS);
      pragma Assert (Fraction <= Unsigned_64'Last / Period_FS);
      pragma Assert (Whole * Period_FS <= 1_844_674_407_300_000_000);
      pragma Assert (Fraction * Period_FS <= 99_999_999_900_000_000);
      pragma Assert (Whole * Period_FS <= Unsigned_64'Last -
        (Fraction * Period_FS) / 1_000_000_000);
      return Whole * Period_FS + (Fraction * Period_FS) / 1_000_000_000;
   end Microseconds;
end HPET_Counter;
