------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Time with SPARK_Mode is

   --  Seconds * Per_Second + ceiling (Fraction / Per_Unit), saturating.
   function Scaled
     (Seconds : Integer_64; Per_Second : Unsigned_64; Fraction : Integer_64;
      Per_Unit : Unsigned_64) return Unsigned_64
   with Pre => Seconds >= 0 and then Fraction >= 0
               and then Per_Second in 1 .. Microseconds_Per_Second
               and then Per_Unit in 1 .. Nanoseconds_Per_Millisecond;

   function Scaled
     (Seconds : Integer_64; Per_Second : Unsigned_64; Fraction : Integer_64;
      Per_Unit : Unsigned_64) return Unsigned_64
   is
      Whole : constant Unsigned_64 := Unsigned_64 (Seconds);
      Part : constant Unsigned_64 :=
        Unsigned_64 (Fraction) / Per_Unit
        + (if Unsigned_64 (Fraction) mod Per_Unit = 0 then 0 else 1);
   begin
      if Whole > Unsigned_64'Last / Per_Second then
         return Forever;
      end if;
      return Saturating_Add (Whole * Per_Second, Part);
   end Scaled;

   function Milliseconds (T : Timespec) return Unsigned_64 is
     (Scaled (T.Seconds, Milliseconds_Per_Second, T.Nanoseconds,
              Nanoseconds_Per_Millisecond));

   function Microseconds (T : Timespec) return Unsigned_64 is
     (Scaled (T.Seconds, Microseconds_Per_Second, T.Nanoseconds,
              Nanoseconds_Per_Microsecond));

   function Milliseconds (T : Timeval) return Unsigned_64 is
     (Scaled (T.Seconds, Milliseconds_Per_Second, T.Microseconds,
              Microseconds_Per_Millisecond));

   function From_Milliseconds (Count : Unsigned_64) return Timespec is
      Whole : constant Unsigned_64 := Count / Milliseconds_Per_Second;
      Part : constant Unsigned_64 := Count mod Milliseconds_Per_Second;
   begin
      pragma Assert (Whole <= Unsigned_64'Last / Milliseconds_Per_Second);
      pragma Assert (Part < Milliseconds_Per_Second);
      declare
         Result : constant Timespec :=
           (Seconds => Integer_64 (Whole),
            Nanoseconds => Integer_64 (Part) * Nanoseconds_Per_Millisecond);
      begin
         pragma Assert (Integer_64 (Part) <= Milliseconds_Per_Second - 1);
         pragma Assert (Result.Nanoseconds <= (Milliseconds_Per_Second - 1) * Nanoseconds_Per_Millisecond);
         pragma Assert (Result.Seconds >= 0 and then Result.Nanoseconds >= 0);
         return Result;
      end;
   end From_Milliseconds;

   function From_Microseconds (Count : Unsigned_64) return Timespec is
      Whole : constant Unsigned_64 := Count / Microseconds_Per_Second;
      Part : constant Unsigned_64 := Count mod Microseconds_Per_Second;
   begin
      pragma Assert (Whole <= Unsigned_64'Last / Microseconds_Per_Second);
      pragma Assert (Part < Microseconds_Per_Second);
      declare
         Result : constant Timespec :=
           (Seconds => Integer_64 (Whole),
            Nanoseconds => Integer_64 (Part) * Nanoseconds_Per_Microsecond);
      begin
         pragma Assert (Integer_64 (Part) <= Microseconds_Per_Second - 1);
         pragma Assert (Result.Nanoseconds <= (Microseconds_Per_Second - 1) * Nanoseconds_Per_Microsecond);
         pragma Assert (Result.Seconds >= 0 and then Result.Nanoseconds >= 0);
         return Result;
      end;
   end From_Microseconds;

end CuBit.Libc_Time;
