------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Published_Clock;

package body CuBit.Deadlines.Clock is

   function Now return Monotonic_Ms is
      Value : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
   begin
      return (if Value = Forever_Word then Monotonic_Ms'Last else Value);
   end Now;

   function After (Milliseconds : Unsigned_64) return Deadline is
      Start : constant Monotonic_Ms := Now;
   begin
      return At_Monotonic
        (if Milliseconds >= Monotonic_Ms'Last - Start then Monotonic_Ms'Last
         else Start + Milliseconds);
   end After;

end CuBit.Deadlines.Clock;
