------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Deadlines relative to now, from the published monotonic clock.
------------------------------------------------------------------------------
pragma Ada_2022;
package CuBit.Deadlines.Clock is

   --  Milliseconds from now. A span past the clock's range saturates at the
   --  last representable millisecond: it never becomes Forever.
   function After (Milliseconds : Unsigned_64) return Deadline;

   --  The monotonic clock now, in milliseconds.
   function Now return Monotonic_Ms;

end CuBit.Deadlines.Clock;
