with Interfaces;
package CuBit.Monotonic is
   type Reading (Available : Boolean := False) is record
      case Available is
         when True => Microseconds : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Read return Reading;
   --  High-resolution monotonic time since boot, read from the clock
   --  publication without a system call when the kernel publishes it
   --  (CuBit.Published_Clock, docs/fast-clock.md); then it shares Milliseconds' epoch.
   --  Otherwise READ_MONOTONIC_MICROSECONDS, in the HPET's own epoch. No
   --  defined continuity across suspend yet. Availability/resolution do not
   --  certify physical accuracy. Callers must bound waits independently to
   --  handle a stopped clock.

   function Milliseconds return Interfaces.Unsigned_64;
   --  GETTIME's clock (the kernel's msTicks, which receive and call deadlines
   --  use), from the publication when it is published: never behind GETTIME,
   --  so a deadline Milliseconds + N does not expire early.
end CuBit.Monotonic;
