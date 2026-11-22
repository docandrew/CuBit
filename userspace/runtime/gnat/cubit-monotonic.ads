with Interfaces;
package CuBit.Monotonic is
   type Reading (Available : Boolean := False) is record
      case Available is
         when True => Microseconds : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Read return Reading;
   --  Monotonic time since boot on Milliseconds' epoch, read from the clock
   --  publication without a system call (CuBit.Published_Clock,
   --  docs/fast-clock.md): at the TSC's resolution where the kernel
   --  publishes the conversion, else at a tick's. Always available. No
   --  defined continuity across suspend yet. Resolution does not certify
   --  physical accuracy. Callers must bound waits independently to handle
   --  a stopped clock.

   --  Read's microseconds as one word: Unavailable_Us when the clock is not
   --  published. For latency stamps (LAT-002): no system call.
   Unavailable_Us : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   function Microseconds return Interfaces.Unsigned_64;

   function Milliseconds return Interfaces.Unsigned_64;
   --  The kernel's msTicks clock (which receive and call deadlines use),
   --  from the publication: never behind it, so a deadline
   --  Milliseconds + N does not expire early.
end CuBit.Monotonic;
