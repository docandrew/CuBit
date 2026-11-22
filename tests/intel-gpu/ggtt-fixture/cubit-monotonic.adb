------------------------------------------------------------------------------
--  Hosted fixture: the clock is this fixture's scripted clock (its
--  CuBit.Messages mock), as the production code read it before the time
--  moved to the clock publication (ABI-002 slice 7).
------------------------------------------------------------------------------
with CuBit.Messages;
package body CuBit.Monotonic is
   use type Interfaces.Unsigned_64;
   function Milliseconds return Interfaces.Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));
   function Microseconds return Interfaces.Unsigned_64 is (Milliseconds * 1_000);
   function Read return Reading is ((Available => True, Microseconds => Microseconds));
end CuBit.Monotonic;
