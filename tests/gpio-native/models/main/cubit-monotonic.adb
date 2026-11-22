------------------------------------------------------------------------------
--  Hosted fixture: the clock is this model's scripted clock (its
--  CuBit.Messages stand-in), as the production code read it before the
--  time moved to the clock publication (ABI-002 slice 7).
------------------------------------------------------------------------------
with CuBit.Messages;
package body CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));
end CuBit.Monotonic;
