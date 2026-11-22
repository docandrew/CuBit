------------------------------------------------------------------------------
--  Hosted fixture: the clock reads 0, as this fixture's system-call mock
--  did before the time moved to the clock publication (ABI-002 slice 7).
------------------------------------------------------------------------------
package body CuBit.Monotonic is
   function Read return Reading is ((Available => True, Microseconds => 0));
   function Microseconds return Interfaces.Unsigned_64 is (0);
   function Milliseconds return Interfaces.Unsigned_64 is (0);
end CuBit.Monotonic;
