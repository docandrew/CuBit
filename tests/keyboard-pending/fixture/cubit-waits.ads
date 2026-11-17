--  Hosted fixture: WAIT's sleep (ABI-002 slice 2) as the driver uses it.
with Interfaces; use Interfaces;
package CuBit.Waits is
   function Sleep_For (Milliseconds : Unsigned_64) return Unsigned_64 is (0);
end CuBit.Waits;
