pragma Ada_2022;
with Interfaces;
-- Hosted wait boundary; no kernel Timer execution.
package CuBit.Waits is
   function Sleep_For (Milliseconds : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
end CuBit.Waits;
