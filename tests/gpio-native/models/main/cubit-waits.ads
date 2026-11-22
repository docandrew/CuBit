pragma Ada_2022;
with Interfaces;
-- Hosted kernel-wait boundary for GPIO_Native_Wait; not runtime execution.
package CuBit.Waits is
   function Sleep_For (Milliseconds : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
end CuBit.Waits;
