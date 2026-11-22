--  Hosted stand-in for CuBit.System_Facts (the kernel's publications):
--  the wall clock comes from CUBIT_TEST_WALL_CLOCK (UTC seconds at
--  monotonic zero); unset means unknown (0).
with Interfaces; use Interfaces;
package CuBit.System_Facts is
   function Wall_Clock_Offset return Unsigned_64;
end CuBit.System_Facts;
