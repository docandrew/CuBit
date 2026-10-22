pragma Ada_2022;
with Bridge_Trace;
package body CuBit.Monotonic is
 function Milliseconds return Interfaces.Unsigned_64 is (Bridge_Trace.Now);
end CuBit.Monotonic;
