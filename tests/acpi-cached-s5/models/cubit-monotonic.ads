pragma Ada_2022;
with Interfaces; with S5_Boundary;
package CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64 is (S5_Boundary.Milliseconds);
end CuBit.Monotonic;
