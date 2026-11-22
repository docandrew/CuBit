pragma Ada_2022;
with Interfaces;
package CuBit.Handles is
   type Reply is new Interfaces.Unsigned_64;
   No_Reply : constant Reply := 0;
end CuBit.Handles;
