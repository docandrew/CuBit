with Interfaces;
package CuBit.Channel_Bells is
   type Words is array (0 .. 3) of Interfaces.Unsigned_64;
   Calls : Natural := 0;
   procedure Note_Notice (Label : Interfaces.Unsigned_32;
      Length : Interfaces.Unsigned_8; W : Words; Is_Notice : out Boolean);
end CuBit.Channel_Bells;
