with Interfaces;
package CuBit.Channel_Bells is
   type Words is array (0 .. 3) of Interfaces.Unsigned_64;
   procedure Note_Notice (Label : Interfaces.Unsigned_32; Length : Interfaces.Unsigned_8; Data : Words; Noted : out Boolean);
end CuBit.Channel_Bells;
