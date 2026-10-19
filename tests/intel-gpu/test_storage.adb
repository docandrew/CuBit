with System.Storage_Elements; use System.Storage_Elements;
package body Test_Storage is
   type Words is array (Positive range <>) of Unsigned_64;
   type Words_Access is access Words;
   function Allocate (Bytes : Unsigned_64) return System.Address is
      Block : constant Words_Access :=
        new Words'(1 .. Positive (Bytes / 8) + Alignment / 8 + 1 => 0);
      Start : constant Integer_Address := To_Integer (Block.all'Address);
   begin
      return To_Address (Start + (Alignment - Start mod Alignment) mod Alignment);
   end Allocate;
end Test_Storage;
