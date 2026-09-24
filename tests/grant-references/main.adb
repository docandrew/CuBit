with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Grant_References; use CuBit.Grant_References;
procedure Main is
   type Generations is array (Positive range <>) of Generation;
   Values : constant Generations := [1, 2, 16#8000_0000#, Maximum_Generation];
begin
   for Slot in Global_Slot loop
      for Gen of Values loop
         declare
            Item : constant Reference := (Slot, Gen);
         begin
            pragma Assert (Valid_Wire (Encode (Item)));
            pragma Assert (Decode (Encode (Item)) = Item);
         end;
      end loop;
   end loop;
   pragma Assert (not Valid_Wire (0));
   pragma Assert (not Valid_Wire (Maximum_Slot));
   pragma Assert (not Valid_Wire (Wire_Field_Base - 1));
   pragma Assert (not Valid_Wire (Wire_Field_Base + Maximum_Slot + 1));
   pragma Assert (not Valid_Wire (Unsigned_64'Last));
   Put_Line ("PASS: canonical grant-reference codec");
end Main;
