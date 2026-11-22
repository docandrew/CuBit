with Ada.Text_IO;
with Free_Block_Set; use Free_Block_Set;
procedure Main is
   Bits : Storage (1 .. 17);
   Item : Index;
   Changed, Found : Boolean;
begin
   Initialize (Bits);
   for I in 0 .. Leaves (Bits) - 1 loop
      Set_Membership (Bits, I, True, Changed);
      pragma Assert (Changed);
   end loop;
   for I in 0 .. Leaves (Bits) - 1 loop
      Find (Bits, Item, Found);
      pragma Assert (Found);
      Set_Membership (Bits, Item, False, Changed);
      pragma Assert (Changed);
   end loop;
   pragma Assert (Empty (Bits));
   Ada.Text_IO.Put_Line ("PASS free block set");
end Main;
