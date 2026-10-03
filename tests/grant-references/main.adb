with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Grant_References; use CuBit.Grant_References;
with Memory_Grants;
with Owned_Memory_Layout;
procedure Main is
   type Generations is array (Positive range <>) of Generation;
   Values : constant Generations := [1, 2, 16#8000_0000#, Maximum_Generation];
begin
   --  These are separately compiled production contracts, not test copies of
   --  the constants. Namespace expansion must change both sides together.
   pragma Assert (Maximum_Slot = Unsigned_64 (Memory_Grants.Global_Slot'Last));
   pragma Assert (Maximum_Generation =
     Unsigned_64 (Memory_Grants.Grant_Generation'Last));
   pragma Assert (Memory_Grants.Received_Region_Limit <= Owned_Memory_Layout.First);
   -- The next fixed mapping is the bootstrap initrd, before owned memory.
   pragma Assert (Memory_Grants.Received_Region_Limit = 16#0000_5000_0000_0000#);
   pragma Assert (Memory_Grants.Received_Region_Limit =
     Memory_Grants.Received_Region_First +
       (Maximum_Slot + 1) * Memory_Grants.Grant_Slot_Bytes);
   for Slot in Global_Slot loop
      declare
         Kernel_Slot : constant Memory_Grants.Global_Slot :=
           Memory_Grants.Global_Slot (Slot);
         Base : constant Unsigned_64 := Memory_Grants.Received_Region_First +
           Slot * Memory_Grants.Grant_Slot_Bytes;
      begin
         pragma Assert (Memory_Grants.Make_Global_Slot
           (Memory_Grants.Owner_Of (Kernel_Slot),
            Memory_Grants.Local_Slot_Of (Kernel_Slot)) = Kernel_Slot);
         pragma Assert (Base >= Memory_Grants.Received_Region_First);
         pragma Assert (Memory_Grants.Grant_Slot_Bytes <=
           Memory_Grants.Received_Region_Limit - Base);
         pragma Assert (not Owned_Memory_Layout.Conflicts
           (Base, Memory_Grants.Grant_Slot_Bytes));
      end;
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
   Put_Line ("PASS: kernel/runtime grant namespace and owned-aperture separation");
end Main;
