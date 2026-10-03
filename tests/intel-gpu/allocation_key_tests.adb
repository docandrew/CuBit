with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
procedure Allocation_Key_Tests is
   package B renames Intel_GPU_Buffer_Backing;
   Key : Unsigned_64;
begin
   for Slot in 1 .. B.Bootstrap_Slots loop
      pragma Assert (B.Allocation_Key (Slot, 0) = 0);
      for Generation in Unsigned_32 range 1 .. 65_536 loop
         Key := B.Allocation_Key (Slot, Generation);
         pragma Assert (Key / 2 ** 32 = Unsigned_64 (Generation));
         pragma Assert (Key mod 2 ** 32 = Unsigned_64 (Slot));
         pragma Assert (Key /= Unsigned_64 (Slot)); -- old reply rejected
         pragma Assert (Key /= B.Allocation_Key (Slot, Generation - 1));
      end loop;
      Key := B.Allocation_Key (Slot, Unsigned_32'Last);
      pragma Assert (Key / 2 ** 32 = Unsigned_64 (Unsigned_32'Last));
      pragma Assert (Key mod 2 ** 32 = Unsigned_64 (Slot));
      pragma Assert (Key /= B.Allocation_Key (Slot, 1));
   end loop;
   Ada.Text_IO.Put_Line ("Allocation keys PASS: 16 slots x 65536 generations, zero/max boundaries, stale slot-only replies");
   Key := B.Allocation_Key (B.Slot'Last, Unsigned_32'Last);
   pragma Assert (Key mod 2 ** 32 = Unsigned_64 (B.Slot'Last));
   pragma Assert (Key / 2 ** 32 = Unsigned_64 (Unsigned_32'Last));
end Allocation_Key_Tests;
