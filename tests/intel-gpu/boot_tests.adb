with Interfaces; use Interfaces;
with Intel_GPU_Boot; use Intel_GPU_Boot;
with Intel_GPU_Resources; use Intel_GPU_Resources;
procedure Boot_Tests is
   Valid : constant Words := [16#0000_0060_0000_0004#, 16#0000_0300_46D2_8086#, 16#10002#, 16#400003#];
   Data : Words;
begin
   pragma Assert (Decode (Valid).Status = Admitted);
   for Bit in 32 .. 63 loop
      Data := Valid; Data (3) := Data (3) or Shift_Left (Unsigned_64'(1), Bit);
      pragma Assert (Decode (Data).Status = Invalid_BAR);
   end loop;
   Data := Valid; Data (3) := 2;
   pragma Assert (Decode (Data).Status = Invalid_BAR);
   Data := Valid; Data (2) := 2;
   pragma Assert (Decode (Data).Status = Invalid_BAR);
   for Bit in 17 .. 63 loop
      Data := Valid; Data (2) := Data (2) or Shift_Left (Unsigned_64'(1), Bit);
      pragma Assert (Decode (Data).Status = Invalid_BAR);
   end loop;
   for Header in Unsigned_64 range 0 .. 255 loop
      Data := Valid; Data (1) := Data (1) or Shift_Left (Header, 48);
      pragma Assert ((Decode (Data).Status = Admitted) = ((Header and 127) = 0));
   end loop;
   Data := Valid; Data (3) := 1;
   pragma Assert (Decode (Data).Status = Invalid_BAR);
   Data := Valid; Data (1) := Data (1) or Shift_Left (Unsigned_64'(1), 56);
   pragma Assert (Decode (Data).Status = Invalid_BAR);
   Data := Valid; Data (1) := 0;
   pragma Assert (Decode (Data).Status = Unknown_Platform);
end Boot_Tests;
