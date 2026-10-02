with Owned_Memory_Layout;
with Memory_Grants;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Layout_Tests is
   package L renames Owned_Memory_Layout;
   Count : Natural := 0;
   procedure Check (Base, Bytes : Unsigned_64) is
      -- Independent wide-integer end-point oracle; no modular wrap.
      B : constant Long_Long_Long_Integer := Long_Long_Long_Integer (Base);
      N : constant Long_Long_Long_Integer := Long_Long_Long_Integer (Bytes);
      Invalid : constant Boolean := N = 0 or B + N >
        Long_Long_Long_Integer (Unsigned_64'Last);
      Expected : constant Boolean := Invalid or else
        (B < Long_Long_Long_Integer (L.Limit) and
         B + N > Long_Long_Long_Integer (L.First));
   begin
      pragma Assert (L.Conflicts (Base, Bytes) = Expected);
      Count := Count + 1;
   end Check;
   type Values is array (Positive range <>) of Unsigned_64;
   Sizes : constant Values :=
     [0, 1, 2, 4095, 4096, 4097, L.Limit - L.First,
      L.Limit - L.First + 1, Unsigned_64'Last];
begin
   pragma Assert (Memory_Grants.Received_Region_Limit <= L.First);
   pragma Assert (L.Limit < 16#0000_6000_0000_0000#); -- framebuffer
   pragma Assert (not L.Conflicts (16#0000_5000_0000_0000#, 2 ** 32)); -- initrd
   pragma Assert (L.First mod 4096 = 0 and L.Limit mod 4096 = 0);
   for Offset in Unsigned_64 range 0 .. 8192 loop
      for Size of Sizes loop
         Check (L.First - 4096 + Offset, Size);
         Check (L.Limit - 4096 + Offset, Size);
         Check (Unsigned_64'Last - Offset, Size);
      end loop;
   end loop;
   Check (0, L.First);
   Check (0, L.First + 1);
   pragma Assert (not L.Conflicts (0, L.First));
   pragma Assert (not L.Conflicts (L.Limit, 4096));
   Ada.Text_IO.Put_Line ("owned memory layout PASS cases=" & Natural'Image (Count));
end Layout_Tests;
