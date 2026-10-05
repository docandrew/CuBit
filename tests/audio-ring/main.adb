with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Audio_Ring;
procedure Main is
   Capacity : constant Unsigned_32 := CuBit.Audio_Ring.Data_Bytes;
   type Bytes is array (Natural range <>) of Unsigned_8;
   Ring : Bytes (0 .. Natural (Capacity) - 1);
   Checks : Natural := 0;
   Counts : constant array (1 .. 3) of Positive := (4, 256 * 4, Natural (Capacity));
   procedure Check (Start : Unsigned_32; Count : Natural) is
      --  Producer's two contiguous reservation spans, independent of the
      --  consumer's per-frame U32 counter arithmetic.
      Offset : constant Natural := Natural (Start mod Capacity);
      First : constant Natural := Natural'Min (Count, Ring'Length - Offset);
      Finish : constant Unsigned_32 := Start + Unsigned_32 (Count);
   begin
      Ring := (others => 0);
      for I in 0 .. Count - 1 loop
         if I < First then
            Ring (Offset + I) := Unsigned_8 (I mod 251);
         else
            Ring (I - First) := Unsigned_8 (I mod 251);
         end if;
      end loop;
      pragma Assert (Finish - Start = Unsigned_32 (Count));
      for I in 0 .. Count - 1 loop
         pragma Assert
           (Ring (Natural ((Start + Unsigned_32 (I)) mod Capacity)) =
              Unsigned_8 (I mod 251));
      end loop;
      Checks := Checks + 1;
   end Check;
begin
   pragma Assert (Capacity /= 0 and then (Capacity and (Capacity - 1)) = 0);
   pragma Assert (CuBit.Audio_Ring.Header_Bytes + Natural (Capacity) <=
                  CuBit.Audio_Ring.Allocation_Bytes);
   --  Every stereo-frame start through a full ring either side of U32 wrap,
   --  including partial writes, mixer periods, and a completely full ring.
   for Frame in 0 .. Natural (Capacity) / 2 loop
      for Count of Counts loop
         Check (Unsigned_32'Last - Capacity + 1 + Unsigned_32 (Frame * 4), Count);
      end loop;
   end loop;
   Put_Line ("Audio ring rollover PASS:" & Checks'Image & " cases");
end Main;
