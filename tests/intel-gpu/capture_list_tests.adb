with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Capture_List; use Intel_GPU_Capture_List;
procedure Capture_List_Tests is
   type Word_Array is array (Positive range <>) of Unsigned_32;
   function Word (Data : Page; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Offset)) +
      Unsigned_32 (Data (Offset + 1)) * 256 +
      Unsigned_32 (Data (Offset + 2)) * 65536 +
      Unsigned_32 (Data (Offset + 3)) * 16777216);
   Empty : constant Descriptors (1 .. 0) := [others => <>];
   Items : Descriptors (17 .. 272);
   Encoded : Image;
begin
   Encoded := Encode (Empty);
   pragma Assert (Encoded.Valid and then Encoded.Bytes = Page'(others => 0));
   for I in Items'Range loop
      Items (I) := (Unsigned_32 (I * 4), I mod 16, (I / 16) mod 16);
   end loop;
   for Count in Natural range 1 .. 255 loop
      Encoded := Encode (Items (17 .. 16 + Count));
      pragma Assert (Encoded.Valid and then Word (Encoded.Bytes, 0) = Unsigned_32 (Count));
      for J in Natural range 0 .. Count - 1 loop
         pragma Assert (Word (Encoded.Bytes, 4 + J * 16) = Items (17 + J).Offset);
         pragma Assert (Word (Encoded.Bytes, 8 + J * 16) = 16#DEAD_F00D#);
         pragma Assert (Word (Encoded.Bytes, 12 + J * 16) =
           Unsigned_32 (Items (17 + J).Group_ID) * 4096 +
           Unsigned_32 (Items (17 + J).Instance) * 1048576);
         pragma Assert (Word (Encoded.Bytes, 16 + J * 16) = 0);
      end loop;
      pragma Assert (for all J in 4 + Count * 16 .. 4095 => Encoded.Bytes (J) = 0);
   end loop;
   Encoded := Encode (Items);
   pragma Assert (not Encoded.Valid and then Encoded.Bytes = Page'(others => 0));
   for Bad of Word_Array'([1, 2, 3, 16#0100_0000#, Unsigned_32'Last]) loop
      Items (20).Offset := Bad;
      Encoded := Encode (Items (17 .. 20));
      pragma Assert (not Encoded.Valid and then Encoded.Bytes = Page'(others => 0));
   end loop;
   Ada.Text_IO.Put_Line ("capture lists: PASS (empty, 255 lengths, flags, bounds)");
end Capture_List_Tests;
