pragma Ada_2022;
with Ada.Text_IO;
with AML_Coercions.Strings;
with AML_Decode; use AML_Decode;
procedure String_Conversion_Tests is
   use type AML_Decode.Byte;
   package S renames AML_Coercions.Strings;
   Checks : Natural := 0;
   Hex : constant String := "0123456789ABCDEF";
   procedure Check (Data : Bytes; Expected : String) is
   begin
      Checks := Checks + 1;
      if Data'Length /= Expected'Length then raise Program_Error; end if;
      for I in 1 .. Data'Length loop
         if Data (Data'First + (I - 1)) /= Character'Pos (Expected (Expected'First + I - 1)) then
            raise Program_Error with Checks'Image;
         end if;
      end loop;
   end Check;
begin
   Check (S.From_Integer (0, Bits_32), "00000000");
   Check (S.From_Integer (0, Bits_64), "0000000000000000");
   Check (S.From_Integer (16#FEDC_BA98_7654_3210#, Bits_64), "FEDCBA9876543210");
   Check (S.From_Integer (16#FEDC_BA98_7654_3210#, Bits_32), "76543210");
   Check (S.From_Buffer (Bytes'(7 .. 6 => 0)), "");
   Check (S.From_Buffer (Bytes'(Positive'Last => 255)), "0xFF");
   for A in Byte loop
      for B in Byte loop
         Check (S.From_Buffer (Bytes'(17 => A, 18 => B)),
           "0x" & Hex (Natural (A) / 16 + 1) & Hex (Natural (A) mod 16 + 1) &
           " 0x" & Hex (Natural (B) / 16 + 1) & Hex (Natural (B) mod 16 + 1));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("String conversion checks:" & Checks'Image);
end String_Conversion_Tests;
