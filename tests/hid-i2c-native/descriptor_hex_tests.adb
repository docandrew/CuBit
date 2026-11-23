pragma Ada_2022;
with Ada.Text_IO;
with HID_I2C_Descriptors;
with HID_Descriptor_Hex;
procedure Descriptor_Hex_Tests is
   package D renames HID_I2C_Descriptors;
   package H renames HID_Descriptor_Hex;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Value (C : Character) return Natural is
     (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
      elsif C in 'a' .. 'f' then Character'Pos (C) - Character'Pos ('a') + 10
      else raise Program_Error with "non-hex character");
   procedure Verify (Data : D.Bytes) is
      Offset, Lines : Natural := 0;
   begin
      while Offset < Data'Length loop
         declare Text : constant String := H.Chunk (Data, Offset); begin
            Check (Text'Length > 0 and Text'Length mod 2 = 0);
            Check (Text'Length <= 384);
            Check (Text'Length = 2 * Natural'Min (192, Data'Length - Offset));
            for J in 0 .. Text'Length / 2 - 1 loop
               Check (16 * Value (Text (Text'First + 2 * J))
                 + Value (Text (Text'First + 2 * J + 1)) =
                   Natural (Data (Data'First + Offset + J)));
            end loop;
            Offset := Offset + Text'Length / 2;
         end;
         Lines := Lines + 1;
      end loop;
      Check (Offset = Data'Length);
      Check (Lines = (if Data'Length = 0 then 0 else (Data'Length - 1) / 192 + 1));
      if Data'Length = 679 then Check (Lines = 4); end if;
   end Verify;
   Full : D.Bytes (0 .. 678);
   Edge : D.Bytes (Natural'Last - 191 .. Natural'Last);
begin
   Check (H.Encode ([]) = "");
   Check (H.Encode ([0, 1, 16#10#, 16#7F#, 16#80#, 16#FE#, 16#FF#]) =
     "0001107f80feff");
   for B in D.Byte loop
      declare Data : constant D.Bytes (Natural'Last .. Natural'Last) := [others => B];
         Text : constant String := H.Encode (Data);
      begin
         Check (Text'Length = 2);
         Check (16 * Value (Text (1)) + Value (Text (2)) = Natural (B));
      end;
   end loop;
   for I in Full'Range loop Full (I) := D.Byte (I mod 256); end loop;
   Verify (Full);
   for I in Edge'Range loop Edge (I) := D.Byte (I mod 256); end loop;
   Verify (Edge);
   for Length in 0 .. 679 loop
      Verify (Full (0 .. Length - 1));
   end loop;
   Ada.Text_IO.Put_Line ("Descriptor hex checks" & Checks'Image);
end Descriptor_Hex_Tests;
