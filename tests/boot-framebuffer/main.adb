pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Boot_Framebuffer; use Boot_Framebuffer;
with Interfaces; use Interfaces;
procedure Main is
   Limit : constant Address := 2 ** 44;
   Budget : constant Byte_Count := 16 * 1024 * 1024;
   Canonical : constant Raw_Description :=
     (Base => 16#E0000000#, Width => 1024, Height => 768, Pitch => 4096,
      Kind => 1, Depth => 32, Red_Position => 16, Red_Size => 8,
      Green_Position => 8, Green_Size => 8, Blue_Position => 0, Blue_Size => 8);
   Raw : Raw_Description := Canonical;
   Count : Natural := 0;
   procedure Check (R : Raw_Description; Expected : Status) is
      D : constant Result := Decode (R, Limit, Budget);
   begin
      pragma Assert (D.State = Expected);
      Count := Count + 1;
   end Check;
   procedure Check_End (Physical_End : Address; Expected : Status) is
      D : constant Result := Decode (Raw, Physical_End, Budget);
   begin
      pragma Assert (D.State = Expected);
      Count := Count + 1;
   end Check_End;
begin
   Check (Raw, Success);
   Raw.Width := 0; Check (Raw, Invalid_Geometry);
   Raw := Canonical; Raw.Pitch := 4095; Check (Raw, Invalid_Geometry);
   Raw := Canonical; Raw.Width := Unsigned_32'Last; Check (Raw, Invalid_Geometry);
   Raw := Canonical; Raw.Pitch := Unsigned_32'Last - 3; Check (Raw, Budget_Exceeded);
   Raw := Canonical; Raw.Depth := 24; Check (Raw, Unsupported_Format);
   Raw := Canonical; Raw.Red_Position := 0; Check (Raw, Unsupported_Format);
   Raw := Canonical; Raw.Base := Unsigned_64'Last; Check (Raw, Invalid_Address);
   Raw.Base := 0; Check (Raw, Invalid_Address);
   Raw := Canonical; Raw.Base := Unsigned_64 (Limit) - 4096; Check (Raw, Invalid_Address);
   Raw := (Base => 16#B8000#, Width => 80, Height => 25, Pitch => 160,
           Kind => 2, Depth => 16, others => 0);
   Check (Raw, Text_Mode);
   Raw.Base := 16#100000#; Check (Raw, Unsupported_Format);
   Raw := Canonical; Raw.Width := 1; Raw.Height := 1; Raw.Pitch := 4;
   Raw.Base := 4092;
   Check_End (4096, Success); -- exact last page, not needlessly rounded upward
   Check_End (4095, Invalid_Address);
   Raw.Base := 4093;
   Check_End (4097, Invalid_Address); -- bytes fit, page mapping does not
   Check_End (8192, Success);
   Raw.Base := 4096;
   Check_End (0, Invalid_Address);
   Raw := Canonical; Raw.Width := 2048; Raw.Height := 2048; Raw.Pitch := 8192;
   Check (Raw, Success); -- exactly the configured backend budget
   Raw.Height := 2049; Check (Raw, Budget_Exceeded);
   for Mask in Unsigned_8 loop
      Raw := Canonical; Raw.Red_Size := Mask;
      Check (Raw, (if Mask = 8 then Success else Unsupported_Format));
      Raw := Canonical; Raw.Green_Position := Mask;
      Check (Raw, (if Mask = 8 then Success else Unsupported_Format));
      Raw := Canonical; Raw.Blue_Position := Mask;
      Check (Raw, (if Mask = 0 then Success else Unsupported_Format));
      Raw := Canonical; Raw.Depth := Mask;
      Check (Raw, (if Mask = 32 then Success else Unsupported_Format));
   end loop;
   -- Independent pixel and page-rounding arithmetic over every byte alignment,
   -- including pitched rows; no use of the admission helper for the oracle.
   for Padding in 0 .. 3 loop
      for Offset in 0 .. 4095 loop
         Raw := Canonical;
         Raw.Base := Canonical.Base + Unsigned_64 (Offset);
         Raw.Width := 9; Raw.Height := 27; Raw.Pitch := 36 + Unsigned_32 (Padding * 4);
         declare
            D : constant Result := Decode (Raw, Limit, Budget);
            Last_Byte : constant Unsigned_64 := Raw.Base +
              Unsigned_64 (Raw.Pitch) * Unsigned_64 (Raw.Height) - 1;
         begin
            pragma Assert (D.State = Success);
            pragma Assert (Unsigned_64 (D.Value.Map_First) = Raw.Base / 4096 * 4096);
            pragma Assert (Unsigned_64 (D.Value.Map_Limit) = (Last_Byte / 4096 + 1) * 4096);
            pragma Assert (not Overlaps (D.Value, 0, D.Value.Map_First));
            pragma Assert (Overlaps (D.Value, D.Value.Map_First, D.Value.Map_First + 1));
            pragma Assert (not Overlaps (D.Value, D.Value.Map_Limit, Limit));
            for Y in 0 .. 26 loop
               for X in 0 .. 8 loop
                  pragma Assert (Pixel_Offset (D.Value, X, Y) = Y * Natural (Raw.Pitch) + X * 4);
               end loop;
            end loop;
            Count := Count + 1;
         end;
      end loop;
   end loop;
   Put_Line ("PASS framebuffer admission cases:" & Count'Image);
end Main;
