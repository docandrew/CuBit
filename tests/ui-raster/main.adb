with Ada.Text_IO;
with Interfaces; use Interfaces;
with Client_Raster; use Client_Raster;
with Glyph_Gamma;

--  Client_Raster against a naive per-pixel reference: random rectangles in
--  random surfaces, every pixel compared, inside and outside the rectangle.
procedure Main is
   Seed : Unsigned_32 := 16#C0B1_7EED#;
   function Next return Unsigned_32 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Seed;
   end Next;
   function Below (N : Positive) return Natural is (Natural (Next mod Unsigned_32 (N)));
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
   end Check;
   Glyph : Mask;
   Ink : constant Word := 16#123456#;
   Ink_Row : constant Glyph_Gamma.Bucket := Glyph_Gamma.Bucket_Of (Ink);
begin
   --  Every ramp entry is the corrected blend of its coverage.
   declare
      Under : constant Word := 16#ABCDEF#;
      Over : constant Ramp := Shades (Ink, Under);
   begin
      for A in Byte loop
         Check (Over (A) = Mix (Ink, Under, Glyph_Gamma.Adjust (Ink_Row, A)), "ramp matches the blend");
      end loop;
   end;
   for Row in Mask_Row loop
      for Column in Mask_Column loop
         Glyph (Row, Column) := Byte (Below (4) * 85);
      end loop;
   end loop;
   for Round in 1 .. 2_000 loop
      declare
         Pitch : constant Positive := 1 + Below (80);
         Rows : constant Positive := 1 + Below (40);
         Surface, Expected : Pixels (0 .. Pitch * Rows - 1);
         W : constant Natural := Below (Natural'Min (Pitch, MASK_COLUMNS) + 1);
         X : constant Natural := (if Pitch > W then Below (Pitch - W + 1) else 0);
         H : constant Natural := Below (Natural'Min (Rows, MASK_ROWS) + 1);
         Y : constant Natural := (if Rows > H then Below (Rows - H + 1) else 0);
         R : constant Area := (X, Y, W, H);
         Value : constant Word := Next and 16#FFFFFF#;
         Row0 : constant Mask_Row := Below (MASK_ROWS - Natural'Max (H, 1) + 1);
         Col0 : constant Mask_Column := Below (MASK_COLUMNS - Natural'Max (W, 1) + 1);
         Opaque : constant Boolean := Below (2) = 0;
      begin
         for I in Surface'Range loop
            Surface (I) := Next and 16#FFFFFF#;
         end loop;
         Expected := Surface;
         Check (Fits (Surface'Length, Pitch, R), "rectangle fits");
         Fill (Surface, Pitch, R, Value);
         for I in Expected'Range loop
            if I / Pitch in Y .. Y + H - 1 and then I mod Pitch in X .. X + W - 1 then
               Expected (I) := Value;
            end if;
         end loop;
         Check (Surface = Expected, "fill matches the reference");
         --  Blit_Mask_Over is Blit_Mask's blend, faster where the pixel is
         --  still the run's background: same pixels on a partly filled surface.
         declare
            Under : constant Word := Surface (Surface'First);
            Plain, Ramped : Pixels := Surface;
         begin
            for I in Plain'Range loop
               if Next mod 2 = 0 then Plain (I) := Under; end if;
            end loop;
            Ramped := Plain;
            Blit_Mask (Plain, Pitch, R, Glyph, Row0, Col0, Ink, False, 0);
            Blit_Mask_Over (Ramped, Pitch, R, Glyph, Row0, Col0, Ink, Under, Shades (Ink, Under));
            Check (Plain = Ramped, "ramped blit matches the blend");
         end;
         Blit_Mask (Surface, Pitch, R, Glyph, Row0, Col0, Ink, Opaque, 16#ABCDEF#);
         for I in Expected'Range loop
            if I / Pitch in Y .. Y + H - 1 and then I mod Pitch in X .. X + W - 1 then
               declare
                  A : constant Byte := Glyph (Row0 + I / Pitch - Y, Col0 + I mod Pitch - X);
               begin
                  --  Coverage goes through the ink's Glyph_Gamma row first.
                  if Opaque then
                     Expected (I) := Mix (Ink, 16#ABCDEF#, Glyph_Gamma.Adjust (Ink_Row, A));
                  elsif A /= 0 then
                     Expected (I) := Mix (Ink, Expected (I), Glyph_Gamma.Adjust (Ink_Row, A));
                  end if;
               end;
            end if;
         end loop;
         Check (Surface = Expected, "blit matches the reference");
         declare
            Source : Pixels (0 .. MASK_ROWS * MASK_COLUMNS - 1);
         begin
            for I in Source'Range loop
               Source (I) := Next;
            end loop;
            Copy_Block (Surface, Pitch, R, Source, MASK_COLUMNS, Col0, Row0);
            for I in Expected'Range loop
               if I / Pitch in Y .. Y + H - 1 and then I mod Pitch in X .. X + W - 1 then
                  Expected (I) := Source ((Row0 + I / Pitch - Y) * MASK_COLUMNS + Col0 + I mod Pitch - X);
               end if;
            end loop;
            Check (Surface = Expected, "block copy matches the reference");
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: Client_Raster fill, mask blits and block copies match the reference," & Checks'Image & " checks");
end Main;
