with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.Fonts;
with Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
-- Native execution of the actual toolkit and Rust font bridge. This checks
-- offscreen pixels, not output configuration negotiation or scanout.
procedure Desktop_Density_Text (Result : out Boolean) is
   use type CuBit.Fonts.Face;
   Sentinel : constant Color := 16#163759#;
begin
   Result := False;
   declare
      package L renames Compositor_Glyph_Layout;
      type Image is array (0 .. 387, 0 .. 643) of Color;
      Actual : aliased Image;
      type Bytes is array (Natural range <>) of Unsigned_8;
      Mask : aliased Bytes (0 .. L.Maximum_Bytes - 1);
      type Scales is array (Positive range <>) of L.G.UI_Scale;
      Values : constant Scales := [(5, 4), (3, 2), (2, 1), (16, 1), (1, 2)];
      Target : Canvas;
      Plan : L.Layout;
      OK, Different : Boolean := False;
      Advance, N, D, Left, Top, Clip_Left, Clip_Top, Clip_Right, Clip_Bottom : Natural;
      Width, A, R, G, B : Unsigned_32;
      Expected : Color;
      Alpha : Unsigned_8;
      Old : CuBit.Fonts.Glyph_Access;
   begin
      for Scale of Values loop
         N := Natural (Scale.Numerator); D := Natural (Scale.Denominator);
         Plan := L.Plan (Scale);
         Left := (N + D - 1) / D; Top := Left;
         Clip_Left := (3 * N + D - 1) / D; Clip_Right := (20 * N + D - 1) / D;
         Clip_Top := (2 * N + D - 1) / D; Clip_Bottom := (16 * N + D - 1) / D;
         for Face in CuBit.Fonts.Face loop
            Old := CuBit.Fonts.Get (Face, 'A');
            Compositor_Glyph_FFI.Rasterize
              (CuBit.Fonts.Face'Pos (Face), Character'Pos ('A'), Plan,
               Mask'Address, Mask'Length, Advance, OK);
            if not OK then return; end if;
            Actual := [others => [others => Sentinel]];
            Target := (addr => Actual'Address, width => 40, height => 24,
              pitch => 644 * 4, densityNumerator => N, densityDenominator => D,
              clipEnabled => True, clip => (3, 2, 17, 14), others => <>);
            if Face = CuBit.Fonts.Sans then
               Draw_UI_Text_Transparent (Target, 1, 1, "A", 16#FFFFFF#);
            else
               Draw_Code_Text (Target, 1, 1, "A", 16#FFFFFF#, 16#204060#);
            end if;
            Width := Unsigned_32 ((9 * N + D - 1) / D);
            for Y in Actual'Range (1) loop
               for X in Actual'Range (2) loop
                  Expected := Sentinel;
                  if X >= Clip_Left and X < Clip_Right and Y >= Clip_Top and Y < Clip_Bottom then
                     if Face = CuBit.Fonts.Monospace and X < Natural (Width) then Expected := 16#204060#; end if;
                     if X >= Left and Y >= Top and X - Left < Plan.Width and Y - Top < Plan.Height then
                        Alpha := Mask ((Y - Top) * Plan.Pitch + X - Left);
                        A := Unsigned_32 (Alpha);
                        R := (255 * A + (Shift_Right (Expected, 16) and 255) * (255 - A) + 127) / 255;
                        G := (255 * A + (Shift_Right (Expected, 8) and 255) * (255 - A) + 127) / 255;
                        B := (255 * A + (Expected and 255) * (255 - A) + 127) / 255;
                        Expected := Shift_Left (R, 16) or Shift_Left (G, 8) or B;
                        if N = 2 and D = 1 then
                           Different := Different or Alpha /= Old.Alpha ((Y - Top) / 2, (X - Left) / 2);
                        end if;
                     end if;
                  end if;
                  if Actual (Y, X) /= Expected then return; end if;
               end loop;
            end loop;
         end loop;
      end loop;
      if not Different then return; end if;
      Result := True;
   end;
end Desktop_Density_Text;
