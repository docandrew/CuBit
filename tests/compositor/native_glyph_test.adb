with Interfaces; use Interfaces;
with Compositor_Glyph_Layout; use Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
with CuBit.Fonts;
function Native_Glyph_Test return Boolean is
   Pixels : aliased array (0 .. Maximum_Bytes + 15) of Unsigned_8;
   Scales : constant array (1 .. 6) of G.UI_Scale := ((1, 1), (5, 4), (3, 2), (2, 1), (1, 16), (16, 1));
   Advance : Natural;
   OK, Ink : Boolean;
   procedure Report with Import, Convention => C, External_Name => "compositor_glyph_report";
begin
   for Face in CuBit.Fonts.Face loop
      for Index in Scales'Range loop
         declare L : constant Layout := Plan (Scales (Index));
         begin
            Pixels := (others => 16#A5#);
            Compositor_Glyph_FFI.Rasterize
              (CuBit.Fonts.Face'Pos (Face), Character'Pos ('W'), L, Pixels'Address, Pixels'Length, Advance, OK);
            if not OK or Advance not in 1 .. L.Width then return False; end if;
            Ink := False;
            for Y in 0 .. L.Height - 1 loop
               for X in 0 .. L.Pitch - 1 loop
                  if X < L.Width then Ink := Ink or Pixels (Y * L.Pitch + X) /= 0;
                  elsif Pixels (Y * L.Pitch + X) /= 16#A5# then return False;
                  end if;
               end loop;
            end loop;
            if Index /= 5 and not Ink then return False; end if;
            for I in L.Bytes .. Pixels'Last loop
               if Pixels (I) /= 16#A5# then return False; end if;
            end loop;
            if Index in 1 | 4 then
               declare
                  Old : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get
                    (Face, 'W', (if Index = 1 then CuBit.Fonts.Normal else CuBit.Fonts.Double_Size));
               begin
                  if Advance /= Natural (Old.Advance) then return False; end if;
                  for Y in 0 .. L.Height - 1 loop
                     for X in 0 .. CuBit.Fonts.Max_Width - 1 loop
                        if Pixels (Y * L.Pitch + X) /= Old.Alpha (Y, X) then return False; end if;
                     end loop;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end loop;
   declare L : constant Layout := Plan ((5, 4));
   begin
      Pixels := (others => 16#A5#);
      Compositor_Glyph_FFI.Rasterize (0, 65, L, Pixels'Address, Unsigned_64 (L.Bytes - 1), Advance, OK);
      if OK then return False; end if;
      for Pixel of Pixels loop if Pixel /= 16#A5# then return False; end if; end loop;
   end;
   Report;
   return True;
end Native_Glyph_Test;
