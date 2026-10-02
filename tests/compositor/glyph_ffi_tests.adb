with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Glyph_Layout; use Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
procedure Glyph_FFI_Tests is
   Pixels : aliased array (0 .. Maximum_Bytes + 15) of Unsigned_8;
   Advance, Cases : Natural := 0;
   OK : Boolean;
begin
   for Face in Unsigned_32 range 0 .. 1 loop
      for N in G.Scale_Component loop
         for D in G.Scale_Component loop
            declare L : constant Layout := Plan ((N, D));
            begin
               Pixels := (others => 16#A5#);
               Compositor_Glyph_FFI.Rasterize
                 (Face, Character'Pos ('W'), L, Pixels'Address, Pixels'Length, Advance, OK);
               pragma Assert (OK and Advance in 1 .. L.Width);
               for Y in 0 .. L.Height - 1 loop
                  for X in L.Width .. L.Pitch - 1 loop
                     pragma Assert (Pixels (Y * L.Pitch + X) = 16#A5#);
                  end loop;
               end loop;
               for I in L.Bytes .. Pixels'Last loop
                  pragma Assert (Pixels (I) = 16#A5#);
               end loop;
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   declare L : constant Layout := Plan ((5, 4));
   begin
      Pixels := (others => 16#A5#);
      Compositor_Glyph_FFI.Rasterize (0, 65, L, Pixels'Address, Unsigned_64 (L.Bytes - 1), Advance, OK);
      pragma Assert (not OK and Advance = 0);
      Compositor_Glyph_FFI.Rasterize (2, 65, L, Pixels'Address, Pixels'Length, Advance, OK);
      pragma Assert (not OK and Advance = 0);
      for Pixel of Pixels loop pragma Assert (Pixel = 16#A5#); end loop;
   end;
   Ada.Text_IO.Put_Line ("GLYPH-FFI: PASS" & Cases'Image & " density/face requests, padding and invalid capacities");
end Glyph_FFI_Tests;
