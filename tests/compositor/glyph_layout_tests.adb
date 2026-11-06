with Ada.Text_IO;
with Compositor_Glyph_Layout; use Compositor_Glyph_Layout;
procedure Glyph_Layout_Tests is
   Cases : Natural := 0;
begin
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         declare
            L : constant Layout := Plan ((N, D));
            W : constant Natural := Natural (Long_Float'Ceiling (Long_Float (Base_Width_Bound) * Long_Float (N) / Long_Float (D)));
            H : constant Natural := Natural (Long_Float'Ceiling (Long_Float (Base_Line) * Long_Float (N) / Long_Float (D)));
         begin
            pragma Assert (Valid (L) and L.Width = W and L.Height = H);
            pragma Assert (L.Bytes <= Maximum_Bytes);
            for N2 in G.Scale_Component loop
               for D2 in G.Scale_Component loop
                  if Natural (N) * Natural (D2) = Natural (N2) * Natural (D) then
                     declare Other : constant Layout := Plan ((N2, D2));
                     begin
                        pragma Assert (L.Width = Other.Width and L.Height = Other.Height and
                          L.Pitch = Other.Pitch and L.Bytes = Other.Bytes);
                     end;
                  end if;
               end loop;
            end loop;
            Cases := Cases + 1;
         end;
      end loop;
   end loop;
   --  The 21-pixel line at 125% and 150% (the 16-pixel em: CuBit.UI_Metrics).
   pragma Assert (Base_Line = 21 and Base_Em = 16);
   pragma Assert (Plan ((5, 4)).Height = 27);
   pragma Assert (Plan ((3, 2)).Height = 32);
   pragma Assert (Plan ((16, 1)).Bytes = Maximum_Bytes);
   Ada.Text_IO.Put_Line ("GLYPH-LAYOUT: PASS" & Cases'Image & " densities, equivalent ratios and maximum storage bound");
end Glyph_Layout_Tests;
