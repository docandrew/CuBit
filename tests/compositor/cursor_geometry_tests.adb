with Ada.Text_IO;
with Compositor_Cursor;
with Desktop_Cursors;
procedure Cursor_Geometry_Tests is
   package C renames Compositor_Cursor;
   package G renames C.G;
   use type G.Logical_Coordinate, G.Logical_Rectangle;
   Points : constant array (Positive range <>) of G.Logical_Coordinate :=
     (G.Logical_Coordinate'First, G.Logical_Coordinate'First + 100,
      -65535, -28, -1, 0, 1, 28, 65535,
      G.Logical_Coordinate'Last - 100, G.Logical_Coordinate'Last);
   Checks : Natural := 0;
begin
   for X of Points loop
      for Y of Points loop
         for Item in Desktop_Cursors.Cursor_ID loop
            declare
               M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Item);
               L : constant Long_Long_Integer := Long_Long_Integer (X) - Long_Long_Integer (M.Hotspot_X);
               T : constant Long_Long_Integer := Long_Long_Integer (Y) - Long_Long_Integer (M.Hotspot_Y);
               R : constant Long_Long_Integer := L + Long_Long_Integer (M.Width);
               B : constant Long_Long_Integer := T + Long_Long_Integer (M.Height);
               P : constant C.Plan := C.Build ((X, Y), M.Width, M.Height, M.Hotspot_X, M.Hotspot_Y);
               Valid : constant Boolean := L >= Long_Long_Integer (G.Logical_Coordinate'First) and
                 T >= Long_Long_Integer (G.Logical_Coordinate'First) and
                 R <= Long_Long_Integer (G.Logical_Coordinate'Last) and B <= Long_Long_Integer (G.Logical_Coordinate'Last);
            begin
               pragma Assert (P.Valid = Valid);
               if Valid then
                  pragma Assert (P.Surface = G.Logical_Rectangle'(G.Logical_Coordinate (L), G.Logical_Coordinate (T), G.Logical_Coordinate (R), G.Logical_Coordinate (B)));
                  pragma Assert (P.Surface.Left + G.Logical_Coordinate (M.Hotspot_X) = X and
                    P.Surface.Top + G.Logical_Coordinate (M.Hotspot_Y) = Y);
               else pragma Assert (P.Surface = G.Logical_Rectangle'(0, 0, 0, 0)); end if;
               Checks := Checks + 1;
            end;
         end loop;
      end loop;
   end loop;
   pragma Assert (not C.Build ((0, 0), 1, 1, 1, 0).Valid);
   pragma Assert (not C.Build ((0, 0), 1, 1, 0, 1).Valid);
   pragma Assert (C.Build ((0, 0), 65535, 65535, 65534, 65534).Valid);
   Ada.Text_IO.Put_Line ("PASS cursor geometry: hotspot preservation, negative origins, all five shapes, logical limits and invalid metadata; cases" & Checks'Image);
end Cursor_Geometry_Tests;
