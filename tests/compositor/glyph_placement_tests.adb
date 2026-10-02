with Ada.Text_IO;
with Compositor_Glyph_Placement;
with Compositor_Transform;
procedure Glyph_Placement_Tests is
   package P renames Compositor_Glyph_Placement;
   package G renames P.G;
   package A renames P.A;
   use type A.Signed, A.Word, G.Logical_Coordinate;
   Screen : G.Output := (80, 72, G.Unrotated, (1, 1), -3, 7);
   Points : constant array (1 .. 8) of G.Logical_Point :=
     ((-4, 4), (-10, -8), (0, 0), (3, 11), (40, 30), (76, 71),
      (G.Logical_Coordinate'First, G.Logical_Coordinate'Last),
      (G.Logical_Coordinate'Last, G.Logical_Coordinate'First));
   Cases : Natural := 0;
begin
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         Screen.Scale := (N, D);
         for X in -300 .. 300 loop
            pragma Assert (P.Snap (G.Logical_Coordinate (X), -3, Screen.Scale) =
              A.Signed (Long_Float'Floor (Long_Float (X + 3) * Long_Float (N) / Long_Float (D) + 0.5)));
         end loop;
         for Rotation in G.Orientation loop
            Screen.Rotation := Rotation;
            for Point of Points loop
               declare
                  Plan : constant A.Result := P.Plan (Screen, Point);
                  L : constant P.L.Layout := P.L.Plan (Screen.Scale);
                  Left : constant A.Signed := P.Snap (Point.X, Screen.X, Screen.Scale);
                  Top : constant A.Signed := P.Snap (Point.Y, Screen.Y, Screen.Scale);
               begin
                  for Y in 0 .. 71 loop
                     for X in 0 .. 79 loop
                        declare
                           UX, UY : A.Signed;
                           Inside : Boolean;
                        begin
                           case Rotation is
                              when G.Unrotated => UX := A.Signed (X); UY := A.Signed (Y);
                              when G.Clockwise_90 => UX := A.Signed (Y); UY := A.Signed (79 - X);
                              when G.Clockwise_180 => UX := A.Signed (79 - X); UY := A.Signed (71 - Y);
                              when G.Clockwise_270 => UX := A.Signed (71 - Y); UY := A.Signed (X);
                           end case;
                           Inside := UX >= Left and UX < Left + A.Signed (L.Width) and
                             UY >= Top and UY < Top + A.Signed (L.Height);
                           if not Plan.Visible then pragma Assert (not Inside);
                           else
                              declare
                                 V : A.Draw renames Plan.Value;
                                 C : constant Compositor_Transform.Coefficients := Compositor_Transform.Build (V, 80, 72);
                                 Clipped : constant Boolean := A.Word (X) >= V.Clip_X and A.Word (X) < V.Clip_X + V.Clip_W and
                                   A.Word (Y) >= V.Clip_Y and A.Word (Y) < V.Clip_Y + V.Clip_H;
                              begin
                                 pragma Assert (Clipped = Inside);
                                 if Inside then
                                    -- After multiplying normalized UV by source dimensions,
                                    -- each target centre is exactly a source pixel centre.
                                    pragma Assert (2 * C.U0 + C.UX * (2 * A.Signed (X) + 1) + C.UY * (2 * A.Signed (Y) + 1) = 2 * (UX - Left) + 1);
                                    pragma Assert (2 * C.V0 + C.VX * (2 * A.Signed (X) + 1) + C.VY * (2 * A.Signed (Y) + 1) = 2 * (UY - Top) + 1);
                                    pragma Assert (C.UD = A.Signed (L.Width) and C.VD = A.Signed (L.Height));
                                 end if;
                              end;
                           end if;
                        end;
                     end loop;
                  end loop;
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("GLYPH-PLACEMENT: PASS" & Cases'Image & " layouts, all256 densities, 4 rotations, clipping, signed/extreme origins and exact texel centres");
end Glyph_Placement_Tests;
