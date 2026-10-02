with Ada.Text_IO;
with Compositor_Density_Selection; use Compositor_Density_Selection;
procedure Density_Selection_Tests is
   use type G.Scale_Component, G.Logical_Coordinate;
   Screens : Outputs := (others => (Width => 1200, Height => 900, others => <>));
   Cases : Natural := 0;
begin
   -- Independent cross multiplication checks the exact rank for every rational
   -- pair; no floating-point or rounding is allowed in density ordering.
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         for M in G.Scale_Component loop
            for E in G.Scale_Component loop
               pragma Assert ((Rank ((N, D)) >= Rank ((M, E))) =
                 (Integer (N) * Integer (E) >= Integer (M) * Integer (D)));
               Cases := Cases + 1;
            end loop;
         end loop;
      end loop;
   end loop;
   Screens (1).Scale := (1, 1);
   Screens (2).X := 1200;
   Screens (2).Scale := (3, 2);
   pragma Assert (Choose (Screens, 2, 1, (1100, 20, 1201, 200)) = 2);
   pragma Assert (Choose (Screens, 2, 2, (100, 20, 1200, 200)) = 1);
   pragma Assert (Choose (Screens, 2, 1, (2000, 0, 2001, 1)) = 1);
   pragma Assert (Choose (Screens, 2, 2, (0, 0, 0, 0)) = 2);
   pragma Assert (Choose (Screens, 2, 1, (10, 10, 0, 0)) = 1);
   Screens (2).X := -800;
   Screens (2).Rotation := G.Clockwise_90;
   pragma Assert (Choose (Screens, 2, 1, (-800, 0, -799, 1)) = 2);
   -- Every result must be an intersecting maximal-density witness, with exact
   -- primary fallback if nothing intersects. Include negative origins/rotation.
   for Rotation in G.Orientation loop
      Screens (2).Rotation := Rotation;
      for X in -900 .. 1300 loop
         declare
            W : constant G.Logical_Rectangle := (G.Logical_Coordinate (X), 0,
              G.Logical_Coordinate (X + 100), 100);
            Selected : constant Output_Index := Choose (Screens, 2, 1, W);
         begin
            if Intersects (Screens (2), W) then pragma Assert (Selected = 2);
            else pragma Assert (Selected = 1); end if;
            Cases := Cases + 1;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("DENSITY-SELECTION: PASS" & Cases'Image & " rational/negative-origin/rotation cases plus seam and fallback checks");
end Density_Selection_Tests;
