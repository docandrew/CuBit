with Ada.Text_IO;
with CuBit.Display_Layouts;
procedure Pointer_Test is
   use CuBit.Display_Layouts;
   use type G.Logical_Point;
   use type G.Logical_Coordinate;
   Ready : Layout := (Count => 2, others => <>);
   Cases : Natural := 0;
begin
   Ready.Items (1).Geometry := (Width => 1024, Height => 768, others => <>);
   Ready.Items (2) := (2, (Width => 1280, Height => 720, X => 1024, others => <>));
   for Rotation in G.Orientation loop
      Ready.Items (2).Geometry.Rotation := Rotation;
      for Scale in G.Scale_Component loop
         Ready.Items (2).Geometry.Scale.Numerator := Scale;
         for X in -4 .. 50 loop
            for Y in -4 .. 35 loop
               declare
                  P : constant G.Logical_Point := (G.Logical_Coordinate (X * 64),
                                                    G.Logical_Coordinate (Y * 64));
                  R : constant Pointer_Position := Confine (Ready, P);
               begin
                  pragma Assert (R.Screen <= Ready.Count);
                  pragma Assert (G.Contains (Ready.Items (R.Screen).Geometry, R.Point));
                  if (for some I in 1 .. Ready.Count => G.Contains (Ready.Items (I).Geometry, P)) then
                     pragma Assert (P = R.Point);
                  end if;
                  pragma Assert (Confine (Ready, R.Point).Point = R.Point);
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   --  Hostile coordinate extremes, negative origins and fractional scales.
   Ready.Items (1).Geometry := (Width => 1, Height => 1,
      X => G.Output_Origin'First, Y => G.Output_Origin'Last, Scale => (16, 1), others => <>);
   Ready.Count := 1;
   declare
      R : constant Pointer_Position := Confine (Ready, (G.Logical_Coordinate'Last,
                                                        G.Logical_Coordinate'First));
   begin
      pragma Assert (G.Contains (Ready.Items (1).Geometry, R.Point));
   end;
   Ada.Text_IO.Put_Line ("PASS pointer confinement:" & Cases'Image & " cases");
end Pointer_Test;
