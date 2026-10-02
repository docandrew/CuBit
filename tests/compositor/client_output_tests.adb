with Ada.Text_IO;
with Desktop_Composition;
with Compositor_Client_Output; use Compositor_Client_Output;
procedure Client_Output_Tests is
   use type G.Logical_Coordinate;
   Cases : Natural := 0;
   Screen : G.Output := (Width => 8, Height => 6, others => <>);
begin
   for W in 1 .. 8 loop
      for H in 1 .. 6 loop
         Screen.Width := G.Physical_Extent (W); Screen.Height := G.Physical_Extent (H);
         for X in 0 .. 10 loop
            for Y in 0 .. 8 loop
               for SW in 1 .. 6 loop
                  for SH in 1 .. 6 loop
                     declare
                        B : constant Desktop_Composition.Blit_Plan := Desktop_Composition.Plan
                          (W, H, SW, SH, (X, Y, 5, 4), True, (1, 1, 5, 3));
                        P : constant Result := Plan (Screen, W, H, SW, SH, X, Y, B);
                     begin
                        pragma Assert (P.Valid = (B.Width > 0 and B.Height > 0));
                        if P.Valid then
                           pragma Assert (P.Surface.Right - P.Surface.Left = G.Logical_Coordinate (SW));
                           pragma Assert (P.Surface.Bottom - P.Surface.Top = G.Logical_Coordinate (SH));
                           pragma Assert (Natural (P.Damage.Left) - X = B.Source_X);
                           pragma Assert (Natural (P.Damage.Top) - Y = B.Source_Y);
                        end if;
                        Cases := Cases + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   declare
      B : constant Desktop_Composition.Blit_Plan := (0, 0, 0, 0, 1, 1);
   begin
      pragma Assert (not Plan (Screen, 8, 6, Natural'Last, 1, 0, 0, B).Valid);
      pragma Assert (not Plan (Screen, 8, 6, 1, 1, Natural'Last, 0, B).Valid);
      pragma Assert (not Plan (Screen, 8, 6, 1, 1, 0, 0, (0, 0, 0, 0, Natural'Last, 1)).Valid);
      Screen.Rotation := G.Clockwise_90;
      pragma Assert (not Plan (Screen, 8, 6, 1, 1, 0, 0, B).Valid);
      Screen.Rotation := G.Unrotated; Screen.Scale := (3, 2);
      pragma Assert (not Plan (Screen, 8, 6, 1, 1, 0, 0, B).Valid);
   end;
   Ada.Text_IO.Put_Line ("CLIENT-OUTPUT: PASS" & Cases'Image & " legacy placement/damage cases and malformed input rejects");
end Client_Output_Tests;
