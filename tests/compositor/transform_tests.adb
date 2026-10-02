with Ada.Text_IO;
with Compositor_Affine; use Compositor_Affine;
with Compositor_Sampling;
with Compositor_Transform;
procedure Transform_Tests is
   use type G.Pixel_Edge, G.Logical_Coordinate;
   use type Word, Signed;
   Screen : G.Output := (Width => 8, Height => 6, others => <>);
   Surface : constant G.Logical_Rectangle := (-2, -1, 3, 4);
   Cases : Natural := 0;
begin
   pragma Assert (Draw'Size = 56 * 8);
   pragma Assert (Compositor_Transform.Quad'Size = 88 * 8);
   for N in G.Scale_Component range 1 .. 4 loop
      for D in G.Scale_Component range 1 .. 4 loop
         Screen.Scale := (N, D);
         for Origin in -6 .. 6 loop
            Screen.X := G.Output_Origin (Origin);
            for Rotation in G.Orientation loop
               Screen.Rotation := Rotation;
               declare
                  P : constant Result := Plan (Screen, Surface, True);
                  Found : Boolean := False;
               begin
                  if P.Visible then
                     pragma Assert (Valid (P.Value, 8, 6) and then P.Value.Over = 1);
                  end if;
                  for Y in G.Pixel_Index range 0 .. 5 loop
                     for X in G.Pixel_Index range 0 .. 7 loop
                        declare
                           M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                             (Screen, (X, Y), Surface, 7, 9);
                           Covered : constant Boolean := P.Visible and then
                             Word (X) >= P.Value.Clip_X and then Word (Y) >= P.Value.Clip_Y and then
                             Word (X) - P.Value.Clip_X < P.Value.Clip_W and then
                             Word (Y) - P.Value.Clip_Y < P.Value.Clip_H;
                        begin
                           pragma Assert (Covered = M.Valid);
                           if P.Visible then
                              declare
                                 C : constant Compositor_Transform.Coefficients :=
                                   Compositor_Transform.Build (P.Value, Screen.Width, Screen.Height);
                                 Q : constant Compositor_Transform.Quad :=
                                   Compositor_Transform.Vertices (P.Value, Screen.Width, Screen.Height);
                                 U : constant Signed := 2 * C.U0 + C.UX * (2 * Signed (X) + 1) + C.UY * (2 * Signed (Y) + 1);
                                 V : constant Signed := 2 * C.V0 + C.VX * (2 * Signed (X) + 1) + C.VY * (2 * Signed (Y) + 1);
                                 Inside : constant Boolean := U >= 0 and U < 2 * C.UD and V >= 0 and V < 2 * C.VD;
                              begin
                                 pragma Assert (Q.UD = C.UD and Q.VD = C.VD);
                                 pragma Assert (Q.Corners (0).U = C.U0 and Q.Corners (0).V = C.V0);
                                 pragma Assert (Q.Corners (2).U = C.U0 + 8*C.UX + 6*C.UY);
                                 pragma Assert (Q.Corners (2).V = C.V0 + 8*C.VX + 6*C.VY);
                                 pragma Assert (Inside = M.Valid);
                                 if Inside then
                                    pragma Assert (U * 7 / (2 * C.UD) = Signed (M.X));
                                    pragma Assert (V * 9 / (2 * C.VD) = Signed (M.Y));
                                 end if;
                              end;
                           end if;
                           Found := Found or M.Valid;
                           Cases := Cases + 1;
                        end;
                     end loop;
                  end loop;
                  pragma Assert (Found = P.Visible);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (not Plan (Screen, (0, 0, 0, 0)).Visible);
   declare P : constant Result := Plan
     (Screen, (G.Logical_Coordinate'First, G.Logical_Coordinate'First,
               G.Logical_Coordinate'Last, G.Logical_Coordinate'Last));
   begin
      pragma Assert (P.Visible and then P.Value.Clip_W = 8 and then P.Value.Clip_H = 6);
   end;
   Ada.Text_IO.Put_Line ("TRANSFORM: PASS" & Cases'Image & " exact scissor/reference samples plus ABI and extreme bounds");
end Transform_Tests;
