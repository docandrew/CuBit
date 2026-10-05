with Ada.Text_IO;
with Compositor_Backdrop;
procedure Backdrop_Tests is
   package B renames Compositor_Backdrop;
   use type B.Word, B.Wide, B.Signed, B.G.Pixel_Edge;
   type Dimensions is array (Positive range <>) of B.G.Physical_Extent;
   Sizes : constant Dimensions := [1, 2, 3, 17, 31, 576, 2048, 65_535];
   Count : Natural := 0;
   R : B.Result;
begin
   pragma Assert (B.Draw'Size = 56 * 8);
   for W of Sizes loop
      for H of Sizes loop
         for SW of Sizes loop
            for SH of Sizes loop
               for Mode in B.S.Placement loop
                  for Clip in 0 .. 4 loop
                     declare
                        Damage : constant B.G.Physical_Rectangle :=
                          (case Clip is
                             when 0 => (0, 0, W, H),
                             when 1 => (0, 0, W / 2, H / 2),
                             when 2 => (W, H, W, H),
                             when 3 => (W, H, 0, 0),
                             when others => (W / 2, H / 2, 65_535, 65_535));
                     begin
                        R := B.Plan (W, H, SW, SH, Mode, Damage);
                        pragma Assert (R.Visible =
                          (Clip = 0 or Clip = 4 or (Clip = 1 and W > 1 and H > 1)));
                        if R.Visible then
                           pragma Assert (B.Valid (R.Value, W, H));
                           pragma Assert (2 * R.Value.Left <= B.Signed (W) - B.Signed (R.Value.Width) + 1);
                           pragma Assert (2 * R.Value.Left >= B.Signed (W) - B.Signed (R.Value.Width) - 1);
                           pragma Assert (2 * R.Value.Top <= B.Signed (H) - B.Signed (R.Value.Height) + 1);
                           pragma Assert (2 * R.Value.Top >= B.Signed (H) - B.Signed (R.Value.Height) - 1);
                           case Mode is
                              when B.S.Fill =>
                                 pragma Assert (R.Value.Width >= B.Wide (W) and R.Value.Height >= B.Wide (H));
                              when B.S.Fit =>
                                 pragma Assert (R.Value.Width <= B.Wide (W) and R.Value.Height <= B.Wide (H));
                              when B.S.Center =>
                                 pragma Assert (R.Value.Width = B.Wide (SW) and R.Value.Height = B.Wide (SH));
                           end case;
                           pragma Assert (R.Value.Source_W = B.Word (SW) and R.Value.Source_H = B.Word (SH));
                        end if;
                        Count := Count + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("BACKDROP: PASS" & Count'Image & " placement/clip cases, 56-byte ABI");
end Backdrop_Tests;
