with Ada.Text_IO;
with Compositor_Image_Sampling;
procedure Image_Sampling_Tests is
   package S renames Compositor_Image_Sampling;
   use type S.Wide;
   P : S.Layout;
   Q : S.Sample;
   Cases : Natural := 0;
   type Sizes is array (Positive range <>) of S.Extent;
   Dimensions : constant Sizes := [1, 2, 3, 7, 16, 31, 236, 150, 576, 2048, 65_535];
   procedure Check_Axis (First, Last : S.Index; Fraction : S.Fraction;
                         Local, Draw : S.Wide; Source : S.Extent) is
      Coordinate : constant S.Wide := S.Wide (First) * 256 + S.Wide (Fraction);
      Clamped : constant S.Wide := S.Wide'Min (Local, (Draw - 1) * 256);
   begin
      pragma Assert (First < Source and Last < Source);
      pragma Assert (Last = Natural'Min (First + 1, Source - 1));
      if Draw = 1 then
         pragma Assert (Coordinate = 0);
      else
         -- Check the rational quantization interval, not a second division.
         pragma Assert (Coordinate * (Draw - 1) <= Clamped * S.Wide (Source - 1));
         pragma Assert ((Coordinate + 1) * (Draw - 1) > Clamped * S.Wide (Source - 1));
      end if;
   end Check_Axis;
begin
   for Size in 1 .. 255 loop
      for Centre in 0 .. Size * 256 - 1 loop
         declare
            Position : constant S.Position := S.From_Centre (Centre, Size);
         begin
            if Centre < 128 then
               pragma Assert (Position = 0);
            elsif Centre >= (Size - 1) * 256 + 128 then
               pragma Assert (Position = S.Wide (Size - 1) * 256);
            else
               pragma Assert (Position + 128 = S.Wide (Centre));
            end if;
         end;
      end loop;
   end loop;
   for Width of Dimensions loop
      for Height of Dimensions loop
         for SW of Dimensions loop
            for SH of Dimensions loop
               for Mode in S.Placement loop
                  P := S.Prepare (Width, Height, SW, SH, Mode);
                  case Mode is
                     when S.Fill =>
                        pragma Assert (S.Draw_Width (P) >= S.Wide (Width));
                        pragma Assert (S.Draw_Height (P) >= S.Wide (Height));
                     when S.Fit =>
                        pragma Assert (S.Draw_Width (P) <= S.Wide (Width));
                        pragma Assert (S.Draw_Height (P) <= S.Wide (Height));
                     when S.Center =>
                        pragma Assert (S.Draw_Width (P) = S.Wide (SW));
                        pragma Assert (S.Draw_Height (P) = S.Wide (SH));
                  end case;
                  for Step in 0 .. 8 loop
                     declare
                        X : constant S.Position := S.Wide (Width) * 256 * S.Wide (Step) / 8 - 128;
                        Y : constant S.Position := S.Wide (Height) * 256 * S.Wide (8 - Step) / 8 - 128;
                        LX : constant S.Wide := X - S.Left (P) * 256;
                        LY : constant S.Wide := Y - S.Top (P) * 256;
                     begin
                        Q := S.At_Point (P, X, Y);
                        pragma Assert (Q.Valid = (LX >= 0 and LX < S.Draw_Width (P) * 256 and
                                                  LY >= 0 and LY < S.Draw_Height (P) * 256));
                        if Q.Valid then
                           Check_Axis (Q.X0, Q.X1, Q.FX, LX, S.Draw_Width (P), SW);
                           Check_Axis (Q.Y0, Q.Y1, Q.FY, LY, S.Draw_Height (P), SH);
                        end if;
                        Cases := Cases + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   P := S.Prepare (5, 3, 3, 2, S.Center);
   pragma Assert (not S.At_Point (P, 255, 0).Valid);
   Q := S.At_Point (P, 384, 128);
   pragma Assert (Q.Valid and then Q.X0 = 0 and then Q.X1 = 1 and then Q.FX = 128 and then Q.FY = 128);
   Ada.Text_IO.Put_Line ("PASS image sampler:" & Cases'Image & " placements/subpixel edges, fill/fit bounds, source indices and rational intervals");
end Image_Sampling_Tests;
