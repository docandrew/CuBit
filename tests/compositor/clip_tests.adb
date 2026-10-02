with Ada.Text_IO;
with Compositor_Affine; use Compositor_Affine;
procedure Clip_Tests is
   use type Word, Signed;
   D : constant Draw := (Origin_X => -3, Origin_Y => 2, Logical_W => 7, Logical_H => 5,
     Numerator => 3, Denominator => 2, Rotation => 1,
     Clip_X => 1, Clip_Y => 1, Clip_W => 6, Clip_H => 4, Over => 1);
   Cases : Natural := 0;
begin
   for L in G.Pixel_Edge range 0 .. 9 loop
      for R in G.Pixel_Edge range 0 .. 9 loop
         for T in G.Pixel_Edge range 0 .. 7 loop
            for B in G.Pixel_Edge range 0 .. 7 loop
               declare
                  P : constant Result := Clip (D, 8, 6, (L, T, R, B));
                  Found : Boolean := False;
               begin
                  if P.Visible then pragma Assert (Same_Transform (D, P.Value)); end if;
                  for Y in 0 .. 5 loop
                     for X in 0 .. 7 loop
                        declare
                           Expected : constant Boolean := X >= 1 and X < 7 and Y >= 1 and Y < 5 and
                             X >= Natural (L) and X < Natural (R) and Y >= Natural (T) and Y < Natural (B);
                           Actual : constant Boolean := P.Visible and then Word (X) >= P.Value.Clip_X and then
                             Word (Y) >= P.Value.Clip_Y and then Word (X) - P.Value.Clip_X < P.Value.Clip_W and then
                             Word (Y) - P.Value.Clip_Y < P.Value.Clip_H;
                        begin
                           pragma Assert (Actual = Expected);
                           Found := Found or Expected;
                           Cases := Cases + 1;
                        end;
                     end loop;
                  end loop;
                  pragma Assert (P.Visible = Found);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("AFFINE-CLIP: PASS" & Cases'Image & " pixel membership cases, unchanged transform");
end Clip_Tests;
