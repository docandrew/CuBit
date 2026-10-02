with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Glyph_Software;
procedure Glyph_Software_Tests is
   package S renames Compositor_Glyph_Software;
   package G renames S.G;
   use type G.Scale_Component, G.Logical_Coordinate;
   Mask : S.Bytes (0 .. S.L.Maximum_Bytes - 1);
   Target : S.Pixels (0 .. 28 * 20 + 7);
   Screen : G.Output := (24, 20, G.Unrotated, (1, 1), -3, 7);
   Points : constant array (1 .. 4) of G.Logical_Point := ((-4, 6), (0, 8), (3, 11), (2 ** 30, -2 ** 30));
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   Tints : constant array (1 .. 4) of Unsigned_32 := (16#FFFF_FFFF#, 16#8020_4060#, 16#00FF_FFFF#, 16#FF31_AF07#);
   Count : Natural := 0;
   function Reference_Over (Coverage : Unsigned_8; Tint, Back : Unsigned_32) return Unsigned_32 is
      Alpha : constant Natural := Natural (Coverage) * Natural (Shift_Right (Tint, 24));
      Value : Unsigned_32 := 0;
   begin
      for C in 0 .. 3 loop
         declare
            F : constant Natural := (if C = 3 then 255 else Natural (Shift_Right (Tint, C * 8) and 255));
            B : constant Natural := Natural (Shift_Right (Back, C * 8) and 255);
         begin
            Value := Value or Shift_Left (Unsigned_32 ((F * Alpha + B * (65025 - Alpha) + 32512) / 65025), C * 8);
         end;
      end loop;
      return Value;
   end Reference_Over;
   function Round_Origin (Value : Integer; Scale : G.UI_Scale) return Integer is
      N : constant Integer := 2 * Value * Integer (Scale.Numerator) + Integer (Scale.Denominator);
      D : constant Integer := 2 * Integer (Scale.Denominator);
   begin
      if N >= 0 then return N / D; else return -((-N + D - 1) / D); end if;
   end Round_Origin;
begin
   for Coverage in Unsigned_8 loop
      for Tint of Tints loop
         for Back in 0 .. 255 loop
            declare B : constant Unsigned_32 := Unsigned_32 (Back) * 16#0101_0101#; begin
               pragma Assert (S.Over (Coverage, Tint, B) = Reference_Over (Coverage, Tint, B));
            end;
         end loop;
      end loop;
   end loop;
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         Screen.Scale := (N, D);
         declare Layout : constant S.L.Layout := S.L.Plan (Screen.Scale); begin
            for I in Mask'Range loop Mask (I) := Unsigned_8 ((I * 37 + I / Layout.Pitch * 11) mod 256); end loop;
            for Rotation in G.Orientation loop
               Screen.Rotation := Rotation;
               for Point of Points loop
                  for Damage in 0 .. 2 loop
                     declare
                        Area : constant G.Physical_Rectangle := (case Damage is
                          when 0 => (0, 0, 24, 20), when 1 => (3, 5, 21, 16), when others => (22, 19, 2, 1));
                        Tint : constant Unsigned_32 := Tints (Damage + 1);
                     begin
                        Target := (others => Sentinel);
                        S.Paint (Screen, Point, Area, Mask, Target, 28, Tint);
                        for I in Target'Range loop
                           declare
                              X : constant Integer := I mod 28;
                              Y : constant Integer := I / 28;
                              U, V : Integer;
                              Expected : Unsigned_32 := Sentinel;
                           begin
                              if Point.X /= 2 ** 30 and X < 24 and Y < 20 and
                                X >= Integer (Area.Left) and X < Integer (Area.Right) and
                                Y >= Integer (Area.Top) and Y < Integer (Area.Bottom)
                              then
                                 case Rotation is
                                    when G.Unrotated => U := X; V := Y;
                                    when G.Clockwise_90 => U := Y; V := 23 - X;
                                    when G.Clockwise_180 => U := 23 - X; V := 19 - Y;
                                    when G.Clockwise_270 => U := 19 - Y; V := X;
                                 end case;
                                 U := U - Round_Origin (Integer (Point.X) + 3, Screen.Scale);
                                 V := V - Round_Origin (Integer (Point.Y) - 7, Screen.Scale);
                                 if U >= 0 and U < Layout.Width and V >= 0 and V < Layout.Height then
                                    Expected := Reference_Over (Mask (V * Layout.Pitch + U), Tint, Sentinel);
                                 end if;
                              end if;
                              pragma Assert (Target (I) = Expected);
                           end;
                        end loop;
                        Count := Count + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("GLYPH-SOFTWARE: PASS" & Count'Image & " physical frames; 262144 blend cases");
end Glyph_Software_Tests;
