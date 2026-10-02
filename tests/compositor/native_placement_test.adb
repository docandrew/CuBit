with Interfaces; use Interfaces;
with Compositor_Glyph_Placement;
with Compositor_Glyph_Software;
with Mesa_Cache;
with Mesa_Masks;
function Native_Placement_Test return Boolean is
   package P renames Compositor_Glyph_Placement;
   package G renames P.G;
   package A renames P.A;
   use type A.Signed, G.Logical_Coordinate;
   package Software renames Compositor_Glyph_Software;
   subtype Bytes is Software.Bytes;
   subtype Pixels is Software.Pixels;
   Mask : aliased Bytes (0 .. P.L.Maximum_Bytes - 1) with Alignment => 64;
   Target : aliased Pixels (0 .. 84 * 72 - 1) with Alignment => 64;
   Software_Target : Pixels (Target'Range);
   Views : Mesa_Cache.State;
   Screen : G.Output := (80, 72, G.Unrotated, (1, 1), -3, 7);
   Scales : constant array (1 .. 6) of G.UI_Scale := ((1, 16), (16, 1), (5, 4), (3, 2), (2, 1), (1, 1));
   Points : constant array (1 .. 4) of G.Logical_Point := ((-4, 4), (0, 8), (3, 11), (40, 30));
   Sentinel : Unsigned_32 := 16#FF12_3456#;
   Tint : Unsigned_32 := 16#FFFF_FFFF#;
   Variant : Natural range 0 .. 1 := 0;
   OK : Boolean;
   Count : Unsigned_32 := 0;
   function Covered (X, Y : Natural) return Boolean is ((X * 7 + Y * 11) mod 13 < 6);
   function Coverage (X, Y : Natural) return Unsigned_8 is
     (if not Covered (X, Y) then 0 elsif Variant = 0 then 255 else Unsigned_8 ((X * 37 + Y * 13) mod 256));
   function Expected_Over (Alpha : Unsigned_8) return Unsigned_32 is
      A : constant Natural := Natural (Alpha) * Natural (Shift_Right (Tint, 24));
      Value : Unsigned_32 := 0;
   begin
      for C in 0 .. 3 loop
         declare
            F : constant Natural := (if C = 3 then 255 else Natural (Shift_Right (Tint, C * 8) and 255));
            B : constant Natural := Natural (Shift_Right (Sentinel, C * 8) and 255);
         begin
            Value := Value or Shift_Left (Unsigned_32 ((F * A + B * (65025 - A) + 32512) / 65025), C * 8);
         end;
      end loop;
      return Value;
   end Expected_Over;
   function Matches (Actual, Expected : Unsigned_32) return Boolean is
   begin
      if Variant = 0 or Expected = Sentinel then return Actual = Expected; end if;
      for C in 0 .. 3 loop
         if abs (Integer (Shift_Right (Actual, C * 8) and 255) -
                 Integer (Shift_Right (Expected, C * 8) and 255)) > 1 then return False; end if;
      end loop;
      return True;
   end Matches;
   procedure Report (Count : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_placement_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
begin
   Mesa_Cache.Initialize (Views, True);
   Mesa_Cache.Ensure (Views, 0, (Target'Address, 80, 72, 84 * 4, 1), Target'Length * 4, OK);
   if not OK then return False; end if;
   for Kind in 0 .. 1 loop
      Variant := Kind;
      Sentinel := (if Kind = 0 then 16#FF12_3456# else 16#8012_3456#);
      Tint := (if Kind = 0 then 16#FFFF_FFFF# else 16#8031_AF07#);
   for Scale of Scales loop
      Screen.Scale := Scale;
      declare Layout : constant P.L.Layout := P.L.Plan (Scale); begin
         Mask := (others => 16#A5#);
         for Y in 0 .. Layout.Height - 1 loop
            for X in 0 .. Layout.Width - 1 loop
               Mask (Y * Layout.Pitch + X) := Coverage (X, Y);
            end loop;
         end loop;
         Mesa_Masks.Ensure (Views, Mesa_Cache.Mask_Slot'First, Mask'Address, Layout, Mask'Length, OK);
         if not OK then return False; end if;
         for Rotation in G.Orientation loop
            Screen.Rotation := Rotation;
            for Point of Points loop
               declare
                  Plan : constant A.Result := P.Plan (Screen, Point);
                  Left : constant A.Signed := P.Snap (Point.X, Screen.X, Scale);
                  Top : constant A.Signed := P.Snap (Point.Y, Screen.Y, Scale);
               begin
                  for Damage in 0 .. 1 loop
                     Target := (others => Sentinel);
                     Software_Target := (others => Sentinel);
                     Software.Paint (Screen, Point,
                       (if Damage = 0 then (0, 0, 80, 72) else (7, 9, 73, 64)),
                       Mask, Software_Target, 84, Tint);
                     if Plan.Visible then
                        declare Clipped : constant A.Result := A.Clip (Plan.Value, 80, 72,
                          (if Damage = 0 then (0, 0, 80, 72) else (7, 9, 73, 64)));
                        begin
                           if Clipped.Visible then
                              Mesa_Masks.Render (Views, 0, Mesa_Cache.Mask_Slot'First, Clipped.Value,
                                                 80, 72, Tint, OK);
                              if not OK then return False; end if;
                           end if;
                        end;
                     end if;
                     for I in Target'Range loop
                        declare
                           X : constant Natural := I mod 84;
                           Y : constant Natural := I / 84;
                           UX, UY : A.Signed;
                           Expected : Unsigned_32 := Sentinel;
                        begin
                           if X < 80 then
                              case Rotation is
                                 when G.Unrotated => UX := A.Signed (X); UY := A.Signed (Y);
                                 when G.Clockwise_90 => UX := A.Signed (Y); UY := A.Signed (79 - X);
                                 when G.Clockwise_180 => UX := A.Signed (79 - X); UY := A.Signed (71 - Y);
                                 when G.Clockwise_270 => UX := A.Signed (71 - Y); UY := A.Signed (X);
                              end case;
                              if UX >= Left and then UX < Left + A.Signed (Layout.Width) and then
                                UY >= Top and then UY < Top + A.Signed (Layout.Height) and then
                                (Damage = 0 or (X in 7 .. 72 and Y in 9 .. 63)) and then
                                Covered (Natural (UX - Left), Natural (UY - Top))
                              then Expected := Expected_Over (Coverage (Natural (UX - Left), Natural (UY - Top))); end if;
                           end if;
                           if Software_Target (I) /= Expected then
                              Mismatch (Count, Unsigned_32 (I), Software_Target (I), Expected); return False;
                           end if;
                           if not Matches (Target (I), Expected) then Mismatch (Count, Unsigned_32 (I), Target (I), Expected); return False; end if;
                        end;
                     end loop;
                     Count := Count + 1;
                  end loop;
               end;
            end loop;
         end loop;
         for I in Mask'Range loop
            declare
               X : constant Natural := I mod Layout.Pitch;
               Y : constant Natural := I / Layout.Pitch;
               Expected : constant Unsigned_8 := (if X < Layout.Width and Y < Layout.Height then
                 Coverage (X, Y) else 16#A5#);
            begin if Mask (I) /= Expected then return False; end if; end;
         end loop;
      end;
   end loop;
   end loop;
   Mesa_Cache.Shutdown (Views);
   if not Mesa_Cache.Can_Retire (Views) or not Mesa_Cache.Views_Clear (Views) then return False; end if;
   Report (Count);
   return Count = 384;
end Native_Placement_Test;
