with Glyph_Gamma;
package body Compositor_Glyph_Software with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);
   use type P.A.Signed, Byte, G.Orientation;
   function At_Pixel (Where : Placement; Pixel : G.Physical_Point) return Sample is
      X, Y : P.A.Signed;
   begin
      if Pixel.X >= Where.Width or Pixel.Y >= Where.Height then return (Valid => False); end if;
      case Where.Rotation is
         when G.Unrotated => X := P.A.Signed (Pixel.X); Y := P.A.Signed (Pixel.Y);
         when G.Clockwise_90 => X := P.A.Signed (Pixel.Y); Y := P.A.Signed (Where.Width) - 1 - P.A.Signed (Pixel.X);
         when G.Clockwise_180 => X := P.A.Signed (Where.Width) - 1 - P.A.Signed (Pixel.X); Y := P.A.Signed (Where.Height) - 1 - P.A.Signed (Pixel.Y);
         when G.Clockwise_270 => X := P.A.Signed (Where.Height) - 1 - P.A.Signed (Pixel.Y); Y := P.A.Signed (Pixel.X);
      end case;
      X := X - Where.Left; Y := Y - Where.Top;
      if X < 0 or Y < 0 or X >= P.A.Signed (Where.Raster.Width) or Y >= P.A.Signed (Where.Raster.Height) then
         return (Valid => False);
      end if;
      return (True, Natural (X), Natural (Y));
   end At_Pixel;
   subtype Channel is Natural range 0 .. 255;
   subtype Opacity is Natural range 0 .. 65_025;
   function Portion (Value : Channel; Alpha : Opacity) return Channel
     with Post => Portion'Result <= Value
   is
   begin
      return (Value * Alpha + 32_512) / 65_025;
   end Portion;
   function Mix (Foreground, Background : Channel; Alpha : Opacity) return Channel
     with Post => Mix'Result >= Channel'Min (Foreground, Background) and
       Mix'Result <= Channel'Max (Foreground, Background)
   is
   begin
      if Foreground >= Background then
         return Background + Portion (Foreground - Background, Alpha);
      else
         return Background - Portion (Background - Foreground, Alpha);
      end if;
   end Mix;
   function Component (Value : Word; Shift : Natural) return Channel is
     (Natural (Interfaces.Shift_Right (Value, Shift) and 255));
   --  Over for coverage already through the tint's Glyph_Gamma row. Its own
   --  contract keeps it out of proof inlining: the bound below needs the
   --  parameter's Byte range, not the caller's expression.
   function Corrected_Over (Coverage : Byte; Tint, Background : Word) return Word
     with Global => null, Post => True;
   function Corrected_Over (Coverage : Byte; Tint, Background : Word) return Word is
      Alpha : constant Opacity := Natural (Coverage) * Component (Tint, 24);
   begin
      --  Full coverage of opaque ink: every Mix returns its foreground.
      if Alpha = Opacity'Last then
         return 16#FF00_0000# or (Tint and 16#00FF_FFFF#);
      end if;
      return Interfaces.Shift_Left (Word (Mix (255, Component (Background, 24), Alpha)), 24) or
        Interfaces.Shift_Left (Word (Mix (Component (Tint, 16), Component (Background, 16), Alpha)), 16) or
        Interfaces.Shift_Left (Word (Mix (Component (Tint, 8), Component (Background, 8), Alpha)), 8) or
        Word (Mix (Component (Tint, 0), Component (Background, 0), Alpha));
   end Corrected_Over;
   function Over (Coverage : Byte; Tint, Background : Word) return Word is
     (Corrected_Over (Glyph_Gamma.Adjust (Glyph_Gamma.Bucket_Of (Tint), Coverage), Tint, Background));
   function Edge (Value : P.A.Signed; Limit : G.Physical_Extent) return G.Pixel_Edge is (G.Pixel_Edge (Value))
     with Pre => Value in 0 .. P.A.Signed (Limit),
       Post => Edge'Result <= Limit and P.A.Signed (Edge'Result) = Value;
   function Bounds (Screen : G.Output; Origin : G.Logical_Point; Offset : L.Subpixel;
                    Damage : G.Physical_Rectangle) return G.Physical_Rectangle is
      Full : constant P.A.Result := P.Plan (Screen, Origin, Offset);
   begin
      if not Full.Visible then return G.Empty; end if;
      declare Clipped : constant P.A.Result := P.A.Clip (Full.Value, Screen.Width, Screen.Height, Damage); begin
         if not Clipped.Visible then return G.Empty; end if;
         return (G.Pixel_Edge (Clipped.Value.Clip_X), G.Pixel_Edge (Clipped.Value.Clip_Y),
                 Edge (P.A.Signed (Clipped.Value.Clip_X) + P.A.Signed (Clipped.Value.Clip_W), Screen.Width),
                 Edge (P.A.Signed (Clipped.Value.Clip_Y) + P.A.Signed (Clipped.Value.Clip_H), Screen.Height));
      end;
   end Bounds;
   function Inside (Index, Pitch : Natural; Area : G.Physical_Rectangle) return Boolean is
     (Index mod Pitch >= Natural (Area.Left) and Index mod Pitch < Natural (Area.Right) and
      Index / Pitch >= Natural (Area.Top) and Index / Pitch < Natural (Area.Bottom));
   function Index_Of (X, Y : Natural; Pitch : Positive) return Natural is (Y * Pitch + X)
     with Pre => X < Pitch and Y < Natural'Last / Pitch,
       Post => Index_Of'Result = Y * Pitch + X and
         Index_Of'Result / Pitch = Y and Index_Of'Result mod Pitch = X;
   procedure Set (Target : in out Pixels; I : Natural; Color : Word)
     with Pre => I in Target'Range,
       Post => Target (I) = Color and
         (for all J in Target'Range => (if J /= I then Target (J) = Target'Old (J)))
   is
   begin
      Target (I) := Color;
   end Set;
   procedure Paint (Screen : G.Output; Origin : G.Logical_Point; Offset : L.Subpixel;
                    Damage : G.Physical_Rectangle; Mask : Bytes;
                    Target : in out Pixels; Pitch : Positive; Tint : Word) is
      Area : constant G.Physical_Rectangle := Bounds (Screen, Origin, Offset, Damage);
      Where : constant Placement := Prepare (Screen, Origin, Offset);
      Ink_Row : constant Glyph_Gamma.Bucket := Glyph_Gamma.Bucket_Of (Tint);
      First : constant Natural := Where.Phase * L.Phase_Bytes (Where.Raster);
   begin
      if Where.Rotation = G.Unrotated then
         --  Unrotated: the raster's column and row are the pixel's less the
         --  placement's origin (At_Pixel's mapping), without the per-pixel
         --  rotation dispatch.
         for Y in Natural (Area.Top) .. Natural (Area.Bottom) - 1 loop
            pragma Loop_Invariant (for all I in Target'Range =>
              (if not Inside (I, Pitch, Area) then Target (I) = Target'Loop_Entry (I)));
            declare
               SY : constant P.A.Signed := P.A.Signed (Y) - Where.Top;
            begin
               if SY >= 0 and then SY < P.A.Signed (Where.Raster.Height) then
                  for X in Natural (Area.Left) .. Natural (Area.Right) - 1 loop
                     pragma Loop_Invariant (for all I in Target'Range =>
                       (if not Inside (I, Pitch, Area) then Target (I) = Target'Loop_Entry (I)));
                     declare
                        SX : constant P.A.Signed := P.A.Signed (X) - Where.Left;
                        I : constant Natural := Index_Of (X, Y, Pitch);
                     begin
                        pragma Assert (Inside (I, Pitch, Area));
                        if SX >= 0 and then SX < P.A.Signed (Where.Raster.Width) and then
                          Mask (First + Natural (SY) * Where.Raster.Pitch + Natural (SX)) /= 0
                        then
                           Set (Target, I, Corrected_Over
                             (Glyph_Gamma.Adjust (Ink_Row, Mask (First + Natural (SY) * Where.Raster.Pitch + Natural (SX))),
                              Tint, Target (I)));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
         return;
      end if;
      for Y in Natural (Area.Top) .. Natural (Area.Bottom) - 1 loop
         pragma Loop_Invariant (for all I in Target'Range =>
           (if not Inside (I, Pitch, Area) then Target (I) = Target'Loop_Entry (I)));
         for X in Natural (Area.Left) .. Natural (Area.Right) - 1 loop
            pragma Loop_Invariant (for all I in Target'Range =>
              (if not Inside (I, Pitch, Area) then Target (I) = Target'Loop_Entry (I)));
            declare
               S : constant Sample := At_Pixel (Where, (G.Pixel_Index (X), G.Pixel_Index (Y)));
               I : constant Natural := Index_Of (X, Y, Pitch);
            begin
               pragma Assert (Inside (I, Pitch, Area));
               --  No coverage blends to the very same pixel (Over with alpha 0
               --  returns the background): skip it. Most of a cell is empty.
               if S.Valid and then Mask (First + S.Y * Where.Raster.Pitch + S.X) /= 0 then
                  Set (Target, I, Corrected_Over
                    (Glyph_Gamma.Adjust (Ink_Row, Mask (First + S.Y * Where.Raster.Pitch + S.X)), Tint, Target (I)));
               end if;
            end;
         end loop;
      end loop;
   end Paint;
end Compositor_Glyph_Software;
