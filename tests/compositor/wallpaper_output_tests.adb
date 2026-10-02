with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.Display_Geometry;
with Compositor_Text;
with Desktop_Wallpaper;
with Wallpaper_Assets;
procedure Wallpaper_Output_Tests is
   package G renames CuBit.Display_Geometry;
   package A renames CuBit.Appearance;
   use type G.Pixel_Edge, G.Logical_Coordinate;
   Sentinel : constant Unsigned_32 := 16#DEAD_BEEF#;
   Stride : constant := 69;
   type Pixels is array (Natural range <>) of Unsigned_32 with Convention => C;
   Whole, Tiled, Legacy : aliased Pixels (0 .. Stride * 48 + 1);
   Screen : G.Output := (64, 48, others => <>);
   Bounds : G.Logical_Rectangle := (-7, 5, 25, 29);
   Clip : G.Physical_Rectangle;
   Cases : Natural := 0;
   type Scales is array (Positive range <>) of G.UI_Scale;
   Densities : constant Scales := [(1, 1), (5, 4), (3, 2), (2, 1)];
begin
   -- Synthetic immutable atlases make sampling errors visible in both axes.
   for I in Wallpaper_Assets.Wallpaper'Range loop
      Wallpaper_Assets.Wallpaper (I) := 16#FF00_0000# or
        Shift_Left (Unsigned_32 (I mod 251), 16) or
        Shift_Left (Unsigned_32 ((I / 2048) mod 241), 8) or Unsigned_32 (I mod 239);
   end loop;
   for I in Wallpaper_Assets.Cubie'Range loop
      Wallpaper_Assets.Cubie (I) := 16#FF00_0000# or
        Shift_Left (Unsigned_32 ((I / 2048) mod 233), 16) or
        Shift_Left (Unsigned_32 (I mod 229), 8) or Unsigned_32 (I mod 227);
   end loop;
   for Scheme in A.Color_Scheme loop
      for Backdrop in A.Background loop
         for Placement in A.Placement loop
            for Density of Densities loop
               for Rotation in G.Orientation loop
                  Screen := (64, 48, Rotation, Density, -9, 3);
                  Whole := (others => Sentinel); Tiled := Whole;
                  Desktop_Wallpaper.Paint_Output (Whole (1)'Address, Stride * 4,
                    Screen, Bounds, (0, 0, 64, 48), (Scheme, Backdrop, Placement));
                  for Y in 0 .. 1 loop
                     for X in 0 .. 1 loop
                        Desktop_Wallpaper.Paint_Output (Tiled (1)'Address, Stride * 4,
                          Screen, Bounds,
                          (G.Pixel_Edge (X * 32), G.Pixel_Edge (Y * 24),
                           G.Pixel_Edge ((X + 1) * 32), G.Pixel_Edge ((Y + 1) * 24)),
                          (Scheme, Backdrop, Placement));
                     end loop;
                  end loop;
                  pragma Assert (Whole = Tiled);
                  pragma Assert (Whole (0) = Sentinel and Whole (Whole'Last) = Sentinel);
                  Clip := Compositor_Text.Clip (Screen, Bounds, (0, 0, 64, 48));
                  for Y in 0 .. 47 loop
                     for X in 0 .. Stride - 1 loop
                        if X >= Natural (Clip.Left) and X < Natural (Clip.Right) and
                          Y >= Natural (Clip.Top) and Y < Natural (Clip.Bottom)
                        then
                           pragma Assert (Whole (1 + Y * Stride + X) /= Sentinel);
                        else
                           pragma Assert (Whole (1 + Y * Stride + X) = Sentinel);
                        end if;
                     end loop;
                  end loop;
                  Cases := Cases + 1;
               end loop;
            end loop;
            -- At unit density the direct writer must retain the legacy filter.
            Screen := (64, 48, others => <>);
            Whole := (others => Sentinel); Legacy := Whole;
            Desktop_Wallpaper.Paint_Output (Whole (1)'Address, Stride * 4, Screen,
              (0, 0, 32, 24), (0, 0, 64, 48), (Scheme, Backdrop, Placement));
            Desktop_Wallpaper.Paint (Legacy (1)'Address, 32, 24, Stride * 4,
              0, 0, 32, 24, (Scheme, Backdrop, Placement));
            pragma Assert (Whole = Legacy);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS real wallpaper writer:" & Cases'Image &
     " scaled/rotated/style cases, damage tiling, padded-row guards, legacy filter parity");
end Wallpaper_Output_Tests;
