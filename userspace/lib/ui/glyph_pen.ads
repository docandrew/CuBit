--  Pen arithmetic of the one text layout (UI-026): positions in 1/64 logical
--  pixel, from unrounded advances and pair kerning, placed on quarter-pixel
--  phases. CuBit.Fonts.Width and CuBit.Fonts.Lay_Out (the only measure and
--  placement paths of the toolkit and Desktop) do all their arithmetic here.
package Glyph_Pen with SPARK_Mode, Pure is
   Unit : constant := 64;
   Phases : constant := 4;
   Phase_Step : constant := Unit / Phases;
   --  Largest pen position: a 4 Mi-pixel line, far past any surface.
   Last_Pixel : constant := 2 ** 22;
   Last_Position : constant := Last_Pixel * Unit;
   --  A glyph advance is at most a raster cell wide; |kerning| at most four
   --  pixels (the bundled faces stay under two).
   Advance_Limit : constant := 32 * Unit;
   Kerning_Limit : constant := 4 * Unit;

   subtype Position is Natural range 0 .. Last_Position;
   subtype Pixel is Natural range 0 .. Last_Pixel;
   subtype Phase is Natural range 0 .. Phases - 1;
   subtype Advance is Natural range 0 .. Advance_Limit;
   subtype Kerning is Integer range -Kerning_Limit .. Kerning_Limit;

   function Start (X : Pixel) return Position is (X * Unit)
     with Post => Start'Result / Unit = X and Start'Result mod Unit = 0;

   --  The pen after a glyph (Width) and the kerning before the next one.
   --  Saturates at both ends instead of wrapping: text past Last_Pixel is
   --  clipped by every surface anyway.
   function Step (P : Position; Width : Advance; Pair : Kerning) return Position is
     (if Pair < 0 and then P + Width < -Pair then 0
      elsif P + Width + Pair > Last_Position then Last_Position
      else P + Width + Pair)
     with Post => Step'Result <= Last_Position and
       (if P + Width + Pair in Position then Step'Result = P + Width + Pair);

   --  Nearest quarter pixel: the pixel the glyph's origin column lands on,
   --  and the raster phase that supplies the remaining quarter.
   function Quarters (P : Position) return Natural is ((P + Phase_Step / 2) / Phase_Step)
     with Post => Quarters'Result <= Last_Pixel * Phases;
   function Pixel_Of (P : Position) return Pixel is (Quarters (P) / Phases)
     with Post => Pixel_Of'Result * Unit <= P + Phase_Step / 2 and
                  P < Pixel_Of'Result * Unit + Unit + Phase_Step / 2;
   function Phase_Of (P : Position) return Phase is (Quarters (P) mod Phases)
     with Post => Pixel_Of (P) * Unit + Phase_Of'Result * Phase_Step <= P + Phase_Step / 2 and
                  P + Phase_Step / 2 < Pixel_Of (P) * Unit + Phase_Of'Result * Phase_Step + Phase_Step;

   --  Whole-pixel placement, for glyph caches without phases.
   function Nearest (P : Position) return Pixel is ((P + Unit / 2) / Unit)
     with Post => Nearest'Result * Unit <= P + Unit / 2 and P + Unit / 2 < Nearest'Result * Unit + Unit;
end Glyph_Pen;
