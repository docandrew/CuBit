--  The Calculator's key symbols (docs/calculator.md, "The app"): + - x /
--  = square root, pi and erase, drawn as strokes of one weight and
--  rasterized at the canvas's physical resolution, anti-aliased, so they
--  stay crisp and centred at 100%, 150% and 200% and match the stems of
--  the key font. The fonts' character set has no pi, root or erase sign,
--  and its x and / would not share one weight with the others.
--
--  Platform-free: coverage in, the window turns it into ink (Calc_Shells).
--  Proved (level 1): no run-time errors.
package Calc_Symbols with SPARK_Mode is
   type Symbol is (Plus_Symbol, Minus_Symbol, Times_Symbol, Divide_Symbol, Equals_Symbol, Root_Symbol, Pi_Symbol,
                   Erase_Symbol);

   --  Sub-pixel unit of positions and stroke widths: 1/64 physical pixel.
   SUBPIXEL : constant := 64;
   --  The largest symbol box, in physical pixels (a 32-pixel symbol at
   --  sixteen times density).
   MAXIMUM_EDGE : constant := 512;
   subtype Edge is Positive range 1 .. MAXIMUM_EDGE;
   --  Stroke widths up to a sixth of the largest box.
   subtype Stroke_Width is Positive range 1 .. MAXIMUM_EDGE * SUBPIXEL / 6;

   OPAQUE : constant := 255;
   subtype Coverage is Natural range 0 .. OPAQUE;
   --  Row, column; 0 transparent, OPAQUE fully inked.
   type Coverage_Map is array (Natural range <>, Natural range <>) of Coverage;

   --  Item drawn into a square box of Into'Length physical pixels with
   --  strokes Stroke wide (1/64 pixel).
   procedure Rasterize (Item : Symbol; Stroke : Stroke_Width; Into : out Coverage_Map)
     with Pre => Into'First (1) = 0 and then Into'First (2) = 0
                 and then Into'Length (1) in Edge and then Into'Length (2) = Into'Length (1);
end Calc_Symbols;
