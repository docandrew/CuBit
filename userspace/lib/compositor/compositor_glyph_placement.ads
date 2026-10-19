with Compositor_Affine;
with Compositor_Glyph_Layout;
package Compositor_Glyph_Placement with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   package G renames A.G;
   package L renames Compositor_Glyph_Layout;
   use type A.Signed, A.Word;
   subtype Position is A.Signed range -(2 ** 35) .. 2 ** 35;
   -- Nearest physical origin; exact half-pixel ties go toward positive infinity.
   -- Snap each logical origin independently: never accumulate rounded advances.
   function Snap (Logical : G.Logical_Coordinate; Origin : G.Output_Origin;
                  Scale : G.UI_Scale) return Position
     with Post =>
       2 * Snap'Result * A.Signed (Scale.Denominator) - A.Signed (Scale.Denominator) <=
         2 * (A.Signed (Logical) - A.Signed (Origin)) * A.Signed (Scale.Numerator) and then
       2 * (A.Signed (Logical) - A.Signed (Origin)) * A.Signed (Scale.Numerator) <
         2 * Snap'Result * A.Signed (Scale.Denominator) + A.Signed (Scale.Denominator);
   -- Quarter-pixel placement along the text (UI-026): Logical plus Offset /
   -- L.Pen_Unit logical pixels, at Scale, rounded to the nearest 1 / L.Phases
   -- physical pixel (ties toward positive infinity): the raster's left pixel
   -- and the phase that supplies the remaining fraction.
   Phase_Step : constant := L.Pen_Unit / L.Phases;
   type Subpixel_Origin is record
      Pixel : Position;
      Phase : L.Phase;
   end record;
   function Snap_Phase (Logical : G.Logical_Coordinate; Offset : L.Subpixel;
                        Origin : G.Output_Origin; Scale : G.UI_Scale) return Subpixel_Origin
     with Post =>
       -- Q = Pixel * Phases + Phase quarters; V = the exact position in
       -- 1 / (Pen_Unit * Denominator) physical pixel: |Q * step - V| <= step / 2.
       2 * (Snap_Phase'Result.Pixel * L.Phases + A.Signed (Snap_Phase'Result.Phase)) *
         Phase_Step * A.Signed (Scale.Denominator) - Phase_Step * A.Signed (Scale.Denominator) <=
       2 * ((A.Signed (Logical) - A.Signed (Origin)) * L.Pen_Unit + A.Signed (Offset)) * A.Signed (Scale.Numerator)
       and then
       2 * ((A.Signed (Logical) - A.Signed (Origin)) * L.Pen_Unit + A.Signed (Offset)) * A.Signed (Scale.Numerator) <
         2 * (Snap_Phase'Result.Pixel * L.Phases + A.Signed (Snap_Phase'Result.Phase)) *
           Phase_Step * A.Signed (Scale.Denominator) + Phase_Step * A.Signed (Scale.Denominator);
   -- Rasterize at Screen.Scale, then place those pixels at unit scale. Scaling
   -- the ceil-rounded raster back to a logical line box would resample it.
   -- Origin.X carries Offset (its pen fraction): the raster's columns start at
   -- Snap_Phase (...).Pixel and its rows at Snap (Origin.Y, ...).
   function Plan (Screen : G.Output; Origin : G.Logical_Point; Offset : L.Subpixel) return A.Result
     with Post => (if Plan'Result.Visible then
       A.Valid (Plan'Result.Value, Screen.Width, Screen.Height) and then
       Plan'Result.Value.Numerator = 1 and then Plan'Result.Value.Denominator = 1 and then
       Plan'Result.Value.Over = 1 and then
       Plan'Result.Value.Rotation = A.Word (G.Orientation'Pos (Screen.Rotation)) and then
       Plan'Result.Value.Logical_W = A.Word (L.Plan (Screen.Scale).Width) and then
       Plan'Result.Value.Logical_H = A.Word (L.Plan (Screen.Scale).Height) and then
       Plan'Result.Value.Origin_X = -Snap_Phase (Origin.X, Offset, Screen.X, Screen.Scale).Pixel and then
       Plan'Result.Value.Origin_Y = -Snap (Origin.Y, Screen.Y, Screen.Scale));
end Compositor_Glyph_Placement;
