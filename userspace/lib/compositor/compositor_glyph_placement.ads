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
   -- Rasterize at Screen.Scale, then place those pixels at unit scale. Scaling
   -- the ceil-rounded raster back to a logical line box would resample it.
   function Plan (Screen : G.Output; Origin : G.Logical_Point) return A.Result
     with Post => (if Plan'Result.Visible then
       A.Valid (Plan'Result.Value, Screen.Width, Screen.Height) and then
       Plan'Result.Value.Numerator = 1 and then Plan'Result.Value.Denominator = 1 and then
       Plan'Result.Value.Over = 1 and then
       Plan'Result.Value.Rotation = A.Word (G.Orientation'Pos (Screen.Rotation)) and then
       Plan'Result.Value.Logical_W = A.Word (L.Plan (Screen.Scale).Width) and then
       Plan'Result.Value.Logical_H = A.Word (L.Plan (Screen.Scale).Height) and then
       Plan'Result.Value.Origin_X = -Snap (Origin.X, Screen.X, Screen.Scale) and then
       Plan'Result.Value.Origin_Y = -Snap (Origin.Y, Screen.Y, Screen.Scale));
end Compositor_Glyph_Placement;
