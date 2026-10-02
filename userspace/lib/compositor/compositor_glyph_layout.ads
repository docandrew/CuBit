with CuBit.Display_Geometry;
package Compositor_Glyph_Layout with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Scale_Component;
   -- Bundled-font raster request. Metrics and outline parsing are foreign;
   -- storage geometry and the selected density are compositor policy.
   Base_Em : constant := 13;
   Base_Line : constant := 17;
   Base_Width_Bound : constant := 32;
   Row_Alignment : constant := 16;
   Maximum_Bytes : constant := 512 * 272;
   subtype Raster_Width is Positive range 1 .. 512;
   subtype Raster_Height is Positive range 1 .. 272;
   subtype Byte_Length is Positive range 1 .. Maximum_Bytes;
   type Layout is record
      Em_Numerator : Positive range 13 .. 208;
      Em_Denominator : G.Scale_Component;
      Width : Raster_Width;
      Height : Raster_Height;
      Pitch : Raster_Width;
      Bytes : Byte_Length;
   end record;
   function Valid (L : Layout) return Boolean is
     (L.Pitch >= L.Width and then L.Pitch mod Row_Alignment = 0 and then
      L.Pitch - L.Width < Row_Alignment and then L.Bytes = L.Pitch * L.Height);
   function Same_Raster (Left, Right : Layout) return Boolean is
     (Left.Width = Right.Width and Left.Height = Right.Height and
      Left.Pitch = Right.Pitch and Left.Bytes = Right.Bytes and
      Left.Em_Numerator * Positive (Right.Em_Denominator) =
        Right.Em_Numerator * Positive (Left.Em_Denominator));
   function Plan (Scale : G.UI_Scale) return Layout
     with Post => Valid (Plan'Result) and then
       Plan'Result.Em_Numerator = Base_Em * Positive (Scale.Numerator) and then
       Plan'Result.Em_Denominator = Scale.Denominator and then
       (Plan'Result.Width - 1) * Positive (Scale.Denominator) < Base_Width_Bound * Positive (Scale.Numerator) and then
       Plan'Result.Width * Positive (Scale.Denominator) >= Base_Width_Bound * Positive (Scale.Numerator) and then
       (Plan'Result.Height - 1) * Positive (Scale.Denominator) < Base_Line * Positive (Scale.Numerator) and then
       Plan'Result.Height * Positive (Scale.Denominator) >= Base_Line * Positive (Scale.Numerator);
end Compositor_Glyph_Layout;
