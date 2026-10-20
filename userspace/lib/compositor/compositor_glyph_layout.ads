with CuBit.Display_Geometry;
package Compositor_Glyph_Layout with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Scale_Component;
   -- Bundled-font raster request. Metrics and outline parsing are foreign;
   -- storage geometry and the selected density are compositor policy.
   Base_Em : constant := 13;
   -- Bundled faces by the font engine's face number (CuBit.Fonts.Face'Pos):
   -- Sans, Monospace and the emphasis face Sans_Semibold.
   Face_Count : constant := 3;
   subtype Face_Index is Natural range 0 .. Face_Count - 1;
   Sans_Face : constant Face_Index := 0;
   Semibold_Face : constant Face_Index := 2;
   -- Rasterized code points: printable ASCII.
   First_Code : constant := 32;
   Last_Code : constant := 126;
   subtype Code_Point is Natural range First_Code .. Last_Code;
   Code_Count : constant := Last_Code - First_Code + 1;
   -- Every face's whole printable set at one density: the most distinct
   -- glyph keys one frame (one output density) can draw.
   Repertoire : constant := Face_Count * Code_Count;
   Base_Line : constant := 17;
   Base_Width_Bound : constant := 32;
   Row_Alignment : constant := 16;
   -- Horizontal subpixel phases (UI-026): a mask holds one raster per phase,
   -- stacked: phase P is rows P * Height .. P * Height + Height - 1, its ink
   -- shifted right by P / Phases of a pixel of the raster's density. Glyph
   -- pens are in 1/Pen_Unit logical pixel (Glyph_Pen.Unit).
   Phases : constant := 4;
   Pen_Unit : constant := 64;
   subtype Phase is Natural range 0 .. Phases - 1;
   -- Grid fitting (UI-026, low density): below Grid_Fit_Numerator /
   -- Grid_Fit_Denominator device pixels per logical pixel the font engine
   -- fits each phase's vertical stems to whole pixels of that phase's raster
   -- (glyphs still go on quarter-pixel phases, so a stem moves at most half
   -- a pixel and spacing stays even); at and above it, stems keep their
   -- outline positions. Why 3/2: the bundled faces' standard stem is 1.14
   -- px at the 13 px em (IBM Plex Sans), and a quarter phase spreads a stem
   -- over two columns: averaged over the four phases its darkest column
   -- loses 18% of its contrast at 100% and 8% at 125%, but 1% at 150% (a
   -- 1.71 px stem keeps a full column at every phase) and none at 200%.
   Grid_Fit_Numerator : constant := 3;
   Grid_Fit_Denominator : constant := 2;
   function Grid_Fitted (Scale : G.UI_Scale) return Boolean is
     (Natural (Scale.Numerator) * Grid_Fit_Denominator < Grid_Fit_Numerator * Natural (Scale.Denominator));
   -- A glyph origin's fraction of a logical pixel, in 1/Pen_Unit.
   subtype Subpixel is Natural range 0 .. Pen_Unit - 1;
   Last_Width : constant := 512;
   Last_Height : constant := 272;
   Maximum_Bytes : constant := Last_Width * Last_Height * Phases;
   subtype Raster_Width is Positive range 1 .. Last_Width;
   subtype Raster_Height is Positive range 1 .. Last_Height;
   subtype Byte_Length is Positive range 1 .. Maximum_Bytes;
   -- One mask's bytes at density 2: owners that budget every slot at this
   -- size keep the whole printable set resident up to 2x.
   Dense_Scale : constant := 2;
   Dense_Bytes : constant :=
     ((Base_Width_Bound * Dense_Scale + Row_Alignment - 1) / Row_Alignment) * Row_Alignment *
     Base_Line * Dense_Scale * Phases;
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
      L.Pitch - L.Width < Row_Alignment and then L.Bytes = L.Pitch * L.Height * Phases);
   -- One phase's raster, and the rows of the whole stacked mask.
   function Phase_Bytes (L : Layout) return Positive is (L.Pitch * L.Height)
     with Pre => Valid (L), Post => Phase_Bytes'Result * Phases = L.Bytes;
   function Strip_Height (L : Layout) return Positive is (L.Height * Phases)
     with Post => Strip_Height'Result <= Last_Height * Phases;
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
