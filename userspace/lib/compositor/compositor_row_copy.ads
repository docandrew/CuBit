with CuBit.Display_Geometry;
package Compositor_Row_Copy with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   subtype Wide is Long_Long_Integer;
   use type G.Pixel_Edge, G.Scale_Component, G.Orientation;
   type Region is record
      Source_X, Source_Y, Target_X, Target_Y, Width, Height : Natural := 0;
   end record;
   type Readback_Batch is record
      Source_Offset, Target_Offset, Row_Bytes, Rows : Natural := 0;
   end record;
   -- Tight BGRA GPU readback into a pitched output writer. At most Byte_Budget
   -- payload bytes per event-loop call; no padding writes. Pointer authority,
   -- completion, nonaliasing and holding both owners remain caller obligations.
   function Readback_Plan
     (Width, Height, First_Row : G.Pixel_Edge;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural)
      return Readback_Batch
     with Post =>
       (if Readback_Plan'Result.Rows > 0 then
          Readback_Plan'Result.Row_Bytes = Natural (Width) * 4 and
          Readback_Plan'Result.Rows <= Natural (Height - First_Row) and
          Wide (Readback_Plan'Result.Rows) * Wide (Readback_Plan'Result.Row_Bytes) <= Wide (Byte_Budget) and
          Wide (Readback_Plan'Result.Source_Offset) +
            Wide (Readback_Plan'Result.Rows) * Wide (Readback_Plan'Result.Row_Bytes) <= Wide (Source_Bytes) and
          Wide (Readback_Plan'Result.Target_Offset) +
            Wide (Readback_Plan'Result.Rows - 1) * Wide (Target_Pitch) +
            Wide (Readback_Plan'Result.Row_Bytes) <= Wide (Target_Bytes)
        else Readback_Plan'Result = (0, 0, 0, 0));
   -- Opaque, unrotated 1:1 pixels only. No resampling or blending is skipped.
   -- Mapping authority and physical alias exclusion remain caller obligations.
   function Plan
     (Screen : G.Output; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : G.Physical_Extent;
      Damage : G.Physical_Rectangle) return Region
     with Post =>
       (if Plan'Result.Width > 0 then
          Plan'Result.Height > 0 and
          Screen.Rotation = G.Unrotated and
          Screen.Scale.Numerator = Screen.Scale.Denominator and
          Wide (Source_Width) = Wide (Surface.Right) - Wide (Surface.Left) and
          Wide (Source_Height) = Wide (Surface.Bottom) - Wide (Surface.Top) and
          Plan'Result.Source_X + Plan'Result.Width <= Natural (Source_Width) and
          Plan'Result.Source_Y + Plan'Result.Height <= Natural (Source_Height) and
          Plan'Result.Target_X + Plan'Result.Width <= Natural (Screen.Width) and
          Plan'Result.Target_Y + Plan'Result.Height <= Natural (Screen.Height) and
          Plan'Result.Target_X >= Natural (Damage.Left) and
          Plan'Result.Target_Y >= Natural (Damage.Top) and
          Plan'Result.Target_X + Plan'Result.Width <= Natural (Damage.Right) and
          Plan'Result.Target_Y + Plan'Result.Height <= Natural (Damage.Bottom) and
          Wide (Plan'Result.Source_X) = Wide (Plan'Result.Target_X) + Wide (Screen.X) - Wide (Surface.Left) and
          Wide (Plan'Result.Source_Y) = Wide (Plan'Result.Target_Y) + Wide (Screen.Y) - Wide (Surface.Top)
        else Plan'Result = (0, 0, 0, 0, 0, 0));
end Compositor_Row_Copy;
