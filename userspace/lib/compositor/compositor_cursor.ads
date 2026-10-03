with CuBit.Display_Geometry;
-- Immutable cursor geometry. Keep the hotspot fixed in desktop coordinates;
-- map the complete shape to each output before clipping. No pixel storage.
package Compositor_Cursor with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Logical_Coordinate, G.Logical_Rectangle;
   subtype Extent is Positive range 1 .. 65_535;
   subtype Hotspot is Natural range 0 .. 65_534;
   type Plan is record
      Valid : Boolean := False;
      Surface : G.Logical_Rectangle := (0, 0, 0, 0);
   end record;
   function Build (Pointer : G.Logical_Point; Width, Height : Extent;
      Hot_X, Hot_Y : Hotspot) return Plan
     with Post =>
       (if Build'Result.Valid then
          Hot_X < Width and then Hot_Y < Height and then
          Build'Result.Surface.Left = Pointer.X - G.Logical_Coordinate (Hot_X) and then
          Build'Result.Surface.Top = Pointer.Y - G.Logical_Coordinate (Hot_Y) and then
          Build'Result.Surface.Right = Build'Result.Surface.Left + G.Logical_Coordinate (Width) and then
          Build'Result.Surface.Bottom = Build'Result.Surface.Top + G.Logical_Coordinate (Height)
        else Build'Result.Surface = G.Logical_Rectangle'(0, 0, 0, 0));
end Compositor_Cursor;
