with CuBit.Display_Geometry;
-- Two checker rectangles replace the desktop shadow's per-pixel command list.
-- No storage, allocation, timing, foreign calls or animation policy.
package Compositor_Shadow with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Logical_Coordinate, G.Logical_Rectangle;
   subtype Depth is Natural range 0 .. 16;
   type Strips is array (1 .. 2) of G.Logical_Rectangle;
   type Plan is record
      Valid : Boolean := False;
      Areas : Strips := (others => (0, 0, 0, 0));
   end record;
   Empty : constant Strips := (others => (0, 0, 0, 0));
   function Build (Window : G.Logical_Rectangle; Offset : Depth) return Plan
     with Post =>
       (if Offset = 0 or Window.Left >= Window.Right or Window.Top >= Window.Bottom then
          Build'Result.Valid and Build'Result.Areas = Empty
        elsif Window.Right > G.Logical_Coordinate'Last - G.Logical_Coordinate (Offset) or
          Window.Bottom > G.Logical_Coordinate'Last - G.Logical_Coordinate (Offset) then
          not Build'Result.Valid and Build'Result.Areas = Empty
        else Build'Result.Valid and then
          Build'Result.Areas (1) = G.Logical_Rectangle'
            (Window.Right, Window.Top + G.Logical_Coordinate (Offset),
             Window.Right + G.Logical_Coordinate (Offset), Window.Bottom + G.Logical_Coordinate (Offset)) and then
          Build'Result.Areas (2) =
            (if Long_Long_Integer (Window.Right) - Long_Long_Integer (Window.Left) > Long_Long_Integer (Offset)
             then G.Logical_Rectangle'(Window.Left + G.Logical_Coordinate (Offset), Window.Bottom,
                 Window.Right, Window.Bottom + G.Logical_Coordinate (Offset))
             else G.Logical_Rectangle'(0, 0, 0, 0)));
   -- Exactly the union of outward-rounded even-parity logical pixel cells,
   -- including fractional DPI where adjacent logical cells overlap physically.
   -- This is intentionally not a pixel-center sample of a checker texture.
   function Paints (Screen : G.Output; Area : G.Logical_Rectangle;
      Pixel : G.Physical_Point) return Boolean;
end Compositor_Shadow;
