-- Map nearest-sampled source damage into conservative logical client damage.
package Compositor_Source_Damage with SPARK_Mode, Pure is
   subtype Edge is Natural range 0 .. 65_535;
   subtype Extent is Edge range 1 .. Edge'Last;
   subtype Wide is Long_Long_Integer;
   type Interval is record
      First, Last : Edge;
   end record;
   function Axis (Low, High : Edge; Pixels, Logical : Extent) return Interval
     with Pre => Low < High and High <= Pixels,
       Post => Axis'Result.First < Axis'Result.Last and then
         Axis'Result.Last <= Logical and then
         Wide (Axis'Result.First) * Wide (Pixels) <= Wide (Low) * Wide (Logical) and then
         Wide (Axis'Result.Last) * Wide (Pixels) >= Wide (High) * Wide (Logical) and then
         Wide (Low) * Wide (Logical) - Wide (Axis'Result.First) * Wide (Pixels) < Wide (Pixels) and then
         Wide (Axis'Result.Last) * Wide (Pixels) - Wide (High) * Wide (Logical) < Wide (Pixels);
   type Rectangle is record
      X, Y, Width, Height : Edge := 0;
   end record;
   type Box is record
      Left, Top, Right, Bottom : Edge := 0;
   end record;
   Empty : constant Box := (others => 0);
   function Clipped_End (Start, Length : Edge; Limit : Extent) return Edge is
     (Edge (Natural'Min (Limit, Start + Length)));
   function Map
     (Source_Width, Source_Height, Logical_Width, Logical_Height : Extent;
      Area : Rectangle; Full : Boolean) return Box
     with Post => Map'Result.Left <= Map'Result.Right and then
       Map'Result.Top <= Map'Result.Bottom and then
       Map'Result.Right <= Logical_Width and then
       Map'Result.Bottom <= Logical_Height and then
       (if Full then Map'Result = (0, 0, Logical_Width, Logical_Height)
        elsif Area.X >= Clipped_End (Area.X, Area.Width, Source_Width) or else
          Area.Y >= Clipped_End (Area.Y, Area.Height, Source_Height)
        then Map'Result = Empty
        else
          Map'Result.Left = Axis
            (Area.X, Clipped_End (Area.X, Area.Width, Source_Width), Source_Width, Logical_Width).First and then
          Map'Result.Right = Axis
            (Area.X, Clipped_End (Area.X, Area.Width, Source_Width), Source_Width, Logical_Width).Last and then
          Map'Result.Top = Axis
            (Area.Y, Clipped_End (Area.Y, Area.Height, Source_Height), Source_Height, Logical_Height).First and then
          Map'Result.Bottom = Axis
            (Area.Y, Clipped_End (Area.Y, Area.Height, Source_Height), Source_Height, Logical_Height).Last);
end Compositor_Source_Damage;
