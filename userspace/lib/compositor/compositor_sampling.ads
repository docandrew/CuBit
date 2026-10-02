with CuBit.Display_Geometry;
package Compositor_Sampling with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   subtype Wide is Long_Long_Integer;
   subtype Logical_Size is Wide range 1 .. 2 ** 31;
   type Axis_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Index : G.Pixel_Index;
         when False => null;
      end case;
   end record;
   -- Twice the output pixel centre in surface-local logical coordinates,
   -- multiplied by the scale numerator. Keep fractions until the final sample.
   function Centre (Pixel : G.Pixel_Index; Scale : G.UI_Scale;
                    Output_Origin : G.Output_Origin;
                    Surface_Origin : G.Logical_Coordinate) return Wide is
     ((2 * Wide (Pixel) + 1) * Wide (Scale.Denominator) +
        2 * Wide (Scale.Numerator) * (Wide (Output_Origin) - Wide (Surface_Origin)));
   function Span (Size : Logical_Size; Scale : G.UI_Scale) return Wide is
     (2 * Wide (Scale.Numerator) * Size);
   -- Fine source grids permit subpixel filter coordinates without truncating
   -- the output transform to a logical pixel first.
   subtype Fine_Extent is Positive range 1 .. 16_777_216;
   subtype Fine_Index is Natural range 0 .. Fine_Extent'Last - 1;
   type Fine_Axis_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Index : Fine_Index;
         when False => null;
      end case;
   end record;
   function Fine_Axis
     (Pixel : G.Pixel_Index; Scale : G.UI_Scale;
      Output_Origin : G.Output_Origin; Surface_Origin : G.Logical_Coordinate;
      Size : Logical_Size; Source_Pixels : Fine_Extent) return Fine_Axis_Result
     with Post => Fine_Axis'Result.Valid =
       (Centre (Pixel, Scale, Output_Origin, Surface_Origin) >= 0 and
        Centre (Pixel, Scale, Output_Origin, Surface_Origin) < Span (Size, Scale)) and then
       (if Fine_Axis'Result.Valid then
          Fine_Axis'Result.Index < Source_Pixels and then
          Wide (Fine_Axis'Result.Index) =
            Centre (Pixel, Scale, Output_Origin, Surface_Origin) * Wide (Source_Pixels) /
              Span (Size, Scale));
   type Fine_Sample (Valid : Boolean := False) is record
      case Valid is
         when True => X, Y : Fine_Index;
         when False => null;
      end case;
   end record;
   function Fine_Map
     (Screen : G.Output; Pixel : G.Physical_Point; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : Fine_Extent) return Fine_Sample
     with Post => (if Fine_Map'Result.Valid then
       Fine_Map'Result.X < Source_Width and Fine_Map'Result.Y < Source_Height);
   function Axis
     (Pixel : G.Pixel_Index; Scale : G.UI_Scale;
      Output_Origin : G.Output_Origin; Surface_Origin : G.Logical_Coordinate;
      Size : Logical_Size; Source_Pixels : G.Physical_Extent) return Axis_Result
     with Post => Axis'Result.Valid =
       (Centre (Pixel, Scale, Output_Origin, Surface_Origin) >= 0 and
        Centre (Pixel, Scale, Output_Origin, Surface_Origin) < Span (Size, Scale)) and then
       (if Axis'Result.Valid then
          Wide (Axis'Result.Index) < Wide (Source_Pixels) and then
          Wide (Axis'Result.Index) =
            Centre (Pixel, Scale, Output_Origin, Surface_Origin) * Wide (Source_Pixels) /
              Span (Size, Scale));
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => X, Y : G.Pixel_Index;
         when False => null;
      end case;
   end record;
   function Map
     (Screen : G.Output; Pixel : G.Physical_Point; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : G.Physical_Extent) return Sample
     with Post => (if Map'Result.Valid then
       Wide (Map'Result.X) < Wide (Source_Width) and
       Wide (Map'Result.Y) < Wide (Source_Height));
end Compositor_Sampling;
