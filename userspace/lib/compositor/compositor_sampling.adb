package body Compositor_Sampling with SPARK_Mode is
   use type G.Pixel_Edge, G.Logical_Coordinate;
   function Fine_Axis
     (Pixel : G.Pixel_Index; Scale : G.UI_Scale;
      Output_Origin : G.Output_Origin; Surface_Origin : G.Logical_Coordinate;
      Size : Logical_Size; Source_Pixels : Fine_Extent) return Fine_Axis_Result
   is
      N : constant Wide := Centre (Pixel, Scale, Output_Origin, Surface_Origin);
      D : constant Wide := Span (Size, Scale);
   begin
      if N < 0 or else N >= D then return (Valid => False); end if;
      return (True, Fine_Index (N * Wide (Source_Pixels) / D));
   end Fine_Axis;
   function Fine_Map
     (Screen : G.Output; Pixel : G.Physical_Point; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : Fine_Extent) return Fine_Sample
   is
      U, V : G.Pixel_Index;
      X, Y : Fine_Axis_Result;
   begin
      if Pixel.X >= Screen.Width or else Pixel.Y >= Screen.Height or else
        Surface.Left >= Surface.Right or else Surface.Top >= Surface.Bottom
      then return (Valid => False); end if;
      case Screen.Rotation is
         when G.Unrotated => U := Pixel.X; V := Pixel.Y;
         when G.Clockwise_90 => U := Pixel.Y; V := Screen.Width - 1 - Pixel.X;
         when G.Clockwise_180 =>
            U := Screen.Width - 1 - Pixel.X; V := Screen.Height - 1 - Pixel.Y;
         when G.Clockwise_270 => U := Screen.Height - 1 - Pixel.Y; V := Pixel.X;
      end case;
      X := Fine_Axis (U, Screen.Scale, Screen.X, Surface.Left,
                 Wide (Surface.Right) - Wide (Surface.Left), Source_Width);
      Y := Fine_Axis (V, Screen.Scale, Screen.Y, Surface.Top,
                 Wide (Surface.Bottom) - Wide (Surface.Top), Source_Height);
      if not X.Valid or else not Y.Valid then return (Valid => False); end if;
      return (True, X.Index, Y.Index);
   end Fine_Map;
   function Axis
     (Pixel : G.Pixel_Index; Scale : G.UI_Scale;
      Output_Origin : G.Output_Origin; Surface_Origin : G.Logical_Coordinate;
      Size : Logical_Size; Source_Pixels : G.Physical_Extent) return Axis_Result
   is
      R : constant Fine_Axis_Result := Fine_Axis
        (Pixel, Scale, Output_Origin, Surface_Origin, Size, Fine_Extent (Source_Pixels));
   begin
      if not R.Valid then return (Valid => False); end if;
      return (True, G.Pixel_Index (R.Index));
   end Axis;
   function Map
     (Screen : G.Output; Pixel : G.Physical_Point; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : G.Physical_Extent) return Sample
   is
      R : constant Fine_Sample := Fine_Map
        (Screen, Pixel, Surface, Fine_Extent (Source_Width), Fine_Extent (Source_Height));
   begin
      if not R.Valid then return (Valid => False); end if;
      return (True, G.Pixel_Index (R.X), G.Pixel_Index (R.Y));
   end Map;
end Compositor_Sampling;
