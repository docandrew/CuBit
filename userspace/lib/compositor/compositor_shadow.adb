package body Compositor_Shadow with SPARK_Mode is
   use type G.Logical_Coordinate, G.Pixel_Edge;
   function Build (Window : G.Logical_Rectangle; Offset : Depth) return Plan is
      D : constant G.Logical_Coordinate := G.Logical_Coordinate (Offset);
      Result : Plan := (Valid => True, others => <>);
   begin
      if Offset = 0 or Window.Left >= Window.Right or Window.Top >= Window.Bottom then
         return Result;
      end if;
      if Window.Right > G.Logical_Coordinate'Last - D or
        Window.Bottom > G.Logical_Coordinate'Last - D
      then return (others => <>); end if;
      Result.Areas (1) := (Window.Right, Window.Top + D,
        Window.Right + D, Window.Bottom + D);
      if Long_Long_Integer (Window.Right) - Long_Long_Integer (Window.Left) > Long_Long_Integer (D) then
         Result.Areas (2) := (Window.Left + D, Window.Bottom,
           Window.Right, Window.Bottom + D);
      end if;
      return Result;
   end Build;
   function Paints (Screen : G.Output; Area : G.Logical_Rectangle;
      Pixel : G.Physical_Point) return Boolean is
      subtype Wide is Long_Long_Integer;
      U, V : G.Pixel_Index;
      N : constant Wide := Wide (Screen.Scale.Numerator);
      D : constant Wide := Wide (Screen.Scale.Denominator);
      X0, Y0, X1, Y1 : Wide;
   begin
      if Pixel.X >= Screen.Width or Pixel.Y >= Screen.Height or
        Area.Left >= Area.Right or Area.Top >= Area.Bottom then return False; end if;
      case Screen.Rotation is
         when G.Unrotated => U := Pixel.X; V := Pixel.Y;
         when G.Clockwise_90 => U := Pixel.Y; V := Screen.Width - 1 - Pixel.X;
         when G.Clockwise_180 => U := Screen.Width - 1 - Pixel.X; V := Screen.Height - 1 - Pixel.Y;
         when G.Clockwise_270 => U := Screen.Height - 1 - Pixel.Y; V := Pixel.X;
      end case;
      -- Inclusive logical cell indices whose scaled rectangles overlap this
      -- native pixel. All divisions are nonnegative before adding the origin.
      X0 := Wide'Max (Wide (Area.Left), Wide (Screen.X) + Wide (U) * D / N);
      Y0 := Wide'Max (Wide (Area.Top), Wide (Screen.Y) + Wide (V) * D / N);
      X1 := Wide'Min (Wide (Area.Right) - 1, Wide (Screen.X) + ((Wide (U) + 1) * D - 1) / N);
      Y1 := Wide'Min (Wide (Area.Bottom) - 1, Wide (Screen.Y) + ((Wide (V) + 1) * D - 1) / N);
      return X0 <= X1 and then Y0 <= Y1 and then
        (X0 < X1 or Y0 < Y1 or (X0 + Y0) mod 2 = 0);
   end Paints;
end Compositor_Shadow;
