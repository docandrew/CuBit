pragma Ada_2022;
package body CuBit.Display_Geometry with SPARK_Mode is
   type Wide is range -2 ** 62 .. 2 ** 62 - 1;
   function Rotated (Screen : Output) return Boolean is
     (Screen.Rotation in Clockwise_90 | Clockwise_270);
   function Upright_Width (Screen : Output) return Physical_Extent is
     (if Rotated (Screen) then Screen.Height else Screen.Width);
   function Upright_Height (Screen : Output) return Physical_Extent is
     (if Rotated (Screen) then Screen.Width else Screen.Height);

   function Bounds (Screen : Output) return Logical_Rectangle is
     (declare
         N : constant Wide := Wide (Screen.Scale.Numerator);
         D : constant Wide := Wide (Screen.Scale.Denominator);
      begin
        (Left => Screen.X, Top => Screen.Y,
         Right => Screen.X + Logical_Coordinate
           ((Wide (Upright_Width (Screen)) * D + N - 1) / N),
         Bottom => Screen.Y + Logical_Coordinate
           ((Wide (Upright_Height (Screen)) * D + N - 1) / N)));

   function Contains (Screen : Output; Point : Logical_Point) return Boolean is
     (declare B : constant Logical_Rectangle := Bounds (Screen);
      begin Point.X >= B.Left and then Point.X < B.Right and then
        Point.Y >= B.Top and then Point.Y < B.Bottom);

   function Confine (Screen : Output; Point : Logical_Point) return Logical_Point is
     (declare B : constant Logical_Rectangle := Bounds (Screen);
      begin
        (Logical_Coordinate'Max (B.Left, Logical_Coordinate'Min (B.Right - 1, Point.X)),
         Logical_Coordinate'Max (B.Top, Logical_Coordinate'Min (B.Bottom - 1, Point.Y))));

   function Damage (Screen : Output; Area : Logical_Rectangle)
     return Physical_Rectangle
   is
      B : constant Logical_Rectangle := Bounds (Screen);
      L : constant Logical_Coordinate :=
        Logical_Coordinate'Max (B.Left, Area.Left);
      T : constant Logical_Coordinate :=
        Logical_Coordinate'Max (B.Top, Area.Top);
      R : constant Logical_Coordinate :=
        Logical_Coordinate'Min (B.Right, Area.Right);
      H : constant Logical_Coordinate :=
        Logical_Coordinate'Min (B.Bottom, Area.Bottom);
      N : constant Wide := Wide (Screen.Scale.Numerator);
      D : constant Wide := Wide (Screen.Scale.Denominator);
      W : constant Wide := Wide (Upright_Width (Screen));
      V : constant Wide := Wide (Upright_Height (Screen));
      X0, Y0, X1, Y1 : Pixel_Edge;
   begin
      if L >= R or else T >= H then
         return Empty;
      end if;
      X0 := Pixel_Edge (Wide'Min (W, (Wide (L) - Wide (B.Left)) * N / D));
      Y0 := Pixel_Edge (Wide'Min (V, (Wide (T) - Wide (B.Top)) * N / D));
      X1 := Pixel_Edge
        (Wide'Min (W, ((Wide (R) - Wide (B.Left)) * N + D - 1) / D));
      Y1 := Pixel_Edge
        (Wide'Min (V, ((Wide (H) - Wide (B.Top)) * N + D - 1) / D));
      case Screen.Rotation is
         when Unrotated => return (X0, Y0, X1, Y1);
         when Clockwise_90 =>
            return (Screen.Width - Y1, X0, Screen.Width - Y0, X1);
         when Clockwise_180 =>
            return (Screen.Width - X1, Screen.Height - Y1,
                    Screen.Width - X0, Screen.Height - Y0);
         when Clockwise_270 =>
            return (Y0, Screen.Height - X1, Y1, Screen.Height - X0);
      end case;
   end Damage;

   function To_Desktop (Screen : Output; Point : Physical_Point)
     return Point_Mapping
   is
      X, Y : Pixel_Index;
      N : constant Wide := Wide (Screen.Scale.Numerator);
      D : constant Wide := Wide (Screen.Scale.Denominator);
   begin
      if Point.X >= Screen.Width or else Point.Y >= Screen.Height then
         return (Valid => False);
      end if;
      case Screen.Rotation is
         when Unrotated => X := Point.X; Y := Point.Y;
         when Clockwise_90 => X := Point.Y; Y := Screen.Width - 1 - Point.X;
         when Clockwise_180 =>
            X := Screen.Width - 1 - Point.X; Y := Screen.Height - 1 - Point.Y;
         when Clockwise_270 => X := Screen.Height - 1 - Point.Y; Y := Point.X;
      end case;
      return (Valid => True, Value =>
        (X => Screen.X + Logical_Coordinate
           (((2 * Wide (X) + 1) * D) / (2 * N)),
         Y => Screen.Y + Logical_Coordinate
           (((2 * Wide (Y) + 1) * D) / (2 * N))));
   end To_Desktop;
end CuBit.Display_Geometry;
