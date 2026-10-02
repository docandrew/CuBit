package body Compositor_Source_Damage with SPARK_Mode is
   function Axis (Low, High : Edge; Pixels, Logical : Extent) return Interval is
      A : constant Wide := Wide (Low) * Wide (Logical);
      B : constant Wide := Wide (High) * Wide (Logical);
      D : constant Wide := Wide (Pixels);
   begin
      return (Edge (A / D), Edge ((B + D - 1) / D));
   end Axis;
   function Map
     (Source_Width, Source_Height, Logical_Width, Logical_Height : Extent;
      Area : Rectangle; Full : Boolean) return Box
   is
      Right : constant Edge := Clipped_End (Area.X, Area.Width, Source_Width);
      Bottom : constant Edge := Clipped_End (Area.Y, Area.Height, Source_Height);
      X, Y : Interval;
   begin
      if Full then return (0, 0, Logical_Width, Logical_Height); end if;
      if Area.X >= Right or else Area.Y >= Bottom then return Empty; end if;
      X := Axis (Area.X, Right, Source_Width, Logical_Width);
      Y := Axis (Area.Y, Bottom, Source_Height, Logical_Height);
      return (X.First, Y.First, X.Last, Y.Last);
   end Map;
end Compositor_Source_Damage;
