package body Compositor_Cursor with SPARK_Mode is
   function Build (Pointer : G.Logical_Point; Width, Height : Extent;
      Hot_X, Hot_Y : Hotspot) return Plan is
      Left : constant Long_Long_Integer := Long_Long_Integer (Pointer.X) - Long_Long_Integer (Hot_X);
      Top : constant Long_Long_Integer := Long_Long_Integer (Pointer.Y) - Long_Long_Integer (Hot_Y);
      Right : constant Long_Long_Integer := Left + Long_Long_Integer (Width);
      Bottom : constant Long_Long_Integer := Top + Long_Long_Integer (Height);
   begin
      if Hot_X >= Width or Hot_Y >= Height or
        Left < Long_Long_Integer (G.Logical_Coordinate'First) or
        Top < Long_Long_Integer (G.Logical_Coordinate'First) or
        Right > Long_Long_Integer (G.Logical_Coordinate'Last) or
        Bottom > Long_Long_Integer (G.Logical_Coordinate'Last)
      then return (False, (0, 0, 0, 0)); end if;
      return (True, (G.Logical_Coordinate (Left), G.Logical_Coordinate (Top),
        G.Logical_Coordinate (Right), G.Logical_Coordinate (Bottom)));
   end Build;
end Compositor_Cursor;
