package body Compositor_Upload with SPARK_Mode is
   function End_Byte (P : Plan) return Byte_Count is
     (Byte_Count (Long_Long_Integer (P.Offset) + Span (P.Region, P.Row_Pixels, P.Kind)));
   procedure Make (Width, Height : Edge; Buffer_Size : Byte_Count;
      Region : Rectangle; Offset : Byte_Count; Row_Pixels : Edge;
      Kind : Pixel_Format; P : out Plan; Accepted : out Boolean) is
   begin
      Accepted := Fits (Width, Height, Buffer_Size, Region, Offset, Row_Pixels, Kind);
      P := (Ready => Accepted, Width => Width, Height => Height, Region => Region,
            Offset => Offset, Bytes => Buffer_Size, Row_Pixels => Row_Pixels, Kind => Kind);
   end Make;
   procedure Row_Chunk (Width, Height, First_Row : Edge; Buffer_Size : Byte_Count;
      Kind : Pixel_Format; P : out Plan; Accepted : out Boolean;
      Row_Pixels : Edge := 0) is
      Rows : Natural;
      Stride : constant Edge := (if Row_Pixels = 0 then Width else Row_Pixels);
   begin
      P := (others => <>); Accepted := False;
      if Width = 0 or else Stride < Width or else First_Row >= Height then return; end if;
      Rows := Natural'Min (Height - First_Row, Buffer_Size / (Stride * Pixel_Bytes (Kind)));
      if Rows = 0 then return; end if;
      Make (Width, Height, Buffer_Size, (0, First_Row, Width, Edge (Rows)), 0, Row_Pixels, Kind, P, Accepted);
   end Row_Chunk;
end Compositor_Upload;
