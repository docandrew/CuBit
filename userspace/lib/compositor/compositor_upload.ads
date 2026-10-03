-- Checked uncompressed upload geometry. Does not grant pointer authority,
-- synchronize producers, or mark a partially initialized image publishable.
package Compositor_Upload with SPARK_Mode, Pure is
   subtype Edge is Natural range 0 .. 65535;
   subtype Byte_Count is Natural range 0 .. 16 * 1024 * 1024;
   type Pixel_Format is (BGRA8, R8);
   type Rectangle is record X, Y, Width, Height : Edge := 0; end record;
   type Plan is private;
   function Pixel_Bytes (Format : Pixel_Format) return Positive is
     (if Format = BGRA8 then 4 else 1);
   function Valid (P : Plan) return Boolean;
   function Area (P : Plan) return Rectangle;
   function Image_Width (P : Plan) return Edge;
   function Image_Height (P : Plan) return Edge;
   function Buffer_Offset (P : Plan) return Byte_Count;
   function Row_Length (P : Plan) return Edge;
   function Format (P : Plan) return Pixel_Format;
   function Capacity (P : Plan) return Byte_Count;
   function End_Byte (P : Plan) return Byte_Count with Pre => Valid (P),
     Post => End_Byte'Result <= Capacity (P);
   procedure Make (Width, Height : Edge; Buffer_Size : Byte_Count;
      Region : Rectangle; Offset : Byte_Count; Row_Pixels : Edge;
      Kind : Pixel_Format; P : out Plan; Accepted : out Boolean)
     with Post => Accepted = Valid (P) and (if Accepted then
       Area (P) = Region and Image_Width (P) = Width and Image_Height (P) = Height and
       Capacity (P) = Buffer_Size and Buffer_Offset (P) = Offset and
       Row_Length (P) = Row_Pixels and Format (P) = Kind);
   -- Full-width row slices for initialization larger than staging. Row_Pixels
   -- optionally preserves producer padding; zero selects tightly packed rows.
   -- Every row including its padding fits the staging capacity, allowing a
   -- rasterizer to write directly without repacking or copying CPU pixels.
   -- Completion/publication policy must track the confirmed row prefix; these
   -- bounds alone do not authorize sampling a cold, partially uploaded image.
   procedure Row_Chunk (Width, Height, First_Row : Edge; Buffer_Size : Byte_Count;
      Kind : Pixel_Format; P : out Plan; Accepted : out Boolean;
      Row_Pixels : Edge := 0)
     with Post => Accepted = Valid (P) and (if Accepted then
       Area (P).X = 0 and Area (P).Y = First_Row and Area (P).Width = Width and
       Area (P).Height > 0 and Area (P).Y + Area (P).Height <= Height and
       Image_Width (P) = Width and Image_Height (P) = Height and
       Capacity (P) = Buffer_Size and Buffer_Offset (P) = 0 and Row_Length (P) = Row_Pixels and Format (P) = Kind and
       Long_Long_Integer (Area (P).Height) *
         Long_Long_Integer (if Row_Pixels = 0 then Width else Row_Pixels) *
         Long_Long_Integer (Pixel_Bytes (Kind)) <= Long_Long_Integer (Buffer_Size));
private
   function Span (Region : Rectangle; Row_Pixels : Edge; Kind : Pixel_Format) return Long_Long_Integer is
     (if Region.Height = 0 then 0 else
       (Long_Long_Integer (Region.Height - 1) * Long_Long_Integer
        (if Row_Pixels = 0 then Region.Width else Row_Pixels) + Long_Long_Integer (Region.Width)) *
        Long_Long_Integer (Pixel_Bytes (Kind)));
   function Fits (Width, Height : Edge; Buffer_Size : Byte_Count; Region : Rectangle;
      Offset : Byte_Count; Row_Pixels : Edge; Kind : Pixel_Format) return Boolean is
     (Width > 0 and Height > 0 and Region.Width > 0 and Region.Height > 0 and
      Region.X + Region.Width <= Width and Region.Y + Region.Height <= Height and
      (Row_Pixels = 0 or Row_Pixels >= Region.Width) and Offset mod 4 = 0 and
      Long_Long_Integer (Offset) + Span (Region, Row_Pixels, Kind) <= Long_Long_Integer (Buffer_Size));
   type Plan is record
      Ready : Boolean := False;
      Width, Height : Edge := 0;
      Region : Rectangle;
      Offset, Bytes : Byte_Count := 0;
      Row_Pixels : Edge := 0;
      Kind : Pixel_Format := BGRA8;
   end record;
   function Valid (P : Plan) return Boolean is
     (P.Ready and Fits (P.Width, P.Height, P.Bytes, P.Region, P.Offset, P.Row_Pixels, P.Kind));
   function Area (P : Plan) return Rectangle is (P.Region);
   function Image_Width (P : Plan) return Edge is (P.Width);
   function Image_Height (P : Plan) return Edge is (P.Height);
   function Buffer_Offset (P : Plan) return Byte_Count is (P.Offset);
   function Row_Length (P : Plan) return Edge is (P.Row_Pixels);
   function Format (P : Plan) return Pixel_Format is (P.Kind);
   function Capacity (P : Plan) return Byte_Count is (P.Bytes);
end Compositor_Upload;
