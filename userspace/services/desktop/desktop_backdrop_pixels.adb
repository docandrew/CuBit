with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Store;
with System.Storage_Elements;
package body Desktop_Backdrop_Pixels with SPARK_Mode => Off is
   procedure Copy_Chunk
     (Asset : CuBit.Appearance.Background; Mapping : System.Address;
      Bytes : Compositor_Upload.Byte_Count; Plan : Compositor_Upload.Plan;
      Complete : out Boolean)
   is
      package U renames Compositor_Upload;
      package P renames Desktop_Backdrop_Style;
      use System.Storage_Elements;
      package Store renames Desktop_Wallpaper_Store;
      use type System.Address, U.Pixel_Format;
      Pixel_Bytes : constant := 4;
      function Memcpy (Target, Source : System.Address; Bytes : Storage_Count) return System.Address
        with Import, Convention => C, External_Name => "memcpy";
      A : U.Rectangle;
      Stride, Offset : Natural;
      Source, Ignore : System.Address;
   begin
      Complete := False;
      if Mapping = System.Null_Address or else not P.Has_Image (Asset) or else
        not Store.Ready (Asset) or else
        not U.Valid (Plan) or else U.Format (Plan) /= U.BGRA8 or else
        U.Image_Width (Plan) /= P.Width (Asset) or else
        U.Image_Height (Plan) /= P.Height (Asset) or else U.Capacity (Plan) > Bytes
      then return; end if;
      A := U.Area (Plan);
      Stride := (if U.Row_Length (Plan) = 0 then A.Width else U.Row_Length (Plan));
      -- Valid bounds every row within the provided mapping and the source.
      for Row in 0 .. A.Height - 1 loop
         Offset := (A.Y + Row) * P.Width (Asset) + A.X;
         Source := Store.Pixels (Asset) + Storage_Offset (Offset * Pixel_Bytes);
         Ignore := Memcpy
           (Mapping + Storage_Offset (U.Buffer_Offset (Plan) + Row * Stride * Pixel_Bytes),
            Source, Storage_Count (A.Width * Pixel_Bytes));
      end loop;
      Complete := True;
   end Copy_Chunk;
end Desktop_Backdrop_Pixels;
