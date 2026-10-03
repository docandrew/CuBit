with Desktop_Backdrop_Style;
with Interfaces;
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
      use type System.Address, U.Pixel_Format, CuBit.Appearance.Background;
      type Pixels is array (Natural range <>) of Interfaces.Unsigned_32 with Convention => C;
      Wallpaper : constant Pixels (0 .. P.Wallpaper_Width * P.Wallpaper_Height - 1)
        with Import, Convention => C, External_Name => "cubit_desktop_wallpaper";
      Cubie : constant Pixels (0 .. P.Cubie_Width * P.Cubie_Height - 1)
        with Import, Convention => C, External_Name => "cubit_desktop_wallpaper_cubie";
      function Memcpy (Target, Source : System.Address; Bytes : Storage_Count) return System.Address
        with Import, Convention => C, External_Name => "memcpy";
      A : U.Rectangle;
      Stride, Offset : Natural;
      Source, Ignore : System.Address;
   begin
      Complete := False;
      if Mapping = System.Null_Address or else not P.Has_Image (Asset) or else
        not U.Valid (Plan) or else U.Format (Plan) /= U.BGRA8 or else
        U.Image_Width (Plan) /= P.Width (Asset) or else
        U.Image_Height (Plan) /= P.Height (Asset) or else U.Capacity (Plan) > Bytes
      then return; end if;
      A := U.Area (Plan);
      Stride := (if U.Row_Length (Plan) = 0 then A.Width else U.Row_Length (Plan));
      -- Valid bounds every row within the provided mapping and the source.
      for Row in 0 .. A.Height - 1 loop
         Offset := (A.Y + Row) * P.Width (Asset) + A.X;
         Source := (if Asset = CuBit.Appearance.Cubie then Cubie (Offset)'Address
                    else Wallpaper (Offset)'Address);
         Ignore := Memcpy
           (Mapping + Storage_Offset (U.Buffer_Offset (Plan) + Row * Stride * 4),
            Source, Storage_Count (A.Width * 4));
      end loop;
      Complete := True;
   end Copy_Chunk;
end Desktop_Backdrop_Pixels;
