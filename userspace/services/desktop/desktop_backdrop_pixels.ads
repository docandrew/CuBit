with System;
with Compositor_Upload;
with Desktop_Backdrop_Style;
-- Narrow pointer boundary: caller supplies an exclusive writable staging
-- mapping covering Bytes, disjoint from the decoded wallpaper rasters
-- (Desktop_Wallpaper_Store). An image that is not Ready, or is not Width x
-- Height (the size its backing was allocated at), copies nothing.
-- The synchronous call never retains Mapping. Padding remains untouched.
package Desktop_Backdrop_Pixels with SPARK_Mode is
   procedure Copy_Chunk
     (Asset : Desktop_Backdrop_Style.Image_Index; Width, Height : Positive; Mapping : System.Address;
      Bytes : Compositor_Upload.Byte_Count; Plan : Compositor_Upload.Plan;
      Complete : out Boolean)
     with Global => null;
end Desktop_Backdrop_Pixels;
