with CuBit.Appearance;
with Compositor_Image_Sampling;
with Interfaces;
-- Shared immutable-asset metadata and appearance policy for software and GPU
-- wallpaper rendering. Contains neither pixels nor authority to import them.
package Desktop_Backdrop_Style with SPARK_Mode, Pure is
   package A renames CuBit.Appearance;
   package S renames Compositor_Image_Sampling;
   use type A.Background, A.Color_Scheme;
   Wallpaper_Width : constant := 2048;
   Wallpaper_Height : constant := 576;
   Cubie_Width : constant := 2048;
   Cubie_Height : constant := 1152;
   function Has_Image (Value : A.Background) return Boolean is
     (Value in A.Wallpaper | A.Cubie);
   function Width (Value : A.Background) return S.Extent is
     (if Value = A.Cubie then Cubie_Width else Wallpaper_Width);
   function Height (Value : A.Background) return S.Extent is
     (if Value = A.Cubie then Cubie_Height else Wallpaper_Height);
   function Color (Style : A.Preferences) return Interfaces.Unsigned_32 is
     (if Style.Backdrop = A.Ocean then 16#FF20_4058#
      elsif Style.Scheme = A.Alloy_Dark then 16#FF20_282E# else 16#FF54_5D63#);
   function Placement (Value : A.Placement) return S.Placement is
     (case Value is when A.Fill => S.Fill, when A.Fit => S.Fit,
      when A.Center => S.Center);
end Desktop_Backdrop_Style;
