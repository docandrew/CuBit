with System;
with CuBit.Appearance;
--  Hosted tests only: make an image backdrop Ready in Desktop_Wallpaper_Store
--  by decoding a generated all-zero QOI image of its size, and return its
--  raster for the test to fill (Wallpaper_Assets).
package Wallpaper_Loader is
   function Load (Asset : CuBit.Appearance.Background) return System.Address
     with Pre => Asset in CuBit.Appearance.Wallpaper | CuBit.Appearance.Cubie;
end Wallpaper_Loader;
