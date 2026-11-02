with System;
--  Hosted tests only: size Desktop_Wallpaper_Store for the test catalog
--  (Wallpaper_Assets.Images) on first use, make wallpaper Asset Ready at
--  Width x Height by decoding a generated all-zero QOI image of that size,
--  and return its raster (CuAlloc) for the test to fill (Wallpaper_Assets).
package Wallpaper_Loader is
   Test_Images : constant := 3;
   function Load (Asset : Positive; Width, Height : Positive) return System.Address
     with Pre => Asset <= Test_Images;
end Wallpaper_Loader;
