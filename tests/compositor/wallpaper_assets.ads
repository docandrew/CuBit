with Interfaces;
with CuBit.Appearance;
with Wallpaper_Loader;
pragma Elaborate_All (Wallpaper_Loader);
--  Hosted tests: the decoded wallpaper rasters, Ready and writable here.
package Wallpaper_Assets is
   type Pixels is array (Natural range <>) of Interfaces.Unsigned_32 with Convention => C;
   Wallpaper : Pixels (0 .. 2048 * 576 - 1)
     with Import, Address => Wallpaper_Loader.Load (CuBit.Appearance.Wallpaper);
   Cubie : Pixels (0 .. 2048 * 1152 - 1)
     with Import, Address => Wallpaper_Loader.Load (CuBit.Appearance.Cubie);
end Wallpaper_Assets;
