with Interfaces;
package Wallpaper_Assets is
   type Pixels is array (Natural range <>) of Interfaces.Unsigned_32 with Convention => C;
   Wallpaper : Pixels (0 .. 2048 * 576 - 1) := (others => 0)
     with Export, Convention => C, External_Name => "cubit_desktop_wallpaper";
   Cubie : Pixels (0 .. 2048 * 1152 - 1) := (others => 0)
     with Export, Convention => C, External_Name => "cubit_desktop_wallpaper_cubie";
end Wallpaper_Assets;
