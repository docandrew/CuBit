with CuBit.Appearance;
--  Desktop's wallpaper assets (docs/assets.md): each image backdrop is a QOI
--  file in the read-only package Assets/cubit-wallpapers/<version>/ under
--  the asset root that the system configuration names (system.assets.root,
--  e.g. "@nvme:0/Assets/"). A file is read once, through the filesystem's
--  request queue with an explicit deadline, decoded into
--  Desktop_Wallpaper_Store and kept for the rest of the session. A missing,
--  unreadable or malformed file is logged as a warning and the backdrop is
--  drawn as the flat theme colour; it is not retried.
package Desktop_Wallpaper_Assets is
   --  The asset root setting and the wallpaper package Desktop was built
   --  against (its exact version, like any other dependency).
   Root_Setting : constant String := "system.assets.root";
   Package_Path : constant String := "cubit-wallpapers/1/";

   --  Load Backdrop's image now if it is one that has not been tried.
   procedure Prepare (Backdrop : CuBit.Appearance.Background);
   --  Prepare Requested's image, then say what to draw: Requested, or its
   --  flat theme colour when the image is unavailable.
   procedure Resolve (Requested : CuBit.Appearance.Preferences;
                      Shown : out CuBit.Appearance.Preferences);
end Desktop_Wallpaper_Assets;
