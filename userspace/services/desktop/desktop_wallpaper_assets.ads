with CuBit.Appearance;
with Desktop_Backdrop_Style;
--  Desktop's wallpaper assets (docs/assets.md): the read-only package
--  Assets/cubit-wallpapers/<version>/ under the asset root that the system
--  configuration names (system.assets.root, e.g. "@nvme:0/Assets/"). Its
--  catalog.ccl (Desktop_Wallpaper_Catalog) says which wallpapers there are;
--  each is a QOI file of any size, read once (Desktop_Asset_Files) with an
--  explicit deadline, decoded into Desktop_Wallpaper_Store and kept for the
--  rest of the session. A missing, unreadable or malformed catalog or file,
--  or an image past the store's decoded-memory ceiling (a CAPACITY line
--  with limit and demand), is logged as a warning and not retried: the
--  backdrop is then the default wallpaper, or the flat theme colour.
--
--  The package is the one catalog source today. A user wallpaper directory
--  would be read the same way (Load_Catalog over a second root) and its
--  entries appended to the catalog after the package's.
package Desktop_Wallpaper_Assets is
   --  The wallpaper package Desktop was built against (its exact version,
   --  like any other dependency); the root is Desktop_Asset_Files'.
   Package_Path : constant String := "cubit-wallpapers/2/";
   Catalog_File : constant String := "catalog.ccl";

   --  Read the package's catalog and size the store to it (once, at
   --  startup, before the appearance is read).
   procedure Load_Catalog;

   --  Settle a stored appearance against the catalog
   --  (Desktop_Wallpaper_Choice.Settle), warning when its wallpaper is not
   --  in the catalog.
   procedure Settle (Requested : in out CuBit.Appearance.Preferences);

   --  Load Requested's image now if it is one that has not been tried.
   procedure Prepare (Requested : CuBit.Appearance.Preferences);
   --  Prepare Requested's image, then say what to draw: its wallpaper, the
   --  default wallpaper when that one is unavailable, or the flat colour.
   procedure Resolve (Requested : CuBit.Appearance.Preferences;
                      Shown : out Desktop_Backdrop_Style.Backdrop);
end Desktop_Wallpaper_Assets;
