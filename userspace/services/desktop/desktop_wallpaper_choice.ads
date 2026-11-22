with CuBit.Appearance;
with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Catalog;
--  Which catalog wallpaper an appearance asks for, and what is drawn for it
--  (no I/O, no logging, so hosted tests drive it with any catalog and any
--  decoded images). A name the catalog does not hold, such as a picture
--  since removed from the package, means the catalog's default wallpaper;
--  a wallpaper whose file did not decode shows the default instead, and
--  without that the flat colour: never a crash or a blank desktop. The
--  caller (Desktop_Wallpaper_Assets) loads the files and says so in the log.
package Desktop_Wallpaper_Choice is
   package A renames CuBit.Appearance;
   package Catalog renames Desktop_Wallpaper_Catalog;
   use type A.Backdrop_Kind;

   type Outcome is
     (Kept,               --  a solid colour, or a wallpaper the catalog holds
      Defaulted,          --  no name: the catalog's default
      Unknown_Wallpaper,  --  a name the catalog does not hold: its default instead
      No_Catalog);        --  an image was asked for but there is no catalog

   --  The wallpaper Requested shows (No_Wallpaper: a solid colour, or no
   --  catalog), and why.
   procedure Choose
     (Requested : A.Preferences; Index : out Catalog.Wallpaper_Count; Result : out Outcome)
     with Post => Index <= Catalog.Count and then
                  (if Requested.Kind = A.Solid then Index = Catalog.No_Wallpaper and then Result = Kept);

   --  Requested with its wallpaper settled against the catalog: an unknown
   --  or missing name becomes the default's, so Settings shows (and Apply
   --  stores) what the desktop draws. Unchanged without a catalog.
   procedure Settle (Requested : in out A.Preferences; Result : out Outcome);

   --  The wallpaper drawn for Chosen (Choose's): Chosen once its image is
   --  decoded (Desktop_Wallpaper_Store.Ready), else the default if that one
   --  is, else No_Wallpaper (the flat colour).
   function Shown_Index (Chosen : Catalog.Wallpaper_Count) return Catalog.Wallpaper_Count;
   --  What a frame draws for Requested showing wallpaper Index (Shown_Index's):
   --  the image at its decoded size, or the requested solid colour or the
   --  flat theme colour.
   function Style_For (Requested : A.Preferences; Index : Catalog.Wallpaper_Count)
     return Desktop_Backdrop_Style.Backdrop;
end Desktop_Wallpaper_Choice;
