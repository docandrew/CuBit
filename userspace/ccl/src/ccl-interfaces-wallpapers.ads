--  The wallpaper package's catalog as CCL types (Assets/cubit-wallpapers/
--  <version>/catalog.ccl). A wallpaper is declared once, in the build's
--  source list (assets/wallpapers/wallpapers.ccl, one Wallpaper_Sources):
--  ccl-manifest --wallpaper-catalog checks it against these declarations
--  with the CCL analyser and writes the package's catalog, one Wallpaper per
--  line in its canonical source (CCL.Typed_Values renders it), and
--  tools/build_wallpaper_assets.py writes each image's QOI file. The desktop
--  reads the catalog with the decoder generated from Wallpaper
--  (CCL.Asset_Codecs.Wallpaper, ccl-manifest --asset-codecs) and takes each
--  image's size from its QOI header, so neither a picture nor a count of
--  pictures is compiled into the desktop.
--
--  name is the wallpaper's stable identity (what the desktop's appearance
--  setting stores); label is what Settings shows; file is the QOI image in
--  the package; attribution says where the picture came from and on what
--  terms it ships. The first entry is the package's default wallpaper.
package CCL.Interfaces.Wallpapers with SPARK_Mode, Pure is
   --  One published catalog entry.
   TYPE_SOURCE : constant String :=
     "(type Wallpaper (record (name String) (label String) (file String) (attribution String)))";
   ROOT_TYPE_NAME : constant String := "Wallpaper";

   --  A name: 1 .. MAXIMUM_NAME of a-z, 0-9 and single '-', starting with a
   --  letter. The image file is <name>.qoi, so a name is also a safe file
   --  name in the package. The ceiling is the appearance setting's
   --  (CuBit.Appearance.Maximum_Image_Name): an identifier, not content.
   MAXIMUM_NAME : constant := 64;
   --  A label Settings can show on one row.
   MAXIMUM_LABEL : constant := 48;
   FILE_EXTENSION : constant String := ".qoi";

   --  The build's source list: each picture's source image (relative to
   --  assets/wallpapers/) and the raster the build reduces it to, which
   --  bounds the decoded image's memory.
   SOURCES_TYPE_SOURCE : constant String :=
     "(type Raster_Extent (range 1 16384)) " &
     "(type Wallpaper_Source (record (name String) (label String) (source String) " &
     "(width Raster_Extent) (height Raster_Extent) (attribution String))) " &
     "(type Wallpaper_Sources (record (images (List Wallpaper_Source))))";
   SOURCES_ROOT_TYPE_NAME : constant String := "Wallpaper_Sources";
end CCL.Interfaces.Wallpapers;
