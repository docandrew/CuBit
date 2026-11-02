------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Asset package catalogs, built on the host (ccl-manifest
--  --wallpaper-catalog and --asset-codecs).
--
--  @description
--  A wallpaper is declared once: in the build's source list
--  (assets/wallpapers/wallpapers.ccl), one CCL expression of type
--  Wallpaper_Sources (CCL.Interfaces.Wallpapers.SOURCES_TYPE_SOURCE),
--  checked by the type system and read by field name, never by a keyword
--  reader. Build_Wallpapers checks it and writes the package's catalog: one
--  Wallpaper per line, each the canonical source CCL.Typed_Values renders
--  for it (so the line is the type system's spelling, not this tool's), in
--  the source list's order (the first is the default). It returns the
--  raster plan tools/build_wallpaper_assets.py turns into QOI files.
--
--  Emit_Codecs writes CCL.Asset_Codecs.<Root>, the decoder a program reads
--  each catalog line with, generated from the entry type by Type_Codecs
--  exactly as the typed-setting decoders are.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Strings.Unbounded;
with CuBit.Failures;

package Asset_Catalogs is
   use Ada.Strings.Unbounded;

   --  Check Source (the text of Source_Path's file; Source_Directory holds
   --  the pictures it names) and write the catalog to Catalog_Path. Plan is
   --  one line per wallpaper: name, source path, raster width and height,
   --  separated by tabs (a build-tool interface, not CCL).
   procedure Build_Wallpapers
     (Source, Source_Path, Source_Directory, Catalog_Path : String;
      Plan : out Unbounded_String; Success : out Boolean; Why : out CuBit.Failures.Failure);

   --  Write every asset catalog's entry decoder into Directory.
   procedure Emit_Codecs
     (Directory : String; Success : out Boolean; Why : out CuBit.Failures.Failure);
end Asset_Catalogs;
