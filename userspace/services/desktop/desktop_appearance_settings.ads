------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The desktop's appearance setting, read from and written to Config.
--
--  `desktop.appearance.v2` holds a typed CCL Appearance
--  (CCL.Interfaces.Desktop_Display.APPEARANCE_TYPE_SOURCE), stored as its canonical source:
--
--    (Appearance scheme => Color_Scheme.Alloy_Light backdrop =>
--      (Backdrop.Image (Wallpaper_Choice name => "cubes" placement => Placement.Fill)))
--    (Appearance scheme => Color_Scheme.Alloy_Dark backdrop => (Backdrop.Solid Solid_Color.Ocean))
--
--  (each on one line). Decode reads a stored value with the decoder
--  generated from its type (CCL.Setting_Codecs.Appearance); anything but
--  the canonical, schema-valid form, or a wallpaper name the catalog could
--  never hold (CuBit.Appearance.Valid_Image_Name), is refused and the desktop
--  keeps CuBit.Appearance.Default. A well-formed name the catalog does not
--  hold is decoded as it is: the desktop falls back to its default
--  wallpaper and says so (Desktop_Wallpaper_Assets). Encode writes the
--  canonical form (tests/wallpaper-catalog checks it is what the CCL checker
--  produces).
------------------------------------------------------------------------------
with CuBit.Appearance;

package Desktop_Appearance_Settings is
   procedure Decode (Source : String; Item : out CuBit.Appearance.Preferences; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is CuBit.Appearance.Preferences;
   function Encode (Item : CuBit.Appearance.Preferences) return String;
end Desktop_Appearance_Settings;
