--  Versioned appearance preferences. Pure data, not authority to change them.
--  The backdrop is a solid colour or a wallpaper named by its catalog name
--  (the wallpaper package's catalog, CCL.Interfaces.Wallpapers): no picture
--  is a literal here. The desktop stores them as one typed Config value
--  (desktop.appearance.v2, CCL.Interfaces.Desktop_Appearance).
package CuBit.Appearance with SPARK_Mode, Pure is
   type Color_Scheme is (Alloy_Light, Alloy_Dark);
   type Backdrop_Kind is (Image, Solid);
   type Solid_Color is (Slate, Ocean);
   type Placement is (Fill, Fit, Center);

   --  A wallpaper's catalog name (CCL.Interfaces.Wallpapers.MAXIMUM_NAME):
   --  an identifier, so a named ceiling rather than content-sized.
   Maximum_Image_Name : constant := 64;
   subtype Image_Name_Length is Natural range 0 .. Maximum_Image_Name;
   --  Unused characters are spaces, so "=" compares names.
   type Image_Name is record
      Length : Image_Name_Length := 0;
      Text : String (1 .. Maximum_Image_Name) := [others => ' '];
   end record;
   --  No name: the catalog's default wallpaper.
   Default_Image : constant Image_Name := (others => <>);
   function Name (Item : Image_Name) return String is (Item.Text (1 .. Item.Length));
   --  The catalog's spelling of a name: 1 .. Maximum_Image_Name of a-z, 0-9
   --  and single '-', starting with a letter and not ending with '-'.
   function Valid_Image_Name (Text : String) return Boolean is
     (Text'Length in 1 .. Maximum_Image_Name
      and then Text (Text'First) in 'a' .. 'z'
      and then Text (Text'Last) /= '-'
      and then (for all C of Text => C in 'a' .. 'z' | '0' .. '9' | '-')
      and then (for all I in Text'First .. Text'Last - 1 =>
                  not (Text (I) = '-' and then Text (I + 1) = '-')));
   function To_Image_Name (Text : String) return Image_Name
     with Pre => Text'Length <= Maximum_Image_Name,
          Post => Name (To_Image_Name'Result) = Text;

   type Preferences is record
      Scheme : Color_Scheme := Alloy_Light;
      Kind : Backdrop_Kind := Image;
      --  Kind = Image: which wallpaper, and how it is placed.
      Image : Image_Name := Default_Image;
      Position : Placement := Fill;
      --  Kind = Solid: which colour. Kept while an image is shown, so
      --  switching back and forth in Settings keeps both choices.
      Color : Solid_Color := Slate;
   end record;
   Default : constant Preferences := (others => <>);
end CuBit.Appearance;
