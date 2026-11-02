with CuBit.Appearance;
with CuBit.QOI;
with Compositor_Image_Sampling;
with Interfaces;
-- Shared appearance policy for software and GPU wallpaper rendering: what
-- one frame draws once the appearance has been resolved against the
-- wallpaper catalog (Desktop_Wallpaper_Assets.Resolve). Contains neither
-- pixels nor authority to import them; an image is a wallpaper's catalog
-- index and the size its QOI header declared, never a compiled-in picture.
package Desktop_Backdrop_Style with SPARK_Mode, Pure is
   package A renames CuBit.Appearance;
   package S renames Compositor_Image_Sampling;
   use type A.Color_Scheme, A.Solid_Color;

   --  A wallpaper of the catalog (Desktop_Wallpaper_Catalog.Wallpaper_Index),
   --  or none: a solid colour.
   subtype Image_Index is Natural;
   No_Image : constant Image_Index := 0;

   type Backdrop is record
      Scheme : A.Color_Scheme := A.Alloy_Light;
      --  A decoded (Ready) wallpaper, or No_Image.
      Image : Image_Index := No_Image;
      --  The image's size, from its QOI header (Desktop_Wallpaper_Store).
      Width, Height : S.Extent := 1;
      Position : A.Placement := A.Fill;
      --  Without an image: which solid colour.
      Color : A.Solid_Color := A.Slate;
   end record;
   --  The widest row a wallpaper can have (its QOI header's limit): the GPU
   --  path's upload staging must hold one.
   Widest_Row_Pixels : constant := CuBit.QOI.Maximum_Side;
   --  The flat theme colour: what a backdrop is while it has no image.
   Flat : constant Backdrop := (others => <>);

   Ocean_Color : constant := 16#FF20_4058#;
   Slate_Dark_Color : constant := 16#FF20_282E#;
   Slate_Light_Color : constant := 16#FF54_5D63#;

   function Has_Image (Style : Backdrop) return Boolean is (Style.Image /= No_Image);
   --  The fill under (and around a Fit or Center) image, or the whole
   --  backdrop without one.
   function Color (Style : Backdrop) return Interfaces.Unsigned_32 is
     (if not Has_Image (Style) and then Style.Color = A.Ocean then Ocean_Color
      elsif Style.Scheme = A.Alloy_Dark then Slate_Dark_Color else Slate_Light_Color);
   function Placement (Value : A.Placement) return S.Placement is
     (case Value is when A.Fill => S.Fill, when A.Fit => S.Fit,
      when A.Center => S.Center);
end Desktop_Backdrop_Style;
