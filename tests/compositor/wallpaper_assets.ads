with Interfaces;
with CuBit.Appearance;
with Desktop_Backdrop_Style;
with Wallpaper_Loader;
pragma Elaborate_All (Wallpaper_Loader);
--  Hosted tests: three decoded wallpapers of different sizes, Ready and
--  writable here (the store has no fixed slots: any catalog entry, any
--  size), and the backdrops the tests draw: each image, and the two solid
--  colours.
package Wallpaper_Assets is
   type Pixels is array (Natural range <>) of Interfaces.Unsigned_32 with Convention => C;
   type Test_Backdrop is (Wide, Tall, Third, Slate, Ocean);
   subtype Test_Image is Test_Backdrop range Wide .. Third;
   Widths : constant array (Test_Image) of Positive := [Wide => 2048, Tall => 2048, Third => 1000];
   Heights : constant array (Test_Image) of Positive := [Wide => 576, Tall => 1152, Third => 750];
   --  Each image's catalog index.
   function Index (Item : Test_Image) return Positive is (Test_Image'Pos (Item) + 1);

   Wallpaper : Pixels (0 .. Widths (Wide) * Heights (Wide) - 1)
     with Import, Address => Wallpaper_Loader.Load (Index (Wide), Widths (Wide), Heights (Wide));
   Tall_Image : Pixels (0 .. Widths (Tall) * Heights (Tall) - 1)
     with Import, Address => Wallpaper_Loader.Load (Index (Tall), Widths (Tall), Heights (Tall));
   Third_Image : Pixels (0 .. Widths (Third) * Heights (Third) - 1)
     with Import, Address => Wallpaper_Loader.Load (Index (Third), Widths (Third), Heights (Third));

   --  What a frame draws for Item: as Desktop_Wallpaper_Choice.Style_For
   --  resolves it.
   function Style (Item : Test_Backdrop; Scheme : CuBit.Appearance.Color_Scheme;
                   Position : CuBit.Appearance.Placement) return Desktop_Backdrop_Style.Backdrop is
     (case Item is
        when Test_Image =>
          (Scheme => Scheme, Image => Index (Item), Width => Widths (Item), Height => Heights (Item),
           Position => Position, Color => CuBit.Appearance.Slate),
        when Slate =>
          (Desktop_Backdrop_Style.Flat with delta Scheme => Scheme, Position => Position),
        when Ocean =>
          (Desktop_Backdrop_Style.Flat with delta Scheme => Scheme, Position => Position,
                                                   Color => CuBit.Appearance.Ocean));
end Wallpaper_Assets;
