with System;
with CuBit.Appearance;
with CuBit.QOI;
with Desktop_Backdrop_Style;
--  The decoded wallpaper rasters (docs/assets.md): one fixed buffer per
--  image backdrop, sized by Desktop_Backdrop_Style, filled once from its
--  asset file by the streaming QOI decoder and then read-only. An image that
--  is missing, malformed or the wrong size never becomes Ready; it is shown
--  as the flat theme colour instead (Shown). No I/O here: the loader
--  (Desktop_Wallpaper_Assets) feeds the bytes it read.
package Desktop_Wallpaper_Store with SPARK_Mode, Abstract_State => State,
  Initializes => State
is
   package A renames CuBit.Appearance;
   package Q renames CuBit.QOI;
   package P renames Desktop_Backdrop_Style;
   use type A.Background;

   --  The backdrops with an image: exactly those P.Has_Image names.
   subtype Image is A.Background with Static_Predicate => Image in A.Wallpaper | A.Cubie;

   type Status is (Not_Loaded, Loading, Loaded, Unavailable);
   function Current (Asset : Image) return Status with Global => State;
   function Ready (Asset : A.Background) return Boolean is
     (Asset in Image and then Current (Asset) = Loaded) with Global => State;
   --  Only one asset loads at a time.
   function Busy return Boolean with Global => State;

   --  What to draw for Style: the style itself, or, when its image is not
   --  Ready, the flat theme colour (Slate) with the same scheme and placement.
   function Shown (Style : A.Preferences) return A.Preferences is
     (if P.Has_Image (Style.Backdrop) and then not Ready (Style.Backdrop)
      then (Style with delta Backdrop => A.Slate) else Style)
     with Global => State;

   --  The raster's first pixel (16#AARRGGBB#, rows of P.Width (Asset)
   --  pixels): read-only to callers, valid for the rest of the process.
   function Pixels (Asset : Image) return System.Address
     with Global => State, Pre => Ready (Asset);

   type Load_Result is record
      Loaded : Boolean := False;
      Error : Q.Failure := Q.No_Failure;   --  the decoder's, if it failed
      Wrong_Size : Boolean := False;       --  a valid image of another size
      Width, Height : Natural := 0;        --  as the file declared them
   end record;

   procedure Begin_Load (Asset : Image)
     with Global => (In_Out => State),
          Pre  => not Busy and then Current (Asset) = Not_Loaded,
          Post => Busy and then Current (Asset) = Loading;
   --  More of the file. Wants_More turns False once the stream is rejected.
   procedure Feed (Asset : Image; Input : Q.Byte_Array)
     with Global => (In_Out => State),
          Pre  => Busy and then Current (Asset) = Loading,
          Post => Busy and then Current (Asset) = Loading;
   function Wants_More (Asset : Image) return Boolean
     with Global => State, Pre => Busy and then Current (Asset) = Loading;
   --  The file has ended (or reading it failed: Read_Failed). Loaded only
   --  if exactly one complete image of the declared size was decoded.
   procedure End_Load (Asset : Image; Read_Failed : Boolean; Result : out Load_Result)
     with Global => (In_Out => State),
          Pre  => Busy and then Current (Asset) = Loading,
          Post => not Busy and then
                  Current (Asset) = (if Result.Loaded then Loaded else Unavailable);
   --  Without reading at all (no asset root, no filesystem): Unavailable.
   procedure Give_Up (Asset : Image)
     with Global => (In_Out => State),
          Pre  => not Busy and then Current (Asset) = Not_Loaded,
          Post => not Busy and then Current (Asset) = Unavailable;
end Desktop_Wallpaper_Store;
