with System;
with CuBit.QOI;
--  The decoded wallpaper rasters (docs/assets.md), one per wallpaper of the
--  catalog (Desktop_Wallpaper_Catalog), by its index. An image is decoded
--  once from its asset file by the streaming QOI decoder and then
--  read-only. Its size is whatever its QOI header says: the header is read
--  first, the raster is then taken from CuAlloc at exactly that size
--  (Desktop_Wallpaper_Store.Rasters), and only then are the pixels decoded.
--  All rasters together stay within Decoded_Ceiling: an image that would
--  pass it is refused (Over_Ceiling) with the limit and its demand, and is
--  never decoded. An image that is missing, malformed, over the ceiling or
--  without memory never becomes Ready; the desktop shows its default
--  wallpaper or a flat colour instead. No I/O here: the loader
--  (Desktop_Wallpaper_Assets) feeds the bytes it read.
package Desktop_Wallpaper_Store with SPARK_Mode,
  Abstract_State => State,
  Initializes => State
is
   package Q renames CuBit.QOI;

   Pixel_Bytes : constant := 4;
   --  The decoded memory all wallpapers together may hold: two 4K rasters
   --  (a named ceiling on memory, not a count of pictures).
   Decoded_Ceiling : constant := 64 * 1024 * 1024;
   subtype Byte_Count is Natural range 0 .. Decoded_Ceiling;
   --  An image's demand can be larger than the ceiling (up to the largest
   --  sides a QOI header may declare), so it is counted in a wider type.
   type Demand_Bytes is range 0 .. Decoded_Ceiling + Q.Maximum_Side * Q.Maximum_Side * Pixel_Bytes;

   subtype Image_Count is Natural;
   subtype Image is Positive;

   --  The images the store has room to track: the catalog's count once
   --  Reserve has run, 0 before.
   function Capacity return Image_Count with Global => State;
   --  Track Count images (the catalog's), none loaded. Done is False when
   --  the heap has no room for the table.
   procedure Reserve (Count : Image_Count; Done : out Boolean)
     with Global => (In_Out => State),
          Pre  => Capacity = 0 and then not Busy,
          Post => not Busy and then
                  (if Done then Capacity = Count and then
                     (for all Asset in 1 .. Count => Current (Asset) = Not_Loaded)
                   else Capacity = 0);

   type Status is (Not_Loaded, Loading, Loaded, Unavailable);
   function Current (Asset : Image) return Status
     with Global => State, Pre => Asset <= Capacity;
   function Ready (Asset : Natural) return Boolean is
     (Asset in 1 .. Capacity and then Current (Asset) = Loaded)
     with Global => State;
   --  Only one asset loads at a time.
   function Busy return Boolean with Global => State;
   --  Decoded bytes held by every raster.
   function Used return Byte_Count with Global => State;

   --  A Ready image's size, from its QOI header.
   function Width (Asset : Image) return Q.Side
     with Global => State, Pre => Ready (Asset);
   function Height (Asset : Image) return Q.Side
     with Global => State, Pre => Ready (Asset);
   --  The raster's first pixel (16#AARRGGBB#, Height rows of Width
   --  pixels): read-only to callers, valid for the rest of the process.
   function Pixels (Asset : Image) return System.Address
     with Global => State, Pre => Ready (Asset);

   type Refusal is
     (Accepted,       --  decoded, or the decoder's own failure (Error)
      Over_Ceiling,   --  Limit / Demand: the ceiling and what it would take
      No_Memory);     --  the heap had no room for the raster
   type Load_Result is record
      Loaded : Boolean := False;
      Error : Q.Failure := Q.No_Failure;   --  the decoder's, if it failed
      Refused : Refusal := Accepted;
      Width, Height : Natural := 0;        --  as the file declared them
      Limit : Byte_Count := Decoded_Ceiling;
      Demand : Demand_Bytes := 0;          --  Used before it plus this image
   end record;

   procedure Begin_Load (Asset : Image)
     with Global => (In_Out => State),
          Pre  => not Busy and then Asset <= Capacity and then Current (Asset) = Not_Loaded,
          Post => Capacity = Capacity'Old and then Busy and then Current (Asset) = Loading;
   --  More of the file. Wants_More turns False once the stream is rejected.
   procedure Feed (Asset : Image; Input : Q.Byte_Array)
     with Global => (In_Out => State),
          Pre  => Busy and then Asset <= Capacity and then Current (Asset) = Loading,
          Post => Capacity = Capacity'Old and then Busy and then Current (Asset) = Loading;
   function Wants_More (Asset : Image) return Boolean
     with Global => State, Pre => Busy and then Asset <= Capacity and then Current (Asset) = Loading;
   --  The file has ended (or reading it failed: Read_Failed). Loaded only
   --  if exactly one complete image was decoded within the ceiling.
   procedure End_Load (Asset : Image; Read_Failed : Boolean; Result : out Load_Result)
     with Global => (In_Out => State),
          Pre  => Busy and then Asset <= Capacity and then Current (Asset) = Loading,
          Post => Capacity = Capacity'Old and then not Busy and then
                  Current (Asset) = (if Result.Loaded then Loaded else Unavailable);
   --  Without reading at all (no asset root, no filesystem): Unavailable.
   procedure Give_Up (Asset : Image)
     with Global => (In_Out => State),
          Pre  => not Busy and then Asset <= Capacity and then Current (Asset) = Not_Loaded,
          Post => Capacity = Capacity'Old and then not Busy and then Current (Asset) = Unavailable;
end Desktop_Wallpaper_Store;
