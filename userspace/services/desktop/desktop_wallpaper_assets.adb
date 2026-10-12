with Interfaces; use Interfaces;
with CuBit.QOI;
with Desktop_Asset_Files;
with Desktop_Backdrop_Style;
with Desktop_Logs;
with Desktop_Wallpaper_Store;

package body Desktop_Wallpaper_Assets is
   package Store renames Desktop_Wallpaper_Store;
   package Files renames Desktop_Asset_Files;
   package Q renames CuBit.QOI;
   use type CuBit.Appearance.Background;
   use type Store.Status;
   use type Files.Read_Status;

   LF : constant Character := Character'Val (10);

   --  The whole load, from opening the queue to closing it, ends by this
   --  deadline; startup never waits longer on the filesystem for a
   --  wallpaper.
   Load_Budget_Ms : constant := 2_000;
   Extension    : constant String := ".qoi";
   Flat_Colour  : constant String := "; using the flat theme colour";

   --  Each image's file in the package.
   function File_Name (Asset : Store.Image) return String is
     ((case Asset is
         when CuBit.Appearance.Wallpaper => "cubes",
         when CuBit.Appearance.Cubie => "cubie") & Extension);

   --  The worst QOI stream for Asset's raster: a five-byte Op_RGBA chunk per
   --  pixel. Longer files are rejected without reading them all.
   function Maximum_Bytes (Asset : Store.Image) return Unsigned_64 is
     (Q.Header_Bytes + Q.Marker_Bytes + 5 * Unsigned_64
        (Desktop_Backdrop_Style.Width (Asset)) *
        Unsigned_64 (Desktop_Backdrop_Style.Height (Asset)));

   --  Readable reasons: the runtime has no enumeration images.
   function Reason (Error : Q.Failure) return String is
     (case Error is
        when Q.No_Failure => "no image",
        when Q.Bad_Magic => "not a QOI file",
        when Q.Bad_Size => "invalid size",
        when Q.Over_Limit => "larger than the raster",
        when Q.Bad_Channels => "invalid channel count",
        when Q.Bad_Colorspace => "invalid colour space",
        when Q.Run_Past_End => "run past the last pixel",
        when Q.Bad_Marker => "bad end marker",
        when Q.Trailing_Data => "data after the end marker",
        when Q.Truncated => "truncated");

   procedure Load (Asset : Store.Image) is
      function Wants_More return Boolean is (Store.Wants_More (Asset));
      procedure Feed (Input : Q.Byte_Array) is
      begin
         Store.Feed (Asset, Input);
      end Feed;
      procedure Read is new Files.Read (Feed => Feed, Wants_More => Wants_More);
      Result : Files.Read_Result;
      Path : String (1 .. Files.Maximum_Path);
      Length : Natural;
      Decoded : Store.Load_Result;
   begin
      Store.Begin_Load (Asset);
      Read (Package_Path & File_Name (Asset), Maximum_Bytes (Asset), Load_Budget_Ms,
            Result, Path, Length);
      Store.End_Load (Asset, not Files.Read_Whole (Result), Decoded);
      declare
         Shown : constant String := Path (1 .. Length);
      begin
         if Decoded.Loaded then
            Desktop_Logs.Write ("desktop: wallpaper loaded " & Shown & " bytes=" &
              Result.Bytes'Image & " ms=" & Result.Elapsed_Ms'Image & LF);
         elsif Result.Status = Files.No_Asset_Root then
            Desktop_Logs.Warn ("desktop: wallpaper unavailable: " & Files.Describe (Result) &
                               Flat_Colour);
         elsif not Files.Read_Whole (Result) then
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " unavailable: " &
                               Files.Describe (Result) & Flat_Colour);
         elsif Decoded.Wrong_Size then
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " rejected: size" &
                  Decoded.Width'Image & " x" & Decoded.Height'Image & " is not the expected" &
                  Desktop_Backdrop_Style.Width (Asset)'Image & " x" &
                  Desktop_Backdrop_Style.Height (Asset)'Image & Flat_Colour);
         else
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " rejected: " &
                               Reason (Decoded.Error) & Flat_Colour);
         end if;
      end;
   end Load;

   procedure Prepare (Backdrop : CuBit.Appearance.Background) is
   begin
      if Backdrop in Store.Image and then not Store.Busy and then
        Store.Current (Backdrop) = Store.Not_Loaded
      then
         Load (Backdrop);
      end if;
   end Prepare;

   procedure Resolve (Requested : CuBit.Appearance.Preferences;
                      Shown : out CuBit.Appearance.Preferences) is
   begin
      Prepare (Requested.Backdrop);
      Shown := Store.Shown (Requested);
   end Resolve;
end Desktop_Wallpaper_Assets;
