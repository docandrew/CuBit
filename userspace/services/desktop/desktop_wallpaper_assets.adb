with Interfaces; use Interfaces;
with CuBit.QOI;
with Desktop_Asset_Files;
with Desktop_Logs;
with Desktop_Wallpaper_Catalog;
with Desktop_Wallpaper_Choice;
with Desktop_Wallpaper_Store;

package body Desktop_Wallpaper_Assets is
   package Store renames Desktop_Wallpaper_Store;
   package Catalog renames Desktop_Wallpaper_Catalog;
   package Choice renames Desktop_Wallpaper_Choice;
   package Files renames Desktop_Asset_Files;
   package Q renames CuBit.QOI;
   package A renames CuBit.Appearance;
   use type Store.Status;
   use type Store.Refusal;
   use type Files.Read_Status;
   use type Catalog.Load_Status;
   use type Choice.Outcome;
   use type A.Backdrop_Kind;
   use type A.Image_Name;

   LF : constant Character := Character'Val (10);

   --  The whole load, from opening the queue to closing it, ends by this
   --  deadline; startup never waits longer on the filesystem for a
   --  wallpaper (or for the catalog).
   Load_Budget_Ms : constant := 2_000;
   Unavailable_Image : constant String := "; using the default wallpaper or the flat theme colour";
   Unavailable_Catalog : constant String := "; offering only the solid colours";

   --  The worst QOI stream the store could accept: a five-byte Op_RGBA
   --  chunk per pixel of the decoded-memory ceiling. Longer files are
   --  rejected without reading them all.
   Op_RGBA_Bytes : constant := 5;
   Maximum_Image_Bytes : constant Unsigned_64 :=
     Q.Header_Bytes + Q.Marker_Bytes + Op_RGBA_Bytes * Unsigned_64 (Store.Decoded_Ceiling / Store.Pixel_Bytes);

   --  Readable reasons: the runtime has no enumeration images.
   function Reason (Error : Q.Failure) return String is
     (case Error is
        when Q.No_Failure => "no image",
        when Q.Bad_Magic => "not a QOI file",
        when Q.Bad_Size => "invalid size",
        when Q.Over_Limit => "larger than its raster",
        when Q.Bad_Channels => "invalid channel count",
        when Q.Bad_Colorspace => "invalid colour space",
        when Q.Run_Past_End => "run past the last pixel",
        when Q.Bad_Marker => "bad end marker",
        when Q.Trailing_Data => "data after the end marker",
        when Q.Truncated => "truncated");

   procedure Load_Catalog is
      procedure Read is new Files.Read (Feed => Catalog.Feed, Wants_More => Catalog.Wants_More);
      Result : Files.Read_Result;
      Path : String (1 .. Files.Maximum_Path);
      Length : Natural;
      Loaded : Catalog.Load_Result;
      Reserved : Boolean;
   begin
      if Store.Capacity /= 0 then
         return;
      end if;
      Catalog.Begin_Load;
      Read (Package_Path & Catalog_File, Catalog.Maximum_Catalog_Bytes, Load_Budget_Ms,
            Result, Path, Length);
      Catalog.End_Load (not Files.Read_Whole (Result), Loaded);
      declare
         Shown : constant String :=
           (if Length = 0 then Package_Path & Catalog_File else Path (1 .. Length));
      begin
         if Result.Status = Files.Too_Large then
            Desktop_Logs.Warn ("desktop: CAPACITY wallpaper-catalog-bytes limit=" &
              Natural'Image (Catalog.Maximum_Catalog_Bytes) & " demand>" &
              Natural'Image (Catalog.Maximum_Catalog_Bytes) & " " & Shown & Unavailable_Catalog);
         elsif not Files.Read_Whole (Result) then
            Desktop_Logs.Warn ("desktop: wallpaper catalog " & Shown & " unavailable: " &
              Files.Describe (Result) & Unavailable_Catalog);
         elsif Loaded.Status /= Catalog.Loaded then
            Desktop_Logs.Warn ("desktop: wallpaper catalog " & Shown & " rejected: " &
              Catalog.Describe (Loaded) & Unavailable_Catalog);
         else
            Desktop_Logs.Write ("desktop: wallpaper catalog " & Shown & " " &
              Catalog.Describe (Loaded) & LF);
         end if;
      end;
      if Catalog.Count > 0 then
         Store.Reserve (Catalog.Count, Reserved);
         if not Reserved then
            Desktop_Logs.Warn ("desktop: no memory to track" & Catalog.Count'Image &
              " wallpaper(s)" & Unavailable_Image);
         end if;
      end if;
   end Load_Catalog;

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
      Name : constant String := Catalog.File (Asset);
   begin
      Store.Begin_Load (Asset);
      Read (Package_Path & Name, Maximum_Image_Bytes, Load_Budget_Ms, Result, Path, Length);
      Store.End_Load (Asset, not Files.Read_Whole (Result), Decoded);
      declare
         Shown : constant String := (if Length = 0 then Package_Path & Name else Path (1 .. Length));
      begin
         if Decoded.Loaded then
            Desktop_Logs.Write ("desktop: wallpaper loaded " & Shown & " size=" &
              Decoded.Width'Image & " x" & Decoded.Height'Image & " bytes=" &
              Result.Bytes'Image & " ms=" & Result.Elapsed_Ms'Image & LF);
         elsif Decoded.Refused = Store.Over_Ceiling then
            Desktop_Logs.Warn ("desktop: CAPACITY wallpaper-decoded-bytes limit=" &
              Natural'Image (Decoded.Limit) & " demand=" & Store.Demand_Bytes'Image (Decoded.Demand) &
              " " & Shown & Unavailable_Image);
         elsif Decoded.Refused = Store.No_Memory then
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " unavailable: no memory for" &
              Store.Demand_Bytes'Image (Decoded.Demand) & " decoded bytes" & Unavailable_Image);
         elsif Result.Status = Files.No_Asset_Root then
            Desktop_Logs.Warn ("desktop: wallpaper unavailable: " & Files.Describe (Result) &
                               Unavailable_Image);
         elsif not Files.Read_Whole (Result) then
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " unavailable: " &
                               Files.Describe (Result) & Unavailable_Image);
         else
            Desktop_Logs.Warn ("desktop: wallpaper " & Shown & " rejected: " &
                               Reason (Decoded.Error) & Unavailable_Image);
         end if;
      end;
   end Load;

   --  Load Asset if it has not been tried.
   procedure Prepare_Image (Asset : Catalog.Wallpaper_Count) is
   begin
      if Asset /= Catalog.No_Wallpaper and then Asset <= Store.Capacity and then
        not Store.Busy and then Store.Current (Asset) = Store.Not_Loaded
      then
         Load (Asset);
      end if;
   end Prepare_Image;

   Warned_Name : A.Image_Name := A.Default_Image;

   procedure Settle (Requested : in out CuBit.Appearance.Preferences) is
      Asked : constant A.Image_Name := Requested.Image;
      Result : Choice.Outcome;
   begin
      Choice.Settle (Requested, Result);
      if Result = Choice.Unknown_Wallpaper and then Asked /= Warned_Name then
         Warned_Name := Asked;
         Desktop_Logs.Warn ("desktop: wallpaper """ & A.Name (Asked) &
           """ is not in the catalog; using the default """ & A.Name (Requested.Image) & """");
      end if;
   end Settle;

   procedure Prepare (Requested : CuBit.Appearance.Preferences) is
      Index : Catalog.Wallpaper_Count;
      Result : Choice.Outcome;
   begin
      Choice.Choose (Requested, Index, Result);
      Prepare_Image (Index);
   end Prepare;

   procedure Resolve (Requested : CuBit.Appearance.Preferences;
                      Shown : out Desktop_Backdrop_Style.Backdrop) is
      Index : Catalog.Wallpaper_Count;
      Result : Choice.Outcome;
   begin
      Choice.Choose (Requested, Index, Result);
      Prepare_Image (Index);
      --  An image that will not load falls back to the default wallpaper.
      if Index /= Catalog.No_Wallpaper and then not Store.Ready (Index) then
         Prepare_Image (Catalog.Default_Wallpaper);
      end if;
      Shown := Choice.Style_For (Requested, Choice.Shown_Index (Index));
   end Resolve;
end Desktop_Wallpaper_Assets;
