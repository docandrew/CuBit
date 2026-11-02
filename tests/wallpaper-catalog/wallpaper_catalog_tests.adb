--  Hosted tests of the wallpaper catalog and everything that reads it
--  (tests/wallpaper-catalog/README.md). Mode "fallback" (the second run)
--  checks the flat-colour fallback when even the default wallpaper is
--  unavailable; the store is one per process, so it needs its own run.
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Interfaces.Wallpapers;
with CCL.Setting_Kinds;
with CCL.Typed_Settings;
with CCL.Typed_Values;
with CuBit.Appearance;
with CuBit.QOI;
with Desktop_Appearance_Settings;
with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Catalog;
with Desktop_Wallpaper_Choice;
with Desktop_Wallpaper_Store;

procedure Wallpaper_Catalog_Tests is
   package A renames CuBit.Appearance;
   package Q renames CuBit.QOI;
   package W renames CCL.Interfaces.Wallpapers;
   package Catalog renames Desktop_Wallpaper_Catalog;
   package Store renames Desktop_Wallpaper_Store;
   package Choice renames Desktop_Wallpaper_Choice;
   package Settings renames Desktop_Appearance_Settings;
   package Style renames Desktop_Backdrop_Style;
   use type Catalog.Load_Status;
   use type Store.Status, Store.Refusal, Store.Demand_Bytes;
   use type Q.Failure, Q.Byte_Array;
   use type Choice.Outcome;
   use type A.Preferences, A.Image_Name, A.Backdrop_Kind, A.Placement, A.Color_Scheme, A.Solid_Color;
   use type CCL.Setting_Kinds.Setting_Kind;
   use type Style.Backdrop;

   LF : constant Character := Character'Val (10);
   Fallback_Mode : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "fallback";
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Line (Name : String; Label : String := ""; File : String := "";
                  Attribution : String := "Test picture, public domain") return String is
     ("(Wallpaper name => """ & Name & """ label => """ & (if Label = "" then Name else Label) &
      """ file => """ & (if File = "" then Name & ".qoi" else File) & """ attribution => """ &
      Attribution & """)" & LF);

   function Load (Source : String) return Catalog.Load_Result is
      Result : Catalog.Load_Result;
   begin
      Catalog.Load (Source, Result);
      return Result;
   end Load;

   procedure Expect (Source : String; Status : Catalog.Load_Status; At_Line : Natural; What : String) is
      Before : constant Natural := Catalog.Count;
      Result : constant Catalog.Load_Result := Load (Source);
   begin
      Check (Result.Status = Status and then Result.Line = At_Line,
             What & ": " & Catalog.Describe (Result));
      Check (Catalog.Count = Before, What & ": a rejected catalog leaves the one in use");
   end Expect;

   --  A QOI stream for a Width x Height image of Pixel (16#AARRGGBB#),
   --  opaque: the first pixel as Op_RGB, then runs.
   function Image_Stream (Width, Height : Positive; Pixel : Unsigned_32) return Q.Byte_Array is
      function Big (Value : Natural) return Q.Byte_Array is
        [Unsigned_8 (Value / 2**24), Unsigned_8 (Value / 2**16 mod 256),
         Unsigned_8 (Value / 2**8 mod 256), Unsigned_8 (Value mod 256)];
      Longest_Run : constant := 62;
      Count : constant Positive := Width * Height;
      Runs : constant Natural := (Count - 1 + Longest_Run - 1) / Longest_Run;
      Result : Q.Byte_Array (1 .. Q.Header_Bytes + 4 + Runs + Q.Marker_Bytes);
      Next : Positive := 1;
      Left : Natural := Count - 1;
      procedure Emit (Bytes : Q.Byte_Array) is
      begin
         Result (Next .. Next + Bytes'Length - 1) := Bytes;
         Next := Next + Bytes'Length;
      end Emit;
   begin
      Emit ([Q.Magic_Q, Q.Magic_O, Q.Magic_I, Q.Magic_F] & Big (Width) & Big (Height) &
           Q.Byte_Array'[Q.Channels_RGB, Q.Colorspace_SRGB]);
      Emit ([Q.Op_RGB, Unsigned_8 (Shift_Right (Pixel, 16) and 255),
            Unsigned_8 (Shift_Right (Pixel, 8) and 255), Unsigned_8 (Pixel and 255)]);
      while Left > 0 loop
         declare
            Run : constant Positive := Natural'Min (Left, Longest_Run);
         begin
            Emit ([1 => Q.Op_Run or Unsigned_8 (Run - Q.Run_Bias)]);
            Left := Left - Run;
         end;
      end loop;
      Emit ([0, 0, 0, 0, 0, 0, 0, Q.Marker_Last]);
      return Result;
   end Image_Stream;

   --  Load Asset from Bytes in chunks of Chunk bytes (0: all at once).
   function Decode (Asset : Store.Image; Bytes : Q.Byte_Array; Chunk : Natural := 0;
                    Read_Failed : Boolean := False) return Store.Load_Result is
      Result : Store.Load_Result;
      Next : Positive := Bytes'First;
   begin
      Store.Begin_Load (Asset);
      if Chunk = 0 then
         Store.Feed (Asset, Bytes);
      else
         while Next <= Bytes'Last and then Store.Wants_More (Asset) loop
            declare
               Last : constant Natural := Natural'Min (Bytes'Last, Next + Chunk - 1);
            begin
               Store.Feed (Asset, Bytes (Next .. Last));
               Next := Last + 1;
            end;
         end loop;
      end if;
      Store.End_Load (Asset, Read_Failed, Result);
      return Result;
   end Decode;

   function Every_Pixel (Asset : Store.Image; Pixel : Unsigned_32) return Boolean is
      type Pixels is array (Natural range <>) of Unsigned_32;
      Raster : constant Pixels (0 .. Store.Width (Asset) * Store.Height (Asset) - 1)
        with Import, Address => Store.Pixels (Asset);
   begin
      return (for all P of Raster => P = (Pixel or 16#FF00_0000#));
   end Every_Pixel;

   function Preferences (Name : String; Placement : A.Placement := A.Fill) return A.Preferences is
     ((A.Default with delta Kind => A.Image, Image => A.To_Image_Name (Name), Position => Placement));

   --  The catalog's eight test wallpapers: a, b, ..., all distinct.
   Names : constant array (1 .. 8) of Unbounded_String :=
     [To_Unbounded_String ("cubes"), To_Unbounded_String ("night-sky"), To_Unbounded_String ("dunes"),
      To_Unbounded_String ("bad-header"), To_Unbounded_String ("too-large"),
      To_Unbounded_String ("big-one"), To_Unbounded_String ("big-two"), To_Unbounded_String ("missing")];
   function Full_Catalog return String is
      Text : Unbounded_String;
   begin
      for N of Names loop
         Append (Text, Line (To_String (N), "Label " & To_String (N)));
      end loop;
      return To_String (Text);
   end Full_Catalog;

   Loaded : Catalog.Load_Result;
   Reserved : Boolean;
   Decoded : Store.Load_Result;
   Index : Catalog.Wallpaper_Count;
   Outcome : Choice.Outcome;
begin
   --  The published line is the type system's canonical rendering.
   declare
      Rendered : CCL.Typed_Values.Check_Result;
      Text : constant String := Line ("cubes", "Cubes");
   begin
      CCL.Typed_Values.Check (W.TYPE_SOURCE, W.ROOT_TYPE_NAME, Text (Text'First .. Text'Last - 1), Rendered);
      Check (Rendered.Success and then Rendered.Canonical (1 .. Rendered.Length) = Text (Text'First .. Text'Last - 1),
             "a catalog line is the checker's canonical Wallpaper");
   end;

   if Fallback_Mode then
      --  The default (1) is unavailable and 3 is missing: 3 shows the flat
      --  colour; 2 still shows itself.
      Loaded := Load (Line ("cubes") & Line ("night-sky") & Line ("dunes"));
      Check (Loaded.Status = Catalog.Loaded and then Catalog.Count = 3, "three wallpapers");
      Store.Reserve (Catalog.Count, Reserved);
      Check (Reserved and then Store.Capacity = 3, "store sized to the catalog");
      Decoded := Decode (1, Image_Stream (4, 4, 16#11_2233#), Read_Failed => True);
      Check (not Decoded.Loaded and then Store.Current (1) = Store.Unavailable, "default unavailable");
      Decoded := Decode (2, Image_Stream (5, 3, 16#44_5566#));
      Check (Decoded.Loaded and then Store.Ready (2), "second wallpaper decodes");
      Store.Give_Up (3);
      Check (Store.Current (3) = Store.Unavailable, "third gives up without a read");
      Check (Choice.Shown_Index (2) = 2, "a decoded wallpaper shows itself");
      Check (Choice.Shown_Index (3) = Catalog.No_Wallpaper, "no default: the flat colour");
      Choice.Choose (Preferences ("dunes", A.Fit), Index, Outcome);
      Check (Index = 3 and then Outcome = Choice.Kept, "dunes chosen");
      Check (Choice.Style_For (Preferences ("dunes", A.Fit), Choice.Shown_Index (Index)) =
               (Style.Flat with delta Position => A.Fit),
             "the flat colour keeps the placement and scheme");
      Check (Choice.Style_For ((A.Default with delta Kind => A.Solid, Color => A.Ocean,
                                                     Scheme => A.Alloy_Dark), 0) =
               (Style.Flat with delta Color => A.Ocean, Scheme => A.Alloy_Dark),
             "a solid colour is drawn as itself");
      Check (Style.Color ((Style.Flat with delta Color => A.Ocean)) = Style.Ocean_Color, "ocean colour");
      Check (Style.Color ((Style.Flat with delta Scheme => A.Alloy_Dark)) = Style.Slate_Dark_Color,
             "dark slate");
      Put_Line ("Wallpaper fallback tests:" & Checks'Image & " checks," & Failures'Image & " failures");
      if Failures > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
      return;
   end if;

   --  Catalog decoding: valid catalogs of 1, 3 and 8 wallpapers; there is
   --  no fixed number of slots.
   Loaded := Load (Line ("cubes", "Cubes"));
   Check (Loaded.Status = Catalog.Loaded and then Loaded.Count = 1 and then Catalog.Count = 1, "one wallpaper");
   Check (A.Name (Catalog.Name (1)) = "cubes" and then Catalog.Label (1) = "Cubes" and then
          Catalog.File (1) = "cubes.qoi", "its fields");
   Loaded := Load (Line ("cubes") & Line ("night-sky") & Line ("dunes"));
   Check (Loaded.Status = Catalog.Loaded and then Catalog.Count = 3, "three wallpapers");
   Check (Catalog.Find (A.To_Image_Name ("dunes")) = 3 and then Catalog.Find (A.To_Image_Name ("cubes")) = 1,
          "find keeps catalog order");
   Check (Catalog.Find (A.To_Image_Name ("removed-picture")) = Catalog.No_Wallpaper, "an unknown name is not found");
   --  Without the last line feed; and fed one byte at a time.
   declare
      Text : constant String := Full_Catalog;
      Result : Catalog.Load_Result;
   begin
      Loaded := Load (Text (Text'First .. Text'Last - 1));
      Check (Loaded.Status = Catalog.Loaded and then Catalog.Count = 8, "a last line without its line feed");
      Catalog.Begin_Load;
      for C of Text loop
         Catalog.Feed ([1 => Unsigned_8 (Character'Pos (C))]);
      end loop;
      Catalog.End_Load (False, Result);
      Check (Result.Status = Catalog.Loaded and then Catalog.Count = 8, "fed one byte at a time");
      for I in Names'Range loop
         Check (A.Name (Catalog.Name (I)) = To_String (Names (I)) and then
                Catalog.Label (I) = "Label " & To_String (Names (I)), "entry" & I'Image);
      end loop;
   end;

   --  Malformed catalogs load nothing.
   Expect ("garbage" & LF, Catalog.Malformed, 1, "not CCL");
   Expect (Line ("cubes") & LF & Line ("dunes"), Catalog.Malformed, 2, "an empty line");
   Expect ("(Wallpaper label => ""x"" name => ""cubes"" file => ""cubes.qoi"" attribution => ""a"")" & LF,
           Catalog.Malformed, 1, "fields out of order");
   Expect ("(Wallpaper  name => ""cubes"" label => ""x"" file => ""cubes.qoi"" attribution => ""a"")" & LF,
           Catalog.Malformed, 1, "not canonical spacing");
   Expect ("(Wallpaper name => ""cubes"" label => ""x"" file => ""cubes.qoi"")" & LF,
           Catalog.Malformed, 1, "a missing field");
   Expect (Line ("cubes") & "(Wallpaper name => ""a"" label => ""x"" file => ""a.qoi"" attribution => ""a"") x" & LF,
           Catalog.Malformed, 2, "data after the value");
   Expect (Line ("cubes", Attribution => [1 .. 1100 => 'x']), Catalog.Malformed, 1, "a line over a typed value's budget");
   Expect (Line ("cubes") & Line ("dunes") & Line ("cubes"), Catalog.Duplicate_Name, 3, "a duplicate name");
   Expect (Line ("Cubes"), Catalog.Bad_Name, 1, "an upper-case name");
   Expect (Line ("a--b"), Catalog.Bad_Name, 1, "a doubled separator");
   Expect (Line ("-a"), Catalog.Bad_Name, 1, "a leading separator");
   Expect (Line ([1 .. A.Maximum_Image_Name + 1 => 'a']), Catalog.Bad_Name, 1, "an overlong name");
   Expect (Line ("cubes", File => "other.qoi"), Catalog.Bad_File, 1, "another file");
   Expect (Line ("cubes", File => "../cubes.qoi"), Catalog.Bad_File, 1, "a path out of the package");
   Expect (Line ("cubes", File => "cubes.png"), Catalog.Bad_File, 1, "not QOI");
   Expect (Line ("cubes", Label => [1 .. Catalog.Maximum_Label + 1 => 'x']), Catalog.Bad_Label, 1,
           "an overlong label");
   Expect (Line ("cubes", Label => "tab" & Character'Val (9)), Catalog.Bad_Label, 1, "an unprintable label");
   Expect (Line ("cubes", Attribution => ""), Catalog.No_Attribution, 1, "no attribution");
   Expect ("", Catalog.Empty, 0, "an empty catalog");
   declare
      Result : Catalog.Load_Result;
   begin
      Catalog.Begin_Load;
      Catalog.Feed ([1 => Character'Pos ('(')]);
      Catalog.End_Load (True, Result);
      Check (Result.Status = Catalog.Read_Failed and then Catalog.Count = 8, "a failed read loads nothing");
   end;

   --  The store, sized to the catalog: images of any size, each from its
   --  QOI header, on CuAlloc.
   Store.Reserve (Catalog.Count, Reserved);
   Check (Reserved and then Store.Capacity = 8, "store sized to the catalog");
   Check ((for all I in 1 .. 8 => Store.Current (I) = Store.Not_Loaded), "nothing loaded yet");
   Decoded := Decode (1, Image_Stream (64, 18, 16#00_A0_B0_C0#), Chunk => 1);
   Check (Decoded.Loaded and then Store.Ready (1) and then Store.Width (1) = 64 and then
          Store.Height (1) = 18 and then Every_Pixel (1, 16#00_A0_B0_C0#), "64 x 18, byte by byte");
   Decoded := Decode (2, Image_Stream (33, 41, 16#00_10_20_30#), Chunk => 13);
   Check (Decoded.Loaded and then Store.Width (2) = 33 and then Store.Height (2) = 41 and then
          Every_Pixel (2, 16#00_10_20_30#), "33 x 41 in 13-byte chunks");
   Decoded := Decode (3, Image_Stream (7, 5, 16#00_FF_00_FF#));
   Check (Decoded.Loaded and then Store.Width (3) = 7 and then Every_Pixel (3, 16#00_FF_00_FF#),
          "a third image: no fixed slots");
   Check (Store.Used = Store.Pixel_Bytes * (64 * 18 + 33 * 41 + 7 * 5), "used bytes are the rasters'");
   Check (Every_Pixel (1, 16#00_A0_B0_C0#), "the first raster is untouched by the others");

   --  A bad QOI header is refused before any memory is taken.
   declare
      Bytes : Q.Byte_Array := Image_Stream (4, 4, 0);
      Used : constant Natural := Store.Used;
   begin
      Bytes (1) := Character'Pos ('x');
      Decoded := Decode (4, Bytes, Chunk => 3);
      Check (not Decoded.Loaded and then Decoded.Error = Q.Bad_Magic and then
             Store.Current (4) = Store.Unavailable and then Store.Used = Used, "a bad magic number");
   end;
   --  An image over the decoded-memory ceiling: CAPACITY, with the limit
   --  and the demand, never decoded.
   declare
      Header_Only : constant Q.Byte_Array := Image_Stream (1, 1, 0) (1 .. Q.Header_Bytes);
      Huge : Q.Byte_Array := Header_Only;
      Used : constant Natural := Store.Used;
      Side : constant := 8192;
      Tall : constant := 4096;
   begin
      Huge (5 .. 12) := [0, 0, Unsigned_8 (Side / 256), 0, 0, 0, Unsigned_8 (Tall / 256), 0];
      Store.Begin_Load (5);
      Store.Feed (5, Huge);
      Check (not Store.Wants_More (5), "an image over the ceiling is refused at its header");
      Store.End_Load (5, False, Decoded);
      Check (not Decoded.Loaded and then Decoded.Refused = Store.Over_Ceiling and then
             Decoded.Limit = Store.Decoded_Ceiling and then
             Decoded.Demand = Store.Demand_Bytes (Used) + Side * Tall * Store.Pixel_Bytes and then
             Store.Used = Used and then Store.Current (5) = Store.Unavailable,
             "CAPACITY: limit" & Decoded.Limit'Image & " demand" & Decoded.Demand'Image);
   end;
   --  The ceiling is for all images together: a second 32 MiB image does
   --  not fit beside the first.
   Decoded := Decode (6, Image_Stream (4096, 2048, 16#00_12_34_56#), Chunk => 65_536);
   Check (Decoded.Loaded and then Store.Ready (6) and then Every_Pixel (6, 16#00_12_34_56#), "a 32 MiB image fits");
   declare
      Used : constant Natural := Store.Used;
   begin
      Decoded := Decode (7, Image_Stream (4096, 2048, 0), Chunk => 65_536);
      Check (not Decoded.Loaded and then Decoded.Refused = Store.Over_Ceiling and then
             Decoded.Demand = Store.Demand_Bytes (Used) + 4096 * 2048 * Store.Pixel_Bytes and then
             Store.Used = Used, "a second one passes the ceiling with the first");
   end;
   --  A missing file (the read fails): unavailable, memory returned.
   declare
      Used : constant Natural := Store.Used;
   begin
      Decoded := Decode (8, Image_Stream (16, 16, 0) (1 .. 30), Read_Failed => True);
      Check (not Decoded.Loaded and then Store.Current (8) = Store.Unavailable and then Store.Used = Used,
             "a file that cannot be read whole");
   end;
   --  A file that ends inside its header.
   Check (Store.Current (4) = Store.Unavailable, "bad header stays unavailable");

   --  Choosing: a name in the catalog, an unknown one (the default), a
   --  missing file (the default), a solid colour.
   Choice.Choose (Preferences ("night-sky"), Index, Outcome);
   Check (Index = 2 and then Outcome = Choice.Kept, "a catalog name");
   Choice.Choose (A.Default, Index, Outcome);
   Check (Index = 1 and then Outcome = Choice.Defaulted, "no name: the default");
   Choice.Choose (Preferences ("removed-picture"), Index, Outcome);
   Check (Index = 1 and then Outcome = Choice.Unknown_Wallpaper, "an unknown name: the default");
   Choice.Choose ((A.Default with delta Kind => A.Solid, Color => A.Ocean), Index, Outcome);
   Check (Index = 0 and then Outcome = Choice.Kept, "a solid colour");
   Choice.Choose (Preferences ("missing", A.Center), Index, Outcome);
   Check (Index = 8 and then Choice.Shown_Index (Index) = 1, "a missing file shows the default");
   Check (Choice.Style_For (Preferences ("missing", A.Center), Choice.Shown_Index (Index)) =
            (Style.Flat with delta Image => 1, Width => 64, Height => 18, Position => A.Center),
          "the default at its decoded size, in the requested placement");
   Check (Choice.Style_For (Preferences ("dunes", A.Fit), Choice.Shown_Index (3)) =
            (Style.Flat with delta Image => 3, Width => 7, Height => 5, Position => A.Fit),
          "the third image at its decoded size");
   for Placement in A.Placement loop
      Check (Choice.Style_For (Preferences ("night-sky", Placement), 2).Position = Placement,
             "placement " & Placement'Image);
   end loop;

   --  The persisted appearance: canonical, round trips, and a stored
   --  wallpaper the catalog no longer holds falls back to the default.
   Check (CCL.Typed_Settings.Kind_Of ("desktop.appearance.v2") = CCL.Setting_Kinds.Appearance_Setting,
          "desktop.appearance.v2 is typed");
   for Scheme in A.Color_Scheme loop
      for Placement in A.Placement loop
         for Kind in A.Backdrop_Kind loop
            for Color in A.Solid_Color loop
               declare
                  Value : constant A.Preferences :=
                    (Scheme => Scheme, Kind => Kind, Image => A.To_Image_Name ("night-sky"),
                     Position => Placement, Color => Color);
                  Text : constant String := Settings.Encode (Value);
                  Result : CCL.Typed_Settings.Check_Result;
                  Item : A.Preferences;
                  OK : Boolean;
               begin
                  CCL.Typed_Settings.Check (CCL.Setting_Kinds.Appearance_Setting, Text, Result);
                  Check (Result.Success and then Result.Canonical (1 .. Result.Length) = Text,
                         "canonical " & Text);
                  Settings.Decode (Text, Item, OK);
                  Check (OK and then Item.Scheme = Scheme and then Item.Kind = Kind and then
                         (if Kind = A.Image then Item.Image = Value.Image and then Item.Position = Placement
                          else Item.Color = Color), "round trip " & Text);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   declare
      Stored : constant String :=
        "(Appearance scheme => Color_Scheme.Alloy_Dark backdrop => " &
        "(Backdrop.Image (Wallpaper_Choice name => ""cubie"" placement => Placement.Fit)))";
      Item : A.Preferences;
      OK : Boolean;
   begin
      Settings.Decode (Stored, Item, OK);
      Check (OK and then A.Name (Item.Image) = "cubie", "a removed wallpaper's name still decodes");
      Choice.Settle (Item, Outcome);
      Check (Outcome = Choice.Unknown_Wallpaper and then A.Name (Item.Image) = "cubes" and then
             Item.Position = A.Fit and then Item.Scheme = A.Alloy_Dark,
             "and settles to the default wallpaper, keeping the rest");
      Settings.Decode ("(Appearance scheme => Color_Scheme.Alloy_Dark backdrop => " &
                       "(Backdrop.Image (Wallpaper_Choice name => ""Upper"" placement => Placement.Fit)))", Item, OK);
      Check (not OK and then Item = A.Default, "a name the catalog could never hold is refused");
      Settings.Decode ("100", Item, OK);
      Check (not OK and then Item = A.Default, "the old three-digit encoding is refused");
      Settings.Decode ("(Appearance scheme => Color_Scheme.Alloy_Light backdrop => " &
                       "(Backdrop.Image (Wallpaper_Choice name => """" placement => Placement.Fill)))", Item, OK);
      Check (OK and then Item.Image.Length = 0, "an empty name decodes");
      Choice.Settle (Item, Outcome);
      Check (Outcome = Choice.Defaulted and then A.Name (Item.Image) = "cubes", "and settles to the default");
   end;

   Put_Line ("Wallpaper catalog tests:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Wallpaper_Catalog_Tests;
