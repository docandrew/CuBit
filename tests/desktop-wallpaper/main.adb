with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Desktop_Backdrop_Style;
with Desktop_Wallpaper;
with Desktop_Wallpaper_Catalog;
with Desktop_Wallpaper_Store;
with CuBit.Appearance; use CuBit.Appearance;
with CuBit.QOI;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   package Store renames Desktop_Wallpaper_Store;
   package Catalog renames Desktop_Wallpaper_Catalog;
   package Style_Of renames Desktop_Backdrop_Style;
   use type Catalog.Load_Status;
   --  The built asset package (make -C kernel wallpaper-assets).
   Package_Directory : constant String :=
     (if Ada.Command_Line.Argument_Count >= 1 then Ada.Command_Line.Argument (1)
      else "build/Assets/cubit-wallpapers/2");   --  from kernel/

   type Bytes_Access is access CuBit.QOI.Byte_Array;
   function Read (Name : String) return Bytes_Access is
      use Ada.Streams.Stream_IO;
      Path : constant String := Package_Directory & "/" & Name;
      Data : constant Bytes_Access :=
        new CuBit.QOI.Byte_Array (1 .. Natural (Ada.Directories.Size (Path)));
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (File, In_File, Path);
      CuBit.QOI.Byte_Array'Read (Stream (File), Data.all);
      Close (File);
      return Data;
   end Read;

   --  Decode one asset file into the store, as Desktop does, in 64 KiB
   --  chunks.
   procedure Load (Asset : Store.Image) is
      Data : constant Bytes_Access := Read (Catalog.File (Asset));
      First : Positive := 1;
      Result : Store.Load_Result;
   begin
      Store.Begin_Load (Asset);
      while First <= Data'Last loop
         Store.Feed (Asset, Data (First .. Natural'Min (Data'Last, First + 65_535)));
         First := First + 65_536;
      end loop;
      Store.End_Load (Asset, False, Result);
      pragma Assert (Result.Loaded);
   end Load;

   type Pixels is array (Natural range <>) of Unsigned_32 with Convention => C;

   function Style (Asset : Natural; Scheme : Color_Scheme := Alloy_Light;
                   Position : Placement := Fill; Color : Solid_Color := Slate) return Style_Of.Backdrop is
     (if Asset = 0 then (Style_Of.Flat with delta Scheme => Scheme, Position => Position, Color => Color)
      else (Scheme => Scheme, Image => Asset, Width => Store.Width (Asset), Height => Store.Height (Asset),
            Position => Position, Color => Slate));

   procedure Check (Asset : Store.Image; Width, Height, Pitch : Positive) is
      IW : constant Positive := Store.Width (Asset);
      IH : constant Positive := Store.Height (Asset);
      Source : constant Pixels (0 .. IW * IH - 1) with Import, Address => Store.Pixels (Asset);
      Storage : aliased Storage_Array (0 .. Storage_Offset (Pitch * Height + 63)) :=
        (others => 16#CD#);
      function Pixel (X, Y : Natural) return Unsigned_32 is
         Value : Unsigned_32 with Import,
           Address => Storage'Address + 32 + Storage_Offset (Y * Pitch + X * 4);
      begin
         return Value;
      end Pixel;
   begin
      Desktop_Wallpaper.Paint
        (Storage'Address + 32, Width, Height, Pitch, 0, 0, Width, Height, Style (Asset));
      --  Every visible pixel is opaque artwork, not the untouched sentinel.
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            pragma Assert (Shift_Right (Pixel (X, Y), 24) = 255);
         end loop;
      end loop;
      for I in 0 .. 31 loop
         pragma Assert (Storage (Storage_Offset (I)) = 16#CD#);
         pragma Assert
           (Storage (Storage_Offset (32 + Pitch * Height + I)) = 16#CD#);
      end loop;
      for Y in 0 .. Height - 1 loop
         for X in Width * 4 .. Pitch - 1 loop
            pragma Assert (Storage (Storage_Offset (32 + Y * Pitch + X)) = 16#CD#);
         end loop;
      end loop;
      if Width = IW and Height = IH then
         --  At its own size the image is drawn exactly.
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               pragma Assert (Pixel (X, Y) = Source (Y * Width + X));
            end loop;
         end loop;
      elsif Height = IH and then Width < IW and then (IW - Width) mod 2 = 0 then
         --  Exact 1:1 centered crop: half the extra columns removed per side.
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               pragma Assert (Pixel (X, Y) = Source (Y * IW + X + (IW - Width) / 2));
            end loop;
         end loop;
      end if;
   end Check;

   procedure Check_Damage (Style : Style_Of.Backdrop) is
      Full, Partial : aliased Storage_Array (0 .. 120 * 96 * 4 - 1) :=
        (others => 16#CD#);
      In_Damage : Boolean;
   begin
      Desktop_Wallpaper.Paint (Full'Address, 120, 96, 480, 0, 0, 120, 96, Style);
      Desktop_Wallpaper.Paint
        (Partial'Address, 120, 96, 480, 7, 1, 31, 20, Style);
      for I in Partial'Range loop
         In_Damage := Natural (I) / 480 in 1 .. 20 and then
           (Natural (I) mod 480) / 4 in 7 .. 37;
         if In_Damage then
            pragma Assert (Partial (I) = Full (I));
         else
            pragma Assert (Partial (I) = 16#CD#);
         end if;
      end loop;
      --  Tiled damage, including all edges and partial final tiles, must
      --  recreate a full repaint byte-for-byte: no seams or crop-origin drift.
      for Y in 0 .. 13 loop
         for X in 0 .. 9 loop
            Desktop_Wallpaper.Paint
              (Partial'Address, 120, 96, 480, X * 13, Y * 7,
               Natural'Min (13, 120 - X * 13), Natural'Min (7, 96 - Y * 7), Style);
         end loop;
      end loop;
      pragma Assert (Partial = Full);
   end Check_Damage;

   Loaded : Catalog.Load_Result;
   Reserved : Boolean;
begin
   --  The package's own catalog says which wallpapers there are.
   declare
      Text : constant Bytes_Access := Read ("catalog.ccl");
      Source : String (1 .. Text'Length);
   begin
      for I in Source'Range loop
         Source (I) := Character'Val (Text (I));
      end loop;
      Catalog.Load (Source, Loaded);
      pragma Assert (Loaded.Status = Catalog.Loaded and then Catalog.Count > 0);
   end;
   Store.Reserve (Catalog.Count, Reserved);
   pragma Assert (Reserved);
   for Asset in 1 .. Catalog.Count loop
      Load (Asset);
      declare
         IW : constant Positive := Store.Width (Asset);
         IH : constant Positive := Store.Height (Asset);
      begin
         Put_Line (CuBit.Appearance.Name (Catalog.Name (Asset)) & ": " & Catalog.Label (Asset) & IW'Image & " x" & IH'Image);
         Check (Asset, IW, IH, IW * 4);
         Check (Asset, IW / 2 * 2 - (if IW > 2 then 2 else 0), IH, IW * 4);
         Check (Asset, 1366, 768, 5464);
         Check (Asset, 1920, 1080, 7680);
         Check (Asset, 768, 1024, 3072);
         Check (Asset, 2 * IW, IH, 8 * IW);
         Check (Asset, 320, 200, 1283);
         Check (Asset, 1, 1, 4);
      end;
   end loop;
   for Scheme in Color_Scheme loop
      for Position in Placement loop
         for Asset in 0 .. Catalog.Count loop
            Check_Damage (Style (Asset, Scheme, Position));
         end loop;
         Check_Damage (Style (0, Scheme, Position, Ocean));
      end loop;
   end loop;
   Put_Line ("PASS: every catalog wallpaper: aspect fill, exact crop, padding, bounds and tiled damage");
end Main;
