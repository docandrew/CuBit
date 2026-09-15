with Ada.Text_IO; use Ada.Text_IO;
with Desktop_Wallpaper;
with CuBit.Appearance; use CuBit.Appearance;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   type Pixels is array (Natural range <>) of Unsigned_32 with Convention => C;
   Source : constant Pixels
     (0 .. Desktop_Wallpaper.Source_Width * Desktop_Wallpaper.Source_Height - 1)
     with Import, Convention => C, External_Name => "cubit_desktop_wallpaper";
   Cubie_Source : constant Pixels
     (0 .. Desktop_Wallpaper.Cubie_Width * Desktop_Wallpaper.Cubie_Height - 1)
     with Import, Convention => C, External_Name => "cubit_desktop_wallpaper_cubie";

   procedure Check (Width, Height, Pitch : Positive;
                    Backdrop : Background := Wallpaper) is
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
        (Storage'Address + 32, Width, Height, Pitch, 0, 0, Width, Height,
         (Backdrop => Backdrop, others => <>));
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
      if Backdrop = Cubie and Width = 2048 and Height = 1152 then
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               pragma Assert (Pixel (X, Y) = Cubie_Source (Y * Width + X));
            end loop;
         end loop;
      end if;
      if Backdrop = Wallpaper and Width = 2048 and Height = 576 then
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               pragma Assert (Pixel (X, Y) = Source (Y * Width + X));
            end loop;
         end loop;
      end if;
      if Backdrop = Wallpaper and Width = 1024 and Height = 576 then
         --  Exact 1:1 centered crop: 512 source columns removed per side.
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               pragma Assert (Pixel (X, Y) = Source (Y * 2048 + X + 512));
            end loop;
         end loop;
      end if;
   end Check;

   procedure Check_Damage (Style : Preferences) is
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
begin
   Check (2048, 576, 8192);
   Check (1024, 576, 4096);
   Check (1366, 768, 5464);
   Check (1920, 1080, 7680);
   Check (768, 1024, 3072);
   Check (4096, 576, 16384);
   Check (320, 200, 1283);
   Check (1, 1, 4);
   Check (2048, 1152, 8192, Cubie);
   Check (1366, 768, 5464, Cubie);
   Check (1920, 1080, 7680, Cubie);
   Check (1024, 768, 4100, Cubie);
   Check (768, 1024, 3072, Cubie);
   Check (1, 1, 4, Cubie);
   for Scheme in Color_Scheme loop
      for Backdrop in Background loop
         for Position in Placement loop
            declare
               Style : constant Preferences := (Scheme, Backdrop, Position);
            begin
               pragma Assert (Valid (Encode (Style)));
               pragma Assert (Decode (Encode (Style)) = Style);
               Check_Damage (Style);
            end;
         end loop;
      end loop;
   end loop;
   Put_Line ("PASS: wallpaper aspect fill, exact crop, padding, bounds and tiled damage");
end Main;
