with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Desktop_Wallpaper is
   use CuBit.Appearance;
   type Pixels is array (Natural range <>) of Unsigned_32
     with Convention => C;
   Source : constant Pixels (0 .. Source_Width * Source_Height - 1)
     with Import, Convention => C, External_Name => "cubit_desktop_wallpaper";
   Cubie_Source : constant Pixels (0 .. Cubie_Width * Cubie_Height - 1)
     with Import, Convention => C, External_Name => "cubit_desktop_wallpaper_cubie";

   procedure Render
     (Target : System.Address;
      Width, Height, Pitch : Positive)
   is
   begin
      Paint (Target, Width, Height, Pitch, 0, 0, Width, Height);
   end Render;

   procedure Paint
     (Target : System.Address;
      Width, Height, Pitch : Positive;
      X, Y, W, H : Natural;
      Style : Preferences := Default)
   is
      --  Caller owns Pitch*Height writable bytes, validated by the display
      --  protocol before allocation. Only visible pixels are touched.
      Draw_Width, Draw_Height : Positive;
      Image_Width : constant Positive :=
        (if Style.Backdrop = Cubie then Cubie_Width else Source_Width);
      Image_Height : constant Positive :=
        (if Style.Backdrop = Cubie then Cubie_Height else Source_Height);
      Left, Top : Integer;
      Image_X, Image_Y : Integer;
      Background_Color : constant Unsigned_32 :=
        (if Style.Backdrop = Ocean then 16#FF20_4058#
         elsif Style.Scheme = Alloy_Dark then 16#FF20_282E#
         else 16#FF54_5D63#);
      SX, SY, X0, Y0, X1, Y1, FX, FY : Natural;

      function Sample (X, Y : Natural) return Unsigned_32 is
        (if Style.Backdrop = Cubie then Cubie_Source (Y * Image_Width + X)
         else Source (Y * Image_Width + X)) with Inline;

      function Blend (A, B : Unsigned_32; Fraction : Natural)
        return Unsigned_32
      is
         Result : Unsigned_32 := 16#FF00_0000#;
         Channel : Unsigned_32;
      begin
         --  Integer bilinear filtering; only exposed damage is resampled.
         for Component in 0 .. 2 loop
            Channel :=
              ((Shift_Right (A, Component * 8) and 255) *
                 Unsigned_32 (256 - Fraction) +
               (Shift_Right (B, Component * 8) and 255) *
                 Unsigned_32 (Fraction) + 128) / 256;
            Result := Result or Shift_Left (Channel, Component * 8);
         end loop;
         return Result;
      end Blend;
   begin
      if W = 0 or else H = 0 then
         return;
      end if;
      if Style.Position = Center then
         Draw_Width := Image_Width;
         Draw_Height := Image_Height;
      elsif (Unsigned_64 (Width) * Unsigned_64 (Image_Height) >=
         Unsigned_64 (Height) * Unsigned_64 (Image_Width)
        ) = (Style.Position = Fill)
      then
         Draw_Width := Width;
         Draw_Height := Positive
           ((Unsigned_64 (Width) * Unsigned_64 (Image_Height) + Unsigned_64 (Image_Width) - 1) /
             Unsigned_64 (Image_Width));
      else
         Draw_Height := Height;
         Draw_Width := Positive
           ((Unsigned_64 (Height) * Unsigned_64 (Image_Width) + Unsigned_64 (Image_Height) - 1) /
             Unsigned_64 (Image_Height));
      end if;
      Left := (Width - Draw_Width) / 2;
      Top := (Height - Draw_Height) / 2;
      for Row_Y in Y .. Y + H - 1 loop
         Image_Y := Row_Y - Top;
         Y0 := 0;
         Y1 := 0;
         FY := 0;
         if Image_Y >= 0 and then Image_Y < Draw_Height then
            SY := (if Draw_Height = 1 then 0 else
              Natural (Unsigned_64 (Image_Y) * Unsigned_64 (Image_Height - 1) * 256 /
                       Unsigned_64 (Draw_Height - 1)));
            Y0 := SY / 256;
            Y1 := Natural'Min (Y0 + 1, Image_Height - 1);
            FY := SY mod 256;
         end if;
         for Column_X in X .. X + W - 1 loop
            Image_X := Column_X - Left;
            if Style.Backdrop not in Wallpaper | Cubie or else
              Image_X < 0 or else Image_X >= Draw_Width or else
              Image_Y < 0 or else Image_Y >= Draw_Height
            then
               declare
                  Pixel : Unsigned_32 with Import, Address => Target +
                    Storage_Offset (Row_Y * Pitch + Column_X * 4);
               begin
                  Pixel := Background_Color;
               end;
            else
            SX := (if Draw_Width = 1 then 0 else
              Natural (Unsigned_64 (Image_X) * Unsigned_64 (Image_Width - 1) * 256 /
                       Unsigned_64 (Draw_Width - 1)));
            X0 := SX / 256;
            X1 := Natural'Min (X0 + 1, Image_Width - 1);
            FX := SX mod 256;
            declare
               Pixel : Unsigned_32 with Import,
                 Address => Target +
                   Storage_Offset (Row_Y * Pitch + Column_X * 4);
            begin
               Pixel := Blend
                 (Blend (Sample (X0, Y0), Sample (X1, Y0), FX),
                  Blend (Sample (X0, Y1), Sample (X1, Y1), FX), FY);
            end;
            end if;
         end loop;
      end loop;
   end Paint;
end Desktop_Wallpaper;
