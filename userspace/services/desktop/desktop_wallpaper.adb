with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Desktop_Wallpaper is
   type Pixels is array (Natural range <>) of Unsigned_32
     with Convention => C;
   Source : constant Pixels (0 .. Source_Width * Source_Height - 1)
     with Import, Convention => C, External_Name => "cubit_desktop_wallpaper";

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
      X, Y, W, H : Natural)
   is
      --  Caller owns Pitch*Height writable bytes, validated by the display
      --  protocol before allocation. Only visible pixels are touched.
      Draw_Width, Draw_Height : Positive;
      Left, Top : Natural;
      SX, SY, X0, Y0, X1, Y1, FX, FY : Natural;

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
      if Unsigned_64 (Width) * Source_Height >=
         Unsigned_64 (Height) * Source_Width
      then
         Draw_Width := Width;
         Draw_Height := Positive
           ((Unsigned_64 (Width) * Source_Height + Source_Width - 1) / Source_Width);
      else
         Draw_Height := Height;
         Draw_Width := Positive
           ((Unsigned_64 (Height) * Source_Width + Source_Height - 1) / Source_Height);
      end if;
      Left := (Draw_Width - Width) / 2;
      Top := (Draw_Height - Height) / 2;
      for Row_Y in Y .. Y + H - 1 loop
         SY := (if Draw_Height = 1 then 0 else
           Natural (Unsigned_64 (Row_Y + Top) * (Source_Height - 1) * 256 /
                    Unsigned_64 (Draw_Height - 1)));
         Y0 := SY / 256;
         Y1 := Natural'Min (Y0 + 1, Source_Height - 1);
         FY := SY mod 256;
         for Column_X in X .. X + W - 1 loop
            SX := (if Draw_Width = 1 then 0 else
              Natural (Unsigned_64 (Column_X + Left) * (Source_Width - 1) * 256 /
                       Unsigned_64 (Draw_Width - 1)));
            X0 := SX / 256;
            X1 := Natural'Min (X0 + 1, Source_Width - 1);
            FX := SX mod 256;
            declare
               Pixel : Unsigned_32 with Import,
                 Address => Target +
                   Storage_Offset (Row_Y * Pitch + Column_X * 4);
            begin
               Pixel := Blend
                 (Blend (Source (Y0 * Source_Width + X0),
                         Source (Y0 * Source_Width + X1), FX),
                  Blend (Source (Y1 * Source_Width + X0),
                         Source (Y1 * Source_Width + X1), FX), FY);
            end;
         end loop;
      end loop;
   end Paint;
end Desktop_Wallpaper;
