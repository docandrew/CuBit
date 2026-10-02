with Compositor_Image_Sampling;
with Compositor_Sampling;
with Compositor_Text;
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
   procedure Paint_Output
     (Target : System.Address; Pitch : Positive;
      Screen : CuBit.Display_Geometry.Output;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : Preferences := Default)
   is
      package G renames CuBit.Display_Geometry;
      package S renames Compositor_Image_Sampling;
      use type G.Pixel_Edge, S.Wide;
      W : constant S.Wide := S.Wide (Bounds.Right) - S.Wide (Bounds.Left);
      H : constant S.Wide := S.Wide (Bounds.Bottom) - S.Wide (Bounds.Top);
      Area : constant G.Physical_Rectangle := Compositor_Text.Clip (Screen, Bounds, Damage);
      IW : constant S.Extent := (if Style.Backdrop = Cubie then Cubie_Width else Source_Width);
      IH : constant S.Extent := (if Style.Backdrop = Cubie then Cubie_Height else Source_Height);
      Background : constant Unsigned_32 :=
        (if Style.Backdrop = Ocean then 16#FF20_4058#
         elsif Style.Scheme = Alloy_Dark then 16#FF20_282E# else 16#FF54_5D63#);
      Plan : S.Layout;
      Point : Compositor_Sampling.Fine_Sample;
      Q : S.Sample;
      Color : Unsigned_32;
      function Read_Source (X, Y : S.Index) return Unsigned_32 is
        (if Style.Backdrop = Cubie then Cubie_Source (Y * IW + X)
         else Source (Y * IW + X));
   begin
      if W not in 1 .. S.Wide (S.Extent'Last) or else
        H not in 1 .. S.Wide (S.Extent'Last) or else
        Area.Left >= Area.Right or else Area.Top >= Area.Bottom
      then return; end if;
      Plan := S.Prepare (S.Extent (W), S.Extent (H), IW, IH,
        (case Style.Position is when Fill => S.Fill, when Fit => S.Fit, when Center => S.Center));
      for Y in Area.Top .. Area.Bottom - 1 loop
         for X in Area.Left .. Area.Right - 1 loop
            Point := Compositor_Sampling.Fine_Map (Screen, (X, Y), Bounds,
              Compositor_Sampling.Fine_Extent (W * 256), Compositor_Sampling.Fine_Extent (H * 256));
            if Point.Valid then
               Color := Background;
               if Style.Backdrop in Wallpaper | Cubie then
                  Q := S.At_Point (Plan, S.From_Centre (Point.X, S.Extent (W)),
                                        S.From_Centre (Point.Y, S.Extent (H)));
                  if Q.Valid then
                     Color := Blend
                       (Blend (Read_Source (Q.X0, Q.Y0), Read_Source (Q.X1, Q.Y0), Q.FX),
                        Blend (Read_Source (Q.X0, Q.Y1), Read_Source (Q.X1, Q.Y1), Q.FX), Q.FY);
                  end if;
               end if;
               declare
                  Pixel : Unsigned_32 with Import, Address => Target +
                    Storage_Offset (Natural (Y) * Pitch + Natural (X) * 4);
               begin Pixel := Color; end;
            end if;
         end loop;
      end loop;
   end Paint_Output;
end Desktop_Wallpaper;
