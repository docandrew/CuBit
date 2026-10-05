with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with Desktop_Wallpaper;
with Wallpaper_Assets;
with Compositor_Backdrop_Strips;
procedure Wallpaper_Strips_Tests is
   package A renames CuBit.Appearance;
   use type A.Background, A.Color_Scheme, A.Placement;
   type Pixels is array (Natural range <>) of Unsigned_32 with Convention => C;
   Stride : constant := 264;
   Buffer : aliased Pixels (0 .. Stride * 97 + 1);
   Sentinel : constant Unsigned_32 := 16#A5F0_0D12#;
   Widths : constant array (Positive range <>) of Positive := (1, 63, 64, 65, 127, 128, 129, 257);
   Heights : constant array (Positive range <>) of Positive := (1, 17, 97);
   Cases : Natural := 0;
   function Source (X, Y : Natural; Cubie : Boolean) return Unsigned_32 is
      N : constant Natural := Y * 2048 + X;
   begin
      return 16#FF00_0000# or Shift_Left (Unsigned_32 ((N + (if Cubie then 43 else 0)) mod 251), 16) or
        Shift_Left (Unsigned_32 ((Y * 17) mod 241), 8) or Unsigned_32 ((N * 7) mod 239);
   end Source;
   function Mix (Left, Right : Unsigned_32; F : Natural) return Unsigned_32 is
      Result : Unsigned_32 := 16#FF00_0000#;
      L, R : Natural;
   begin
      for C in 0 .. 2 loop
         L := Natural (Shift_Right (Left, C * 8) and 255);
         R := Natural (Shift_Right (Right, C * 8) and 255);
         Result := Result or Shift_Left (Unsigned_32 ((L * (256 - F) + R * F + 128) / 256), C * 8);
      end loop;
      return Result;
   end Mix;
   -- Independent scalar endpoint oracle, including the original signed centre
   -- rounding. No production sampler or strip cache is used for expected pixels.
   function Reference (W, H, X, Y : Natural; Style : A.Preferences) return Unsigned_32 is
      IW : constant Natural := 2048;
      IH : constant Natural := (if Style.Backdrop = A.Cubie then 1152 else 576);
      DW, DH : Natural;
      LX, LY : Integer;
      SX, SY, X0, X1, Y0, Y1 : Natural;
      Background : constant Unsigned_32 := (if Style.Backdrop = A.Ocean then 16#FF20_4058#
        elsif Style.Scheme = A.Alloy_Dark then 16#FF20_282E# else 16#FF54_5D63#);
   begin
      if Style.Backdrop not in A.Wallpaper | A.Cubie then return Background; end if;
      if Style.Position = A.Center then DW := IW; DH := IH;
      elsif (W * IH >= H * IW) = (Style.Position = A.Fill) then
         DW := W; DH := (W * IH + IW - 1) / IW;
      else DH := H; DW := (H * IW + IH - 1) / IH; end if;
      LX := X - (Integer (W) - Integer (DW)) / 2;
      LY := Y - (Integer (H) - Integer (DH)) / 2;
      if LX < 0 or else LY < 0 or else LX >= DW or else LY >= DH then return Background; end if;
      SX := (if DW = 1 then 0 else Natural (Unsigned_64 (LX) * Unsigned_64 (IW - 1) * 256 / Unsigned_64 (DW - 1)));
      SY := (if DH = 1 then 0 else Natural (Unsigned_64 (LY) * Unsigned_64 (IH - 1) * 256 / Unsigned_64 (DH - 1)));
      X0 := SX / 256; X1 := Natural'Min (X0 + 1, IW - 1);
      Y0 := SY / 256; Y1 := Natural'Min (Y0 + 1, IH - 1);
      return Mix (Mix (Source (X0, Y0, Style.Backdrop = A.Cubie), Source (X1, Y0, Style.Backdrop = A.Cubie), SX mod 256),
                  Mix (Source (X0, Y1, Style.Backdrop = A.Cubie), Source (X1, Y1, Style.Backdrop = A.Cubie), SX mod 256), SY mod 256);
   end Reference;
begin
   for I in Wallpaper_Assets.Wallpaper'Range loop
      Wallpaper_Assets.Wallpaper (I) := Source (I mod 2048, I / 2048, False);
   end loop;
   for I in Wallpaper_Assets.Cubie'Range loop
      Wallpaper_Assets.Cubie (I) := Source (I mod 2048, I / 2048, True);
   end loop;
   for W of Widths loop
      for H of Heights loop
         for Scheme in A.Color_Scheme loop
            for Backdrop in A.Background loop
               for Placement in A.Placement loop
                  for Damage in 0 .. 2 loop
                     declare
                        X : constant Natural := (if Damage = 0 then 0 else W / 2);
                        Y : constant Natural := (if Damage = 0 then 0 else H / 2);
                        CW : constant Natural := (if Damage = 2 then 0 else W - X);
                        CH : constant Natural := H - Y;
                        Style : constant A.Preferences := (Scheme, Backdrop, Placement);
                     begin
                        Buffer := (others => Sentinel);
                        Desktop_Wallpaper.Paint (Buffer (1)'Address, W, H, Stride * 4, X, Y, CW, CH, Style);
                        pragma Assert (Buffer (0) = Sentinel and Buffer (Buffer'Last) = Sentinel);
                        for Row in 0 .. 96 loop
                           for Column in 0 .. Stride - 1 loop
                              if Column >= X and Column < X + CW and Row >= Y and Row < Y + CH then
                                 pragma Assert (Buffer (1 + Row * Stride + Column) = Reference (W, H, Column, Row, Style));
                              else pragma Assert (Buffer (1 + Row * Stride + Column) = Sentinel); end if;
                           end loop;
                        end loop;
                        Cases := Cases + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Buffer := (others => Sentinel);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 65_536, 1, Stride * 4, 0, 0, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 65_536, Stride * 4, 0, 0, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 2, 1, 4, 0, 0, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 2, Natural'Last, 0, 0, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 1, 4, 1, 0, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 1, 4, 0, 1, 1, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 1, 4, 0, 0, 2, 1);
   Desktop_Wallpaper.Paint (Buffer (1)'Address, 1, 1, 4, 0, 0, 1, 2);
   pragma Assert (for all Pixel of Buffer => Pixel = Sentinel);
   Ada.Text_IO.Put_Line ("WALLPAPER STRIPS: PASS" & Cases'Image & " independent scalar/clip/padding cases + 8 invalid-call guards; scratch bytes" &
     Integer'Image (Compositor_Backdrop_Strips.Samples'Object_Size / 8));
end Wallpaper_Strips_Tests;
