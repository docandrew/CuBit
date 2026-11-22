------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Instrument is

   LINE_H : constant := CuBit.UI_Metrics.Line_Height;

   function Dec (N : Unsigned_64) return String is
      Img : constant String := Unsigned_64'Image (N);
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Dec;

   HEX_DIGIT : constant String := "0123456789abcdef";
   NIBBLE : constant := 4;
   function Hex (V : Unsigned_64; Digits_Count : Positive) return String is
      R : String (1 .. Digits_Count);
   begin
      for I in R'Range loop
         R (I) := (if (R'Last - I) * NIBBLE >= Unsigned_64'Size then '0'
                   else HEX_DIGIT (Natural (Shift_Right (V, (R'Last - I) * NIBBLE) and 16#F#) + 1));
      end loop;
      return R;
   end Hex;

   SHORT_DIGITS : constant := 8;
   MAX_DIGITS : constant := 16;
   function Address_Image (V : Unsigned_64) return String is
      Needed : Positive := SHORT_DIGITS;
   begin
      while Needed < MAX_DIGITS and then Shift_Right (V, Needed * NIBBLE) /= 0 loop
         Needed := Needed + 1;
      end loop;
      return "0x" & Hex (V, Needed);
   end Address_Image;

   function Fits (S : String; Width : Integer) return String is
     (if Width <= 0 then "" else Fit_Code_Text (S, Width));

   function Text_Y (Row_Y, Row_H : Integer) return Integer is (Row_Y + (Row_H - LINE_H) / 2);

   procedure Fill (C : Canvas; X, Y, W, H : Integer; Col : Color) is
      X0 : constant Integer := Integer'Max (X, 0);
      Y0 : constant Integer := Integer'Max (Y, 0);
      X1 : constant Integer := X + W;
      Y1 : constant Integer := Y + H;
   begin
      if X1 > X0 and then Y1 > Y0 then
         Fill_Rect (C, (X0, Y0, X1 - X0, Y1 - Y0), Col);
      end if;
   end Fill;

   procedure Frame (C : Canvas; X, Y, W, H, T : Integer; Col : Color) is
   begin
      Fill (C, X, Y, W, T, Col);
      Fill (C, X, Y + H - T, W, T, Col);
      Fill (C, X, Y + T, T, H - 2 * T, Col);
      Fill (C, X + W - T, Y + T, T, H - 2 * T, Col);
   end Frame;

   procedure Mono (C : Canvas; X, Y : Integer; S : String; Col : Color) is
      Skip : Natural := 0;
   begin
      if S'Length = 0 or else Y < 0 then
         return;
      end if;
      if X < 0 then
         Skip := (-X + CHAR_WIDTH - 1) / CHAR_WIDTH;
      end if;
      if Skip < S'Length then
         Draw_Code_Text_Transparent (C, X + Skip * CHAR_WIDTH, Y, S (S'First + Skip .. S'Last), Col);
      end if;
   end Mono;

   procedure Sans (C : Canvas; X, Y : Integer; S : String; Col : Color) is
   begin
      if X >= 0 and then Y >= 0 and then S'Length > 0 then
         Draw_UI_Text_Transparent (C, X, Y, S, Col);
      end if;
   end Sans;

   procedure Bold (C : Canvas; X, Y : Integer; S : String; Col : Color) is
   begin
      if X >= 0 and then Y >= 0 and then S'Length > 0 then
         Draw_Emphasis_Text_Transparent (C, X, Y, S, Col);
      end if;
   end Bold;

   procedure Triangle (C : Canvas; X, Y, Size : Integer; Down : Boolean; Col : Color) is
      Half : constant Integer := Size / 2;
   begin
      for I in 0 .. Half loop
         if Down then
            Fill (C, X + I, Y + Half / 2 + I, Size - 2 * I, 1, Col);
         else
            Fill (C, X + Half / 2 + I, Y + I, 1, Size - 2 * I, Col);
         end if;
      end loop;
   end Triangle;
end CuBit.UI.Instrument;
