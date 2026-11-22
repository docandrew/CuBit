------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CuBit.UI.Hex_Views: row width, scrolling bounds, selection and change
--  marks drawn, read-only draws no cursor.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Hex_Views; use CuBit.UI.Hex_Views;
with CuBit.UI.Instrument;

procedure Hex_Views_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS " else "FAIL ") & What);
      if not Condition then Failures := Failures + 1; end if;
   end Check;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;
   W : constant := 800;
   H : constant := 300;
   type Pixels is array (0 .. W * H - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels'(others => 0);
   C : constant Canvas := (addr => Buffer.all'Address, width => W, height => H, pitch => W * 4, others => <>);
   Bytes, Before : Byte_Array (0 .. 255);
   V : View;
   function Has (Col : Color) return Boolean is (for some X of Buffer.all => X = Col);
begin
   Check (Bytes_Per_Row (900) = WIDE_ROW and then Bytes_Per_Row (300) = NARROW_ROW, "16 bytes a row when wide, else 8");
   for I in Bytes'Range loop
      Bytes (I) := Unsigned_8 (I mod 256);
      Before (I) := Bytes (I);
   end loop;
   Before (5) := 99;
   Draw (C, (0, 0, W, H), 16#4000#, Bytes, Before, True, 16#4010#, 16#4013#, V, P);
   Check (Has (P.Selection), "the selected range is highlighted");
   Check (Has (P.Changed), "a changed byte is marked");
   Check (not Has (P.Accent), "read-only: no cursor");
   Scroll (V, 100, Bytes'Length, W);
   Check (V.Top = Rows_Of (Bytes'Length, W) - 1, "scrolling stops at the last row");
   Scroll (V, -100, Bytes'Length, W);
   Check (V.Top = 0, "and at the first");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   Put_Line ("hex views:" & (if Failures = 0 then " all checks passed" else Natural'Image (Failures) & " FAILED"));
end Hex_Views_Tests;
