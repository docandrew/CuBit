------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Hex_Views is
   use CuBit.UI.Instrument;

   INSET : constant := 10;
   GROUP : constant := 8;
   ADDRESS_CHARS : constant := 14;   --  "0x" and 12 digits
   MARK_H : constant := 2;

   function Bytes_Per_Row (Width : Natural) return Positive is
     (if Width >= (ADDRESS_CHARS + 2 + 3 * WIDE_ROW + 3 + WIDE_ROW) * CHAR_WIDTH + 2 * INSET then WIDE_ROW
      else NARROW_ROW);

   procedure Draw
     (C : Canvas; Area : Rect; Base : Unsigned_64; Bytes, Previous : Byte_Array; Show_Changes : Boolean;
      First, Last : Unsigned_64; V : View; Colors : CuBit.UI.Instrument.Palette)
   is
      P : CuBit.UI.Instrument.Palette renames Colors;
      Per_Row : constant Positive := Bytes_Per_Row (Area.w);
      Rows : constant Natural := Area.h / ROW_HEIGHT;
      Hex_X : constant Integer := Area.x + INSET + (ADDRESS_CHARS + 2) * CHAR_WIDTH;
      Ascii_X : constant Integer := Hex_X + (3 * Per_Row + Per_Row / GROUP) * CHAR_WIDTH + CHAR_WIDTH;
   begin
      for Row in 1 .. Rows loop
         declare
            Start : constant Natural := (V.Top + Row - 1) * Per_Row;
            Y : constant Integer := Area.y + (Row - 1) * ROW_HEIGHT;
            TY : constant Integer := Text_Y (Y, ROW_HEIGHT);
         begin
            exit when Start >= Bytes'Length;
            Mono (C, Area.x + INSET, TY, Address_Image (Base + Unsigned_64 (Start)), P.Muted);
            for K in 0 .. Per_Row - 1 loop
               exit when Start + K >= Bytes'Length;
               declare
                  Off : constant Natural := Start + K;
                  Byte : constant Unsigned_8 := Bytes (Off);
                  Addr : constant Unsigned_64 := Base + Unsigned_64 (Off);
                  BX : constant Integer := Hex_X + (3 * K + K / GROUP) * CHAR_WIDTH;
                  Changed : constant Boolean := Show_Changes and then Previous (Off) /= Byte;
                  In_Sel : constant Boolean := Addr >= First and then Addr <= Last;
                  Cursor : constant Boolean := V.Editing = Editable and then Off = V.Cursor;
                  Ch : constant Character := Character'Val (Byte);
               begin
                  if In_Sel then
                     Fill (C, BX - CHAR_WIDTH / 2, Y + 1, 3 * CHAR_WIDTH, ROW_HEIGHT - 2, P.Selection);
                     Fill (C, Ascii_X + K * CHAR_WIDTH, Y + 1, CHAR_WIDTH, ROW_HEIGHT - 2, P.Selection);
                  end if;
                  if Changed then
                     Fill (C, BX - CHAR_WIDTH / 2, Y + ROW_HEIGHT - MARK_H - 1, 3 * CHAR_WIDTH, MARK_H, P.Changed);
                  end if;
                  if Cursor then
                     Frame (C, BX - CHAR_WIDTH / 2, Y + 1, 3 * CHAR_WIDTH, ROW_HEIGHT - 2, 1, P.Accent);
                  end if;
                  Mono (C, BX, TY, Hex (Unsigned_64 (Byte), 2),
                        (if Changed then P.Changed elsif In_Sel then P.Selection_Text
                         elsif Byte = 0 then P.Dim else P.Text));
                  Mono (C, Ascii_X + K * CHAR_WIDTH, TY, [1 => (if Ch in ' ' .. '~' then Ch else '.')],
                        (if Ch in ' ' .. '~' then P.Muted else P.Dim));
               end;
            end loop;
         end;
      end loop;
   end Draw;

   procedure Scroll (V : in out View; Rows : Integer; Length, Width : Natural) is
      Last_Top : constant Natural := Natural'Max (Rows_Of (Length, Width), 1) - 1;
   begin
      V.Top := Natural (Integer'Max (0, Integer'Min (Integer (Last_Top), Integer (V.Top) + Rows)));
   end Scroll;
end CuBit.UI.Hex_Views;
