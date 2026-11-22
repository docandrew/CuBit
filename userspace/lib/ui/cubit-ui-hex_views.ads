------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hex view: an address column, bytes in hex, and their ASCII
------------------------------------------------------------------------------
--  A window of bytes at a base address, 16 to a row when the area is wide
--  enough, else 8. Zero bytes are dim (structure shows), bytes that changed
--  since the previous window are marked, and an address range (a selected
--  variable) is highlighted in both columns.
--
--  Editable mode is planned, not built: a cursor on a byte, nibble entry,
--  and the edit handed to the caller to write. Read_Only draws no cursor.
with Interfaces;
with CuBit.UI.Instrument;

package CuBit.UI.Hex_Views is
   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Mode is (Read_Only, Editable);

   WIDE_ROW   : constant := 16;
   NARROW_ROW : constant := 8;
   ROW_HEIGHT : constant := CuBit.UI_Metrics.Line_Height + 2;

   type View is record
      Top     : Natural := 0;       --  rows above the first shown
      Editing : Mode := Read_Only;
      Cursor  : Natural := 0;       --  (Editable) the byte under the cursor
   end record;

   function Bytes_Per_Row (Width : Natural) return Positive;
   function Rows_Of (Length : Natural; Width : Natural) return Natural is
     ((Length + Bytes_Per_Row (Width) - 1) / Bytes_Per_Row (Width));

   --  Bytes (from index 0) at Base; Previous: the window before (same base
   --  and length), compared when Show_Changes. Bytes in First .. Last
   --  (addresses; Last < First for none) are highlighted.
   procedure Draw
     (C : Canvas; Area : Rect; Base : Unsigned_64; Bytes, Previous : Byte_Array; Show_Changes : Boolean;
      First, Last : Unsigned_64; V : View; Colors : CuBit.UI.Instrument.Palette)
   with Pre => Bytes'First = 0 and then Previous'First = 0 and then Previous'Length >= Bytes'Length;

   --  Scroll by Rows (negative: up), kept within the window.
   procedure Scroll (V : in out View; Rows : Integer; Length, Width : Natural);
end CuBit.UI.Hex_Views;
