------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The line the guest is writing on its serial console, as a terminal shows
--  it (VMM-001): a cursor that CR, BS and the shell's cursor sequences
--  move, so echoed keys, backspaces ("\b \b") and line redraws look as they
--  do in a terminal, and escape sequences are taken rather than printed.
--
--  @description
--  One line, no screen: LF completes it (the caller takes Text, then
--  Clear), as does writing past its last column (a wrap). Understood:
--  printable bytes (written at the cursor, overwriting), CR (column 0), BS
--  (one left), HT (to the next multiple of eight, as spaces), and the CSI
--  sequences ESC [ n C (right), ESC [ n D (left), ESC [ n G (column n) and
--  ESC [ K, ESC [ 0 K (erase to the end), ESC [ 2 K (the whole line), and
--  ESC [ J likewise within the line. Every other CSI sequence (colours,
--  ESC [ 6 n ...), OSC sequence
--  (ESC ] ... BEL or ESC \) and two-byte ESC sequence is taken and ignored,
--  as are the other control bytes. Bytes above 7Eh are kept as they are
--  (the window's font draws them as CP437).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Console_Line with SPARK_Mode, Pure is

   Width : constant := 200;
   subtype Column is Natural range 0 .. Width;
   subtype Text_Index is Positive range 1 .. Width;
   --  The parameter of a CSI sequence, saturating.
   Parameter_Ceiling : constant := 9_999;
   subtype Parameter is Natural range 0 .. Parameter_Ceiling;

   type Escape_State is (Ground, After_Escape, Control_Sequence, Operating_System_Command, OSC_Escape);

   type Line is record
      Text   : String (Text_Index) := [others => ' '];
      Length : Column := 0;         --  columns holding text
      Cursor : Column := 0;         --  the next column written (0-based)
      State  : Escape_State := Ground;
      Value  : Parameter := 0;      --  the CSI sequence's first parameter
      Has_Value : Boolean := False;
      In_Second : Boolean := False; --  past a ';' (later parameters are ignored)
   end record;

   --  The guest's byte B. Done: the line is complete (LF, or a wrap): the
   --  caller takes Text (1 .. Length) and calls Clear.
   procedure Put (L : in out Line; B : Unsigned_8; Done : out Boolean)
     with Pre  => L.Cursor < Width,
          Post => (if not Done then L.Cursor < Width);

   --  An empty line, the cursor at column 0, the escape state kept.
   procedure Clear (L : in out Line)
     with Post => L.Length = 0 and then L.Cursor = 0;
end Console_Line;
