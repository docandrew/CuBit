------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's stroke font: capitals, digits and a little punctuation as
--  line segments on a 4 x 6 grid, drawn as glowing neon tubes like
--  everything else on screen. Original glyphs, made for this game.
------------------------------------------------------------------------------
package Neon_Font with SPARK_Mode, Pure is

   Grid_Width : constant := 4;
   Grid_Height : constant := 6;
   --  Glyph cells advance this many grid units (glyph plus a gap).
   Advance : constant := 6;

   --  A segment from (X0, Y0) to (X1, Y1) in grid units, Y downward.
   type Segment is record
      X0, Y0, X1, Y1 : Natural := 0;
   end record;

   Maximum_Segments : constant := 16;
   subtype Segment_Count is Natural range 0 .. Maximum_Segments;
   type Segment_Array is array (1 .. Maximum_Segments) of Segment;
   type Glyph is record
      Count : Segment_Count := 0;
      Segments : Segment_Array;
   end record;

   function Shape (Letter : Character) return Glyph;
   --  Width of Text in grid units (Advance per character, the last gap
   --  dropped).
   function Width (Text : String) return Natural is
     (if Text'Length = 0 then 0 else Text'Length * Advance - (Advance - Grid_Width))
     with Pre => Text'Length <= 1_000;

end Neon_Font;
