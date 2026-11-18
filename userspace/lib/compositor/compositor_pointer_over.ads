------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The composited pointer's pixels over a frame row (IPC-009): premultiplied
--  ARGB source over the frame, per channel Source + Frame * (255 - Alpha) /
--  255 rounded, clamped at 255, the result opaque-less (alpha bits zero, as
--  Desktop's frames carry). Proved free of run-time errors, so Desktop builds
--  it without checks.
------------------------------------------------------------------------------
with Interfaces;

package Compositor_Pointer_Over with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_32;
   use type Word;
   type Words is array (Natural range <>) of Word;
   --  For each frame pixel of the row, the pointer image column it shows, or
   --  No_Column (the pointer does not cover it).
   No_Column : constant Integer := -1;
   type Column_Map is array (Natural range <>) of Integer;

   function Over (Source, Frame : Word) return Word;

   --  Frame (I) := Over (Source (Map (I)), Frame (I)) wherever Map (I) is
   --  not No_Column.
   procedure Over_Row (Frame : in out Words; Source : Words; Map : Column_Map)
     with Pre => Frame'Length = Map'Length and then Frame'First = 0 and then Map'First = 0 and then
                 Frame'Last < Natural'Last and then
                 (for all I in Map'Range => Map (I) = No_Column or else Map (I) in Source'Range),
          Post => (for all I in Frame'Range =>
                     Frame (I) = (if Map (I) = No_Column then Frame'Old (I)
                                  else Over (Source (Map (I)), Frame'Old (I))));
end Compositor_Pointer_Over;
