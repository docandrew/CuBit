------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Code view: read-only source lines with a gutter, line numbers, line
--  markers (current, caller, breakpoints), colouring and annotations
------------------------------------------------------------------------------
--  For showing code someone else owns: a debugger's source, a build log's
--  context, a crash's lines. The Text editor edits; this views. It does not
--  lex: the caller colours each line through Colorize (the Debugger uses
--  the Text editor's proved Text_Syntax lexers, so both apps colour the
--  same way), and may hang an annotation after a line (inline values).
--  CuBit.UI.Code_Views.Lines draws a caller model through accessors.
with CuBit.UI.Instrument;

package CuBit.UI.Code_Views is
   --  What the gutter shows for a line.
   type Line_Marker is (No_Marker, Current_Line, Caller_Line, Breakpoint, Disabled_Breakpoint);
   --  The colour classes a Colorize callback paints with.
   type Token_Style is (Plain, Keyword, Type_Name, Number, Literal, Comment, Attribute, Punctuation);
   function Color_Of (Style : Token_Style; Colors : CuBit.UI.Instrument.Palette) return CuBit.UI.Color;

   ROW_HEIGHT : constant := CuBit.UI_Metrics.Line_Height + 2;
   NUMBER_CHARS : constant := 5;

   type View is record
      Top    : Natural := 0;     --  lines above the first shown
      Follow : Boolean := True;  --  keep the current line in view
   end record;

   function Rows_In (Area : Rect) return Natural is (Area.h / ROW_HEIGHT);
   --  Put Line a third of the way down (when following).
   procedure Show (V : in out View; Line : Natural; Area : Rect);
   procedure Scroll (V : in out View; Lines : Integer; Line_Total : Natural);
   --  Where the code starts (right of the gutter).
   function Code_X (Area : Rect) return Integer;
end CuBit.UI.Code_Views;
