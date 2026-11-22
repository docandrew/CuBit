------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the source pane as a toolkit code view
------------------------------------------------------------------------------
--  CuBit.UI.Code_Views.Lines over the snapshot's source: the selected
--  frame's line is current, other frames' lines in the same file are
--  callers, the Text editor's lexers (Text_Syntax) colour it, and the
--  current line carries the values of the variables it names.
with CuBit.UI.Code_Views; use CuBit.UI.Code_Views;
with CuBit.UI.Code_Views.Lines;
with Debugger_Model;

package Debugger_Code is
   package DM renames Debugger_Model;

   --  The time change marks are judged at (set before each draw).
   procedure Set_Now (Now : DM.Microseconds);

   --  The selected frame's line in the snapshot's source file (0: none).
   function Current_Line (S : DM.Snapshot) return Natural;
   --  Whether Word appears in Line as a whole identifier (any case).
   function Mentions (Line, Word : String) return Boolean;
   --  The language's name, for the pane's header.
   function Language_Name (S : DM.Snapshot) return String;

   function Line_Total (S : DM.Snapshot) return Natural is (S.Line_Total);
   function Line (S : DM.Snapshot; N : Positive) return String is
     (if N <= S.Line_Total then DM.Source_Line (S, N) else "");
   function Marker (S : DM.Snapshot; N : Positive) return Line_Marker;
   procedure Colorize
     (S : DM.Snapshot; N : Positive; Text : String;
      Paint : not null access procedure (First, Last : Positive; Style : Token_Style));
   function Annotation (S : DM.Snapshot; N : Positive) return String;
   function Annotation_Marked (S : DM.Snapshot; N : Positive) return Boolean;

   package Lines is new CuBit.UI.Code_Views.Lines
     (Model => DM.Snapshot, Line_Total => Line_Total, Line => Line, Marker => Marker, Colorize => Colorize,
      Annotation => Annotation, Annotation_Marked => Annotation_Marked);
end Debugger_Code;
