------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Instrument views: the shared palette and drawing primitives of the
--  toolkit's data views (graphs, tree tables, hex, code, timelines)
------------------------------------------------------------------------------
--  The look of the stream graph inspector and the Debugger: square boxes,
--  flat fills, no glow, raw data. The primitives take Integer coordinates
--  and clip what falls left of or above the canvas, so a view that pans can
--  draw partly visible boxes and text without checks of its own.
with Interfaces;
with CuBit.UI_Metrics;

package CuBit.UI.Instrument is
   subtype Color is CuBit.UI.Theme_Color;

   type Palette is record
      Workspace, Pane, Pane_Alt, Header, Header_Focus, Rule, Rule_Focus : Color;
      Text, Muted, Dim, Accent, Focus_Mark : Color;
      Selection, Selection_Text, Current_Line, Current_Mark, Caller_Line : Color;
      Changed, Changed_Fill, Danger, Good, Warn : Color;
      --  Syntax.
      Keyword, Comment, Literal, Number, Attribute, Type_Name, Punctuation : Color;
      --  Graph.
      Grid, Node_Fill, Node_Edge, Node_Selected, Record_Head, Array_Head, Pointer_Head,
      Enum_Head, Edge, Back_Edge, Port : Color;
      --  Value kinds (the small square before a local).
      Kind_Scalar, Kind_Enum, Kind_Record, Kind_Array, Kind_Pointer : Color;
      --  The replay strip, which is not built yet.
      Hatch, Hatch_Alt : Color;
   end record;

   Instrument : constant Palette :=
     (Workspace      => 16#0B0F14#,
      Pane           => 16#11171E#,
      Pane_Alt       => 16#141B23#,
      Header         => 16#18202A#,
      Header_Focus   => 16#1C2836#,
      Rule           => 16#25303C#,
      Rule_Focus     => 16#3FB6DC#,
      Text           => 16#D8E0E8#,
      Muted          => 16#8394A5#,
      Dim            => 16#4E5D6C#,
      Accent         => 16#3FB6DC#,
      Focus_Mark     => 16#3FB6DC#,
      Selection      => 16#1F4560#,
      Selection_Text => 16#FFFFFF#,
      Current_Line   => 16#233A2E#,
      Current_Mark   => 16#FFB84D#,
      Caller_Line    => 16#1A2430#,
      Changed        => 16#FFB84D#,
      Changed_Fill   => 16#3A2C14#,
      Danger         => 16#FF6B6B#,
      Good           => 16#5FD38D#,
      Warn           => 16#FFB84D#,
      Keyword        => 16#B794F6#,
      Comment        => 16#5F7887#,
      Literal        => 16#9CD67C#,
      Number         => 16#F5A97F#,
      Attribute      => 16#5ED6C8#,
      Type_Name      => 16#7CC4FA#,
      Punctuation    => 16#93A1B0#,
      Grid           => 16#18212B#,
      Node_Fill      => 16#141C25#,
      Node_Edge      => 16#33414F#,
      Node_Selected  => 16#3FB6DC#,
      Record_Head    => 16#2B3A63#,
      Array_Head     => 16#1F4D49#,
      Pointer_Head   => 16#23465E#,
      Enum_Head      => 16#47305E#,
      Edge           => 16#4FA8D8#,
      Back_Edge      => 16#C78BF0#,
      Port           => 16#4FA8D8#,
      Kind_Scalar    => 16#7C8A99#,
      Kind_Enum      => 16#B794F6#,
      Kind_Record    => 16#6E8CF0#,
      Kind_Array     => 16#4CC2B0#,
      Kind_Pointer   => 16#4FA8D8#,
      Hatch          => 16#151C24#,
      Hatch_Alt      => 16#1A222C#);

   ---------------------------------------------------------------------------
   --  Text.
   function Dec (N : Interfaces.Unsigned_64) return String;
   function Hex (V : Interfaces.Unsigned_64; Digits_Count : Positive) return String;
   --  "0x" and at least 8 hex digits, as many as V needs (up to 16).
   function Address_Image (V : Interfaces.Unsigned_64) return String;
   --  S cut with "..." to Width logical pixels of monospace text.
   function Fits (S : String; Width : Integer) return String;
   --  The top of a line of text centred in a row at Row_Y, Row_H high.
   function Text_Y (Row_Y, Row_H : Integer) return Integer;
   CHAR_WIDTH : constant := CuBit.UI_Metrics.Mono_Advance;

   ---------------------------------------------------------------------------
   --  Drawing on Integer coordinates.
   procedure Fill (C : Canvas; X, Y, W, H : Integer; Col : Color);
   --  A T-pixel outline.
   procedure Frame (C : Canvas; X, Y, W, H, T : Integer; Col : Color);
   --  Monospace text at X; characters left of the canvas are dropped.
   procedure Mono (C : Canvas; X, Y : Integer; S : String; Col : Color);
   procedure Sans (C : Canvas; X, Y : Integer; S : String; Col : Color);
   procedure Bold (C : Canvas; X, Y : Integer; S : String; Col : Color);
   --  A disclosure triangle: right-pointing, or down-pointing (Down).
   procedure Triangle (C : Canvas; X, Y, Size : Integer; Down : Boolean; Col : Color);
   function Intersects (A, B : Rect) return Boolean is
     (A.x < B.x + B.w and then B.x < A.x + A.w and then A.y < B.y + B.h and then B.y < A.y + A.h);
end CuBit.UI.Instrument;
