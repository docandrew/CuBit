------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A status line of key hints (UI-051): "other pane [Tab]  open [Enter]",
--  each hint what a command does and the keycap of its current chord
--  (from the keymap), so a remap shows here too. Text first, keycap after:
--  the one order of menus, buttons and button bars.
--
--  @description
--  The line is an optional text first (a message, the marks), then the
--  hints in priority order, and a quiet text on the right. Too narrow,
--  whole hints drop from the end, then the right text's parts; nothing is
--  cut mid-word (a lone text that cannot fit is elided). A hint whose
--  command has no chord is left out.
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI_Metrics;

package CuBit.UI.Status_Lines is
   --  A line's hints: a named ceiling, more than a status line holds.
   MAXIMUM_HINTS : constant := 16;
   MAXIMUM_HINT_TEXT : constant := 40;
   subtype Hint_Count is Natural range 0 .. MAXIMUM_HINTS;

   HINT_GAP : constant := CuBit.UI_Metrics.Space_L;   --  between hints
   CAP_GAP : constant := CuBit.UI_Metrics.Space_S;  --  text to its keycap

   type Line is private;
   procedure Clear (L : out Line);
   --  A command's hint: its chord's caps and what it does. Not a chord
   --  (the command is unbound): left out.
   procedure Add_Hint (L : in out Line; Item : CuBit.UI.Keys.Chord; Text : String);
   --  A hint without a key ("type to filter").
   procedure Add_Text (L : in out Line; Text : String);
   function Count (L : Line) return Hint_Count;

   --  Draw the status bar: Lead (may be ""), the hints that fit, Right.
   procedure Draw (C : Canvas; R : Rect; Colors : Theme; Lead : String; L : Line; Right : String);
   --  How many of L's hints Draw shows in a bar of R's width (for tests).
   function Shown (R : Rect; Lead : String; L : Line; Right : String) return Hint_Count;
private
   type Hint is record
      Item : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
      Text : String (1 .. MAXIMUM_HINT_TEXT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_HINT_TEXT := 0;
   end record;
   type Hint_Table is array (1 .. MAXIMUM_HINTS) of Hint;
   type Line is record
      Hints : Hint_Table;
      Count : Hint_Count := 0;
   end record;
end CuBit.UI.Status_Lines;
