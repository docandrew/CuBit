------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Tree table: expandable rows with typed columns, change marks, keyboard
--  navigation and paged children
------------------------------------------------------------------------------
--  The caller keeps its items in tree pre-order (each with a depth) and
--  owns their expansion: the table computes which rows show, draws them,
--  and turns keys and clicks into wishes (expand, collapse, load the next
--  page) that the caller grants or not (it may first have to read the
--  children). Columns are laid out by fractions of the width; the first
--  holds the tree (indentation, disclosure triangle, an optional coloured
--  square for the item's kind). A cell may carry a dim note after its text
--  (an Ada range: "(0 .. 100)"). CuBit.UI.Tree_Tables.Rows draws and
--  navigates a caller model through accessors.
package CuBit.UI.Tree_Tables is
   MAX_ITEMS   : constant := 4_096;
   MAX_COLUMNS : constant := 4;
   subtype Item_Count is Natural range 0 .. MAX_ITEMS;
   subtype Item_Index is Positive range 1 .. MAX_ITEMS;
   subtype Column_Count is Positive range 1 .. MAX_COLUMNS;
   subtype Column_Index is Positive range 1 .. MAX_COLUMNS;
   subtype Percent is Natural range 0 .. 100;

   --  How a cell's text is coloured.
   type Tone is (Normal, Link, Danger, Type_Tone, Kind_Tone, Muted_Tone);

   ROW_HEIGHT : constant := CuBit.UI_Metrics.Line_Height + 2;

   type Visible_Array is array (Item_Index) of Item_Index;
   type Start_Array is array (Column_Index) of Percent;
   type View is record
      Selected : Item_Index := 1;
      Top      : Natural := 0;            --  rows above the first shown
      Visible  : Visible_Array := [others => 1];
      Visible_Total : Item_Count := 0;
      Columns  : Column_Count := 3;
      --  Where each column starts, in per cent of the width (first: 0).
      Starts   : Start_Array := [0, 38, 71, 100];
      --  The column whose cell a change mark fills.
      Marked_Column : Column_Index := 2;
   end record;

   --  The visible row of item I (0: hidden).
   function Position (V : View; I : Item_Index) return Natural;
   --  Scroll so the selection is among the Rows shown.
   procedure Ensure_Visible (V : in out View; Rows : Natural);
   function Rows_In (Area : Rect) return Natural is
     (if Area.h > ROW_HEIGHT then (Area.h - ROW_HEIGHT) / ROW_HEIGHT else 0);

   --  What a key or a click asks of the caller.
   type Wish is (No_Wish, Moved, Expand, Collapse, Toggle, Next_Page);
end CuBit.UI.Tree_Tables;
