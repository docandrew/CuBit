------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CuBit.UI.Tree_Tables: visible rows follow expansion, keys and clicks
--  turn into wishes, pagers ask for the next page, marks and notes draw.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Tree_Tables; use CuBit.UI.Tree_Tables;
with CuBit.UI.Tree_Tables.Rows;
with CuBit.UI.Instrument;

procedure Tree_Tables_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS " else "FAIL ") & What);
      if not Condition then Failures := Failures + 1; end if;
   end Check;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;

   --  A record with two fields, then a scalar, then a pager.
   type Item is record
      Depth : Natural := 0;
      Open, Can_Open, Pager, Hot : Boolean := False;
   end record;
   type Items is array (1 .. 5) of Item;
   type Model is record
      I : Items := [(0, True, True, False, False), (1, False, False, False, True), (1, False, False, False, False),
                    (0, False, False, False, False), (0, False, False, True, False)];
   end record;
   function Item_Total (M : Model) return Item_Count is (M.I'Length);
   function Depth (M : Model; I : Item_Index) return Natural is (M.I (I).Depth);
   function Expandable (M : Model; I : Item_Index) return Boolean is (M.I (I).Can_Open);
   function Expanded (M : Model; I : Item_Index) return Boolean is (M.I (I).Open);
   function Is_Pager (M : Model; I : Item_Index) return Boolean is (M.I (I).Pager);
   function Cell (M : Model; I : Item_Index; Column : Column_Index) return String is
     ((if Column = 1 then "item" else "value") & Natural'Image (I));
   function Cell_Note (M : Model; I : Item_Index; Column : Column_Index) return String is
     (if Column = 2 then "(0 .. 9)" else "");
   function Cell_Tone (M : Model; I : Item_Index; Column : Column_Index) return Tone is
     (if Column = 3 then Type_Tone else Normal);
   function Square (M : Model; I : Item_Index; Has_Square : out Boolean) return Color is
   begin
      Has_Square := True;
      return P.Kind_Record;
   end Square;
   function Marked (M : Model; I : Item_Index) return Boolean is (M.I (I).Hot);
   package Table is new CuBit.UI.Tree_Tables.Rows
     (Model, Item_Total, Depth, Expandable, Expanded, Is_Pager, Cell, Cell_Note, Cell_Tone, Square, Marked);

   KEY_DOWN  : constant := 16#50#;
   KEY_LEFT  : constant := 16#4B#;
   KEY_ENTER : constant := 16#1C#;
   W : constant := 600;
   H : constant := 200;
   type Pixels is array (0 .. W * H - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels'(others => 0);
   C : constant Canvas := (addr => Buffer.all'Address, width => W, height => H, pitch => W * 4, others => <>);
   Area : constant Rect := (0, 0, W, H);
   M : Model;
   V : View;
   Result : Wish;
   Which : Item_Index;
   function Has (Col : Color) return Boolean is (for some X of Buffer.all => X = Col);
begin
   Table.Refresh (V, M);
   Check (V.Visible_Total = 5, "an open record shows its fields");
   M.I (1).Open := False;
   Table.Refresh (V, M);
   Check (V.Visible_Total = 3 and then V.Visible (2) = 4, "a closed one hides them");
   M.I (1).Open := True;
   Table.Refresh (V, M);
   Table.Key (V, M, Area, KEY_DOWN, Result, Which);
   Check (Result = Moved and then V.Selected = 2, "Down moves");
   Table.Key (V, M, Area, KEY_LEFT, Result, Which);
   Check (Result = Moved and then V.Selected = 1, "Left on a field goes to its record");
   Table.Key (V, M, Area, KEY_LEFT, Result, Which);
   Check (Result = Collapse and then Which = 1, "Left on an open record asks to collapse it");
   V.Selected := 5;
   Table.Key (V, M, Area, KEY_ENTER, Result, Which);
   Check (Result = Next_Page and then Which = 5, "Enter on a pager asks for the next page");
   Table.Click (V, M, Area, 12, ROW_HEIGHT + ROW_HEIGHT / 2, Result, Which);
   Check (Result = Toggle and then Which = 1, "a click on the triangle toggles");
   Table.Draw (C, Area, M, V, "Name|Value|Type", True, P);
   Check (Has (P.Changed_Fill) and then Has (P.Kind_Record) and then Has (P.Selection), "marks, squares, selection");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   Put_Line ("tree tables:" & (if Failures = 0 then " all checks passed" else Natural'Image (Failures) & " FAILED"));
end Tree_Tables_Tests;
