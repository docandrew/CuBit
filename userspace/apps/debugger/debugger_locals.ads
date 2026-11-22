------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the typed locals as a toolkit tree table
------------------------------------------------------------------------------
--  CuBit.UI.Tree_Tables.Rows over the snapshot's locals (tree pre-order):
--  Name, Value with its Ada range, Type; a square per value kind; values
--  that moved since the last stop marked.
with CuBit.UI;
with CuBit.UI.Tree_Tables; use CuBit.UI.Tree_Tables;
with CuBit.UI.Tree_Tables.Rows;
with Debugger_Model;

package Debugger_Locals is
   package DM renames Debugger_Model;

   procedure Set_Now (Now : DM.Microseconds);

   function Item_Total (S : DM.Snapshot) return Item_Count is (S.Local_Total);
   function Depth (S : DM.Snapshot; I : Item_Index) return Natural is (S.Locals (I).Depth);
   function Expandable (S : DM.Snapshot; I : Item_Index) return Boolean is
     (S.Locals (I).Child_Count > 0 or else (DM.Kind_Has_Children (S.Locals (I).Kind) and then not S.Locals (I).Loaded)
      or else (S.Locals (I).Loaded and then I < S.Local_Total and then S.Locals (I + 1).Depth > S.Locals (I).Depth));
   function Expanded (S : DM.Snapshot; I : Item_Index) return Boolean is
     (S.Locals (I).Expanded and then S.Locals (I).Loaded);
   function Is_Pager (S : DM.Snapshot; I : Item_Index) return Boolean is (False);
   function Cell (S : DM.Snapshot; I : Item_Index; Column : Column_Index) return String;
   function Cell_Note (S : DM.Snapshot; I : Item_Index; Column : Column_Index) return String is
     (if Column = 2 and then S.Locals (I).Bounds.Length > 0 then "(" & DM.Image (S.Locals (I).Bounds) & ")" else "");
   function Cell_Tone (S : DM.Snapshot; I : Item_Index; Column : Column_Index) return Tone;
   function Square (S : DM.Snapshot; I : Item_Index; Has_Square : out Boolean) return CuBit.UI.Color;
   function Marked (S : DM.Snapshot; I : Item_Index) return Boolean;

   package Table is new CuBit.UI.Tree_Tables.Rows
     (Model => DM.Snapshot, Item_Total => Item_Total, Depth => Depth, Expandable => Expandable,
      Expanded => Expanded, Is_Pager => Is_Pager, Cell => Cell, Cell_Note => Cell_Note, Cell_Tone => Cell_Tone,
      Square => Square, Marked => Marked);
end Debugger_Locals;
