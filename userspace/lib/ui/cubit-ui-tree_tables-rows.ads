------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Tree table over a caller's items (CuBit.UI.Tree_Tables)
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI.Instrument;

generic
   type Model (<>) is limited private;
   with function Item_Total (M : Model) return Item_Count;
   with function Depth (M : Model; I : Item_Index) return Natural;
   --  It has children (loaded or not), and whether they show.
   with function Expandable (M : Model; I : Item_Index) return Boolean;
   with function Expanded (M : Model; I : Item_Index) return Boolean;
   --  A row standing for more children not yet read ("+64 more").
   with function Is_Pager (M : Model; I : Item_Index) return Boolean;
   with function Cell (M : Model; I : Item_Index; Column : Column_Index) return String;
   with function Cell_Note (M : Model; I : Item_Index; Column : Column_Index) return String;
   with function Cell_Tone (M : Model; I : Item_Index; Column : Column_Index) return Tone;
   --  The kind square before the first cell; Has_Square False: none.
   with function Square (M : Model; I : Item_Index; Has_Square : out Boolean) return CuBit.UI.Color;
   with function Marked (M : Model; I : Item_Index) return Boolean;
package CuBit.UI.Tree_Tables.Rows is
   --  Which items show: those whose ancestors are all expanded.
   procedure Refresh (V : in out View; M : Model);
   procedure Draw
     (C : Canvas; Area : Rect; M : Model; V : View; Headings : String; Focused : Boolean;
      Colors : CuBit.UI.Instrument.Palette);
   --  Up, Down, Home, End, Page Up/Down move (Moved); Right expands or
   --  steps into the children; Left collapses or steps to the parent;
   --  Enter and Space toggle (on a pager: Next_Page). Item: the wish's item.
   procedure Key (V : in out View; M : Model; Area : Rect; Code : Interfaces.Unsigned_64;
                  Result : out Wish; Item : out Item_Index);
   --  A click selects (Moved); on a disclosure triangle it toggles.
   procedure Click (V : in out View; M : Model; Area : Rect; X, Y : Natural; Result : out Wish; Item : out Item_Index);
   procedure Wheel (V : in out View; Steps : Integer);
end CuBit.UI.Tree_Tables.Rows;
