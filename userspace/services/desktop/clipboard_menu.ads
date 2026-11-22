------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Keyboard model of the clipboard manager popup (Super+V)
--
--  Rows are the history newest first (Clipboard_Store.Older order), rebuilt
--  by Desktop whenever the history changes. The selection never leaves the
--  rows; an empty history has no selection. Keys, Console style:
--    Up/Down, Home/End, 1..9  move (digits paste that row at once)
--    Enter                    paste the selected row into the focused app
--    P                        pin or unpin (secrets cannot be pinned)
--    Delete                   delete the row; Shift+Delete clears unpinned
--    Escape, Super+V          close
------------------------------------------------------------------------------
with Clipboard_Store;

package Clipboard_Menu with SPARK_Mode is
   use type Clipboard_Store.Slot_Count;

   subtype Row_Count is Clipboard_Store.Slot_Count;
   subtype Row_Index is Row_Count range 1 .. Row_Count'Last;
   type Row_Slots is array (Row_Index) of Clipboard_Store.Slot_Count;

   --  Rows visible at once: the popup scrolls past this many.
   Visible_Rows : constant := 9;
   subtype Visible_Count is Row_Count range 0 .. Visible_Rows;

   type Menu_State is record
      Count    : Row_Count := 0;
      Slots    : Row_Slots := [others => 0];
      Selected : Row_Count := 0;
      First    : Row_Index := 1;   --  first visible row
   end record;

   function Valid (M : Menu_State) return Boolean is
     ((if M.Count = 0 then M.Selected = 0 else M.Selected in 1 .. M.Count)
      and then M.First <= Row_Index'Max (1, M.Count)
      and then (for all R in Row_Index =>
                  (if R <= M.Count then M.Slots (R) /= 0))
      and then (if M.Selected /= 0 then
                  M.Selected >= M.First and then
                  M.Selected < M.First + Visible_Rows));

   type Key is
     (Up, Down, Home, Last, Enter, Pin, Delete, Clear, Escape, Number);
   subtype Digit_Value is Row_Count range 1 .. 9;

   type Action is (Nothing, Close, Paste, Toggle_Pin, Remove, Remove_All);

   --  Install a fresh row list, keeping the selection on the same slot when
   --  it is still listed, else the nearest row.
   procedure Reload (M : in out Menu_State; Slots : Row_Slots; Count : Row_Count)
     with Pre => (for all R in Row_Index => (if R <= Count then Slots (R) /= 0)),
          Post => Valid (M) and then M.Count = Count;

   --  The row a click or a digit picked, if any.
   procedure Press
     (M : in out Menu_State; K : Key; Digit : Digit_Value;
      Result : out Action; Slot : out Clipboard_Store.Slot_Count)
     with Pre => Valid (M),
          Post => Valid (M) and then M.Count = M.Count'Old and then
            (if Result in Paste | Toggle_Pin | Remove then
               Slot /= 0 and then M.Selected /= 0 and then
               Slot = M.Slots (M.Selected)
             else Slot = 0);

   --  Pointer selection of a visible row (Offset 0 is the first shown).
   procedure Select_Visible
     (M : in out Menu_State; Offset : Natural; Hit : out Boolean)
     with Pre => Valid (M),
          Post => Valid (M) and then M.Count = M.Count'Old;
end Clipboard_Menu;
