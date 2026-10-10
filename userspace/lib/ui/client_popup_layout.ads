------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a popup menu goes and which row the keyboard selects next
--  (CuBit.UI.Popup_Menus). Pure policy, proved (tests/ui-popups).
------------------------------------------------------------------------------
package Client_Popup_Layout with SPARK_Mode, Pure is
   --  Logical coordinates up to a large screen's; a box is X, Y, W, H.
   MAXIMUM_COORDINATE : constant := 1_000_000;
   subtype Coordinate is Natural range 0 .. MAXIMUM_COORDINATE;
   type Box is record
      X, Y, W, H : Coordinate := 0;
   end record;
   function Inside (Item, Area : Box) return Boolean is
     (Item.X >= Area.X and then Item.Y >= Area.Y
      and then Item.X - Area.X + Item.W <= Area.W and then Item.Y - Area.Y + Item.H <= Area.H);

   --  A W by H popup opened at the pointer (Anchor_X, Anchor_Y): below and
   --  right of it, flipped left or up where it would leave Area, and
   --  shifted (or, larger than Area, cut) to stay inside.
   function Place (Anchor_X, Anchor_Y : Coordinate; W, H : Coordinate; Area : Box) return Box
     with Post => Inside (Place'Result, Area)
                  and then Place'Result.W = Natural'Min (W, Area.W) and then Place'Result.H = Natural'Min (H, Area.H);
   --  A submenu beside its parent's row: to the parent's right, else its
   --  left, its top at Row_Y where it fits.
   function Place_Beside (Parent : Box; Row_Y : Coordinate; W, H : Coordinate; Area : Box) return Box
     with Post => Inside (Place_Beside'Result, Area);

   MAXIMUM_ROWS : constant := 64;
   subtype Row_Count is Natural range 0 .. MAXIMUM_ROWS;
   subtype Row_Index is Row_Count range 1 .. MAXIMUM_ROWS;
   type Selectable_Rows is array (Row_Index) of Boolean;
   --  The next selectable row after From (Forward) or before it, wrapping
   --  around Count rows; From 0 starts at an end. 0 when none is.
   function Next (Rows : Selectable_Rows; Count : Row_Count; From : Row_Count; Forward : Boolean) return Row_Count
     with Pre => From <= Count,
          Post => Next'Result <= Count and then (if Next'Result > 0 then Rows (Next'Result))
                  and then (if Next'Result = 0 then (for all R in 1 .. Count => not Rows (R)));
end Client_Popup_Layout;
