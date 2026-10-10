------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Apps menu's two levels (UI-013): the categories that hold entries
--  (CCL App_Category order), then Power; each category opens a submenu of
--  its entries in key order. Keyboard: Up/Down move among the categories
--  (wrapping; Power takes no keyboard selection, as before), Right or
--  Enter open a category's submenu with its first entry selected, Left or
--  Escape close it, Enter launches, Escape at the top closes the menu.
--  Pointer: a category's submenu opens as soon as the pointer is on it, and
--  Power closes it (user, 2026-10-10: snappy, no rest delay). Pure policy (hosted tests: tests/desktop-launch).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Desktop_Launch;

package Desktop_Launch_Menus is
   subtype Category is Desktop_Launch.Category;
   MAXIMUM_ROWS : constant := Category'Pos (Category'Last) + 1;
   subtype Row_Count is Natural range 0 .. MAXIMUM_ROWS + 1;
   subtype Item_Count is Natural range 0 .. Desktop_Launch.Maximum_Entries;

   type Menu_State is record
      --  The selected top row (Rows + 1 is Power), the open category's
      --  row (0: none), the selected entry in it, and where keys go.
      Row : Row_Count := 1;
      Open : Row_Count := 0;
      Item : Item_Count := 0;
      In_Submenu : Boolean := False;
      --  The row the pointer is on.
      Hover : Row_Count := 0;
   end record;

   --  Categories that hold entries, and the one on a row.
   function Rows (M : Desktop_Launch.Menu) return Row_Count;
   function Power_Row (M : Desktop_Launch.Menu) return Row_Count is (Rows (M) + 1);
   function Category_Of (M : Desktop_Launch.Menu; Row : Row_Count) return Category;
   --  Entries of the category on Row, and the menu index of its Index'th.
   function Items (M : Desktop_Launch.Menu; Row : Row_Count) return Item_Count;
   function Entry_Of (M : Desktop_Launch.Menu; Row : Row_Count; Index : Positive) return Item_Count;
   --  Where an entry is: its row and index in that row's submenu.
   procedure Locate (M : Desktop_Launch.Menu; Entry_Index : Positive; Row : out Row_Count; Index : out Item_Count);
   function Category_Name (Group : Category) return String;

   procedure Reset (S : out Menu_State);
   type Key is (Up, Down, Left, Right, Enter, Escape);
   type Choice is (Nothing, Launch, Power, Close);
   --  Launch: Entry is the menu index to launch.
   procedure Press (S : in out Menu_State; M : Desktop_Launch.Menu; Pressed : Key; Result : out Choice;
                    Entry_Index : out Item_Count);
   --  The pointer on top row Row (0: elsewhere): a category opens at once,
   --  Power closes the open submenu.
   procedure Hover_Row (S : in out Menu_State; M : Desktop_Launch.Menu; Row : Row_Count);
   --  The pointer on the open submenu's Index'th entry.
   procedure Hover_Item (S : in out Menu_State; Index : Item_Count);
   --  A click on top row Row: a category opens at once; Power is chosen.
   procedure Click_Row (S : in out Menu_State; M : Desktop_Launch.Menu; Row : Row_Count; Result : out Choice);
end Desktop_Launch_Menus;
