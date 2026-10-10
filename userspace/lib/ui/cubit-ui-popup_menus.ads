------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Popup (context) menus: opened at a point, with separators, disabled and
--  checked items, accelerator hints, icons and one level of submenus.
--  Keyboard: Up/Down (skipping separators and disabled items), Right or
--  Enter into a submenu, Left out of it, Enter to choose, Escape to close.
--  Pointer: hover selects, a release on an item chooses it, a press
--  outside closes the menu and is consumed. Placement stays inside the
--  given area (Client_Popup_Layout, proved). Each open level registers one
--  surface control, so a menu costs the Control_Map two entries at most.
--  No allocation, callbacks or IPC.
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with Client_Popup_Layout;

package CuBit.UI.Popup_Menus is
   MAXIMUM_MENUS : constant := 4;           --  the root and its submenus
   MAXIMUM_ITEMS : constant := Client_Popup_Layout.MAXIMUM_ROWS;
   MAXIMUM_CAPTION : constant := 48;
   MAXIMUM_ACCELERATOR : constant := 16;
   subtype Menu_Count is Natural range 0 .. MAXIMUM_MENUS;
   subtype Menu_Index is Menu_Count range 1 .. MAXIMUM_MENUS;
   ROOT : constant Menu_Index := 1;
   subtype Item_Count is Client_Popup_Layout.Row_Count;
   subtype Item_Index is Client_Popup_Layout.Row_Index;
   --  The application's command for an item; 0 is "nothing chosen".
   subtype Command is Natural;
   NO_COMMAND : constant Command := 0;

   type Model is private;
   procedure Clear (M : out Model);
   --  Items are appended to Menu; Add_Submenu also makes a new menu (its
   --  index is returned) for further items.
   procedure Add
     (M : in out Model; Menu : Menu_Index; Caption : String; Choice : Command;
      Accelerator : String := ""; Enabled : Boolean := True; Checked : Boolean := False;
      Has_Icon : Boolean := False; Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.File);
   procedure Add_Separator (M : in out Model; Menu : Menu_Index);
   procedure Add_Submenu (M : in out Model; Menu : Menu_Index; Caption : String; Child : out Menu_Count);
   function Items (M : Model; Menu : Menu_Index) return Item_Count;

   type Popup_State is private;
   function Is_Open (S : Popup_State) return Boolean;
   --  Open at the pointer, kept inside Area (the window).
   procedure Open (S : in out Popup_State; M : Model; X, Y : Natural; Area : Rect);
   procedure Close (S : in out Popup_State);
   --  The area the open menus cover (repaint it when they change or close).
   function Covered (S : Popup_State) return Rect;
   --  The selected item of the deepest open menu (0: none), for tests.
   function Selected (S : Popup_State) return Item_Count;
   function Depth (S : Popup_State) return Menu_Count;

   --  Draw after everything else; registers Base .. Base + MAXIMUM_MENUS - 1.
   ROW_HEIGHT : constant := 24;
   SEPARATOR_HEIGHT : constant := 9;
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; S : in out Popup_State; M : Model; Colors : Theme;
      Base : Controls.Control_ID);

   type Key is (Up, Down, Left, Right, Home, End_Key, Enter, Escape);
   procedure Handle_Key (S : in out Popup_State; M : Model; Pressed : Key; Chosen : out Command;
                         Handled : out Boolean);
   --  While open, route every pointer event here before anything else;
   --  Handled means consumed.
   procedure Handle_Pointer
     (S : in out Popup_State; M : Model; Action : Controls.Pointer_Action; X, Y : Natural;
      Chosen : out Command; Handled : out Boolean);
private
   type Item is record
      Caption : String (1 .. MAXIMUM_CAPTION) := [others => ' '];
      Caption_Length : Natural range 0 .. MAXIMUM_CAPTION := 0;
      Accelerator : String (1 .. MAXIMUM_ACCELERATOR) := [others => ' '];
      Accelerator_Length : Natural range 0 .. MAXIMUM_ACCELERATOR := 0;
      Choice : Command := NO_COMMAND;
      Enabled : Boolean := True;
      Checked : Boolean := False;
      Separator : Boolean := False;
      Submenu : Menu_Count := 0;
      Has_Icon : Boolean := False;
      Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.File;
   end record;
   type Item_Table is array (Item_Index) of Item;
   type Menu_Items is record
      Rows : Item_Table;
      Count : Item_Count := 0;
   end record;
   type Menu_Table is array (Menu_Index) of Menu_Items;
   type Model is record
      Menus : Menu_Table;
      Count : Menu_Count := 1;
   end record;

   type Level is record
      Menu : Menu_Index := ROOT;
      Area : Client_Popup_Layout.Box;
      Selected : Item_Count := 0;
   end record;
   type Level_Table is array (Menu_Index) of Level;
   type Popup_State is record
      Open_Levels : Menu_Count := 0;
      Levels : Level_Table;
      Window : Client_Popup_Layout.Box;
      --  A press inside the menus: its release may choose.
      Pressed_Inside : Boolean := False;
   end record;
end CuBit.UI.Popup_Menus;
