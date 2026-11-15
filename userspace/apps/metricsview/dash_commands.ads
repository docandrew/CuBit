------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Metrics window's commands and its menu bar (MET-002's app): File
--  (new, open, save as, duplicate, the recent dashboards), Edit (undo,
--  redo and the editor's commands), View (zoom, the ranges, live, the CCL
--  and the keys). The menu bar is made from this table each time it is
--  shown, with each item's state, so it cannot drift from the keys.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.UI.Menus;

package Dash_Commands is
   type Command is
     (No_Command,
      New_Dashboard, Open_Dashboard, Save_As, Duplicate_Dashboard,
      Undo, Redo, Toggle_Editor, Add_Panel, Panel_Source, Duplicate_Panel, Remove_Panel, Copy_Source,
      Zoom_In, Zoom_Out, Range_1, Range_2, Range_3, Range_4, Range_5, Range_6, Go_Live, Show_Source, Show_Keys);
   subtype Range_Command is Command range Range_1 .. Range_6;

   --  The recent dashboards: their menu items' commands follow the
   --  table's (Recent_Command (1) is the newest).
   Maximum_Recent : constant := 5;
   subtype Recent_Count is Natural range 0 .. Maximum_Recent;
   subtype Recent_Index is Recent_Count range 1 .. Maximum_Recent;
   function Recent_Command (Index : Recent_Index) return Positive;
   --  A menu item's command back: one of the table's, or a recent one.
   procedure Decode (Item : Natural; Which : out Command; Recent : out Recent_Count);

   type Recent_Captions is array (Recent_Index) of CuBit.UI.Menus.Text;

   --  What the items show: enabled, checked.
   type State is record
      Can_Undo, Can_Redo, Editing, Has_Selection, Can_Add, Live, Source_Shown, Keys_Shown : Boolean := False;
      --  The dashboards folder answered (Open, Save As and recent need it).
      Folder_Ready : Boolean := False;
      --  The range preset shown (0: none).
      Shown_Range : Natural range 0 .. 6 := 0;
      Recent : Recent_Captions := [others => null];
      Recent_Used : Recent_Count := 0;
   end record;

   function Menu_Bar (S : State) return CuBit.UI.Menus.Model;
   --  The narrowest popup that shows every item whole.
   function Popup_Width (Model : CuBit.UI.Menus.Model) return Positive;
end Dash_Commands;
