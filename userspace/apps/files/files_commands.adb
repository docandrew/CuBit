------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Files_Commands is
   use type Keys.Modifier, Keys.Modifier_Set, Keys.Key, Keys.Chord;
   package IC renames CuBit.UI.Icons;
   package Caps renames CuBit.UI.Keycaps;

   function Caption (Item : Command) return String is
     (case Item is
         when Cursor_Up => "Up", when Cursor_Down => "Down", when Page_Up_Rows => "Page up",
         when Page_Down_Rows => "Page down", when First_Row => "First", when Last_Row => "Last",
         when Extend_Up => "Mark up", when Extend_Down => "Mark down", when Extend_Page_Up => "Mark a page up",
         when Extend_Page_Down => "Mark a page down", when Extend_First => "Mark to the first",
         when Extend_Last => "Mark to the last", when Open_Entry => "Open", when Parent_Folder => "Up",
         when Erase_Or_Parent => "Up", when History_Back => "Back", when History_Forward => "Forward",
         when Clear_Or_Cancel => "Clear", when Next_Pane => "Other pane", when Previous_Pane => "Pane before",
         when Next_Tab => "Next tab", when Previous_Tab => "Tab before", when Move_Tab_Left => "Move tab left",
         when Move_Tab_Right => "Move tab right", when New_Tab => "New tab", when Restore_Tab => "Reopen tab",
         when Close_Tab => "Close tab", when Close_Pane => "Close pane", when Add_Pane => "Add pane",
         when Single_Pane => "Single", when Toggle_Left_Pane => "Left", when Toggle_Right_Pane => "Right",
         when Swap_Panes => "Swap", when Mark_And_Next => "Mark", when Toggle_Mark => "Mark",
         when Mark_All => "Mark all", when Unmark_All => "Unmark all", when Invert_Marks => "Invert marks",
         when Help => "Help", when Refresh => "Refresh", when View => "View", when Edit => "Edit",
         when Copy => "Copy", when Move => "Move", when Rename => "Rename", when New_Folder => "Folder",
         when Delete => "Delete", when Quit => "Quit", when Sort_By_Name => "Name",
         when Sort_By_Extension => "Extension", when Sort_By_Modified => "Modified", when Sort_By_Size => "Size",
         when Toggle_Drawer => "Places", when Bookmark_Folder => "Bookmark", when Bookmark_Entry => "Bookmark",
         when Open_In_Other_Pane => "Open there", when Context_Menu => "Menu", when Columns_Menu => "Columns",
         when Copy_Entry_Names => "Copy names", when Copy_Entry_Paths => "Copy paths",
         when Eject_Volume => "Eject", when Eject_Closing_Files => "Eject, closing files", when Show_Extension => "Ext",
         when Show_Size => "Size", when Show_Modified => "Modified", when Show_Changed => "Changed",
         when Show_Kind => "Kind", when Show_Mode => "Mode", when Show_Owner => "Owner",
         when Toggle_Overlay => "Timing", when Toggle_Key_Bar => "Key bar", when Close_Viewer => "Close",
         when Viewer_Line_Up => "Up", when Viewer_Line_Down => "Down", when Viewer_Page_Up => "Page up",
         when Viewer_Page_Down => "Page down", when Viewer_To_Top => "Top", when Viewer_To_Bottom => "Bottom",
         when Viewer_Text_Or_Hex => "Hex", when Prompt_Accept => "OK", when Prompt_Cancel => "Cancel",
         when Prompt_Erase => "Erase", when Skip_Existing => "Skip", when Overwrite_Existing => "Overwrite",
         when Keep_Both => "Keep both", when Ask_Each => "Ask", when Skip_All => "Skip all",
         when Overwrite_All => "Overwrite all", when Keep_Both_All => "Keep both for all");

   function Description (Item : Command) return String is
     (case Item is
         when Help => "Help (not yet)", when Refresh => "Refresh", when View => "View the file",
         when Edit => "Edit (not yet)", when Copy => "Copy to the other pane", when Move => "Move to the other pane",
         when Rename => "Rename", when New_Folder => "New folder", when Delete => "Delete",
         when Quit => "Quit", when Single_Pane => "One pane or all", when History_Back => "Back",
         when History_Forward => "Forward", when Parent_Folder => "Up to the parent folder",
         when Toggle_Drawer => "Places", when Columns_Menu => "Columns", when New_Tab => "New tab",
         when Add_Pane => "Add a pane", when Toggle_Left_Pane => "Show or hide the left pane",
         when Toggle_Right_Pane => "Show or hide the right pane", when Sort_By_Name => "Sort by name",
         when Sort_By_Extension => "Sort by extension", when Sort_By_Modified => "Sort by modified",
         when Sort_By_Size => "Sort by size", when Close_Tab => "Close tab", when Close_Viewer => "Close the viewer",
         when Viewer_Text_Or_Hex => "Text or hex", when Context_Menu => "The menu for the entry",
         when Eject_Volume => "Make the USB stick safe to remove",
         when Eject_Closing_Files => "Close open files on the stick, then eject it",
         when others => Caption (Item));

   function Shade (Item : Command) return Caps.Tone is
     (case Item is
         when Delete => Caps.Danger,
         when Refresh | View | Copy | Move | Rename | New_Folder | Open_Entry | Open_In_Other_Pane
            | Copy_Entry_Names | Copy_Entry_Paths | Prompt_Accept => Caps.Accent,
         when others => Caps.Neutral);

   function Picture (Item : Command) return IC.Icon is
     (case Item is
         when History_Back => IC.Go_Back, when History_Forward => IC.Go_Forward,
         when Parent_Folder | Erase_Or_Parent => IC.Go_Up, when Refresh => IC.Refresh,
         when New_Folder => IC.New_Folder, when Copy => IC.Copy, when Move => IC.Move,
         when Delete => IC.Delete, when Toggle_Drawer => IC.Drawer, when Columns_Menu => IC.Columns,
         when View => IC.Document_File, when New_Tab => IC.New_Tab, when Add_Pane => IC.Add_Pane,
         when Single_Pane => IC.Close, when Rename => IC.Rename, when Open_Entry => IC.Folder,
         when Bookmark_Folder | Bookmark_Entry => IC.Favorites,
         when Eject_Volume | Eject_Closing_Files => IC.Drive,
         when others => IC.File);

   function Panes return Files_Keymaps.Map.Binding_List is
     [(+Keys.Up, Cursor_Up), (+Keys.Down, Cursor_Down), (+Keys.Page_Up, Page_Up_Rows),
      (+Keys.Page_Down, Page_Down_Rows), (+Keys.Home, First_Row), (+Keys.End_Key, Last_Row),
      (Keys.Shift + Keys.Up, Extend_Up), (Keys.Shift + Keys.Down, Extend_Down),
      (Keys.Shift + Keys.Page_Up, Extend_Page_Up), (Keys.Shift + Keys.Page_Down, Extend_Page_Down),
      (Keys.Shift + Keys.Home, Extend_First), (Keys.Shift + Keys.End_Key, Extend_Last),
      (+Keys.Enter, Open_Entry), (+Keys.Backspace, Erase_Or_Parent), (Keys.Alt + Keys.Up, Parent_Folder),
      (Keys.Alt + Keys.Left, History_Back), (Keys.Alt + Keys.Right, History_Forward),
      (+Keys.Escape, Clear_Or_Cancel),
      (+Keys.Tab, Next_Pane), (Keys.Shift + Keys.Tab, Previous_Pane),
      (Keys.Ctrl + Keys.Tab, Next_Tab), (Keys.Ctrl + Keys.Page_Down, Next_Tab),
      (Keys.Ctrl + Keys.Shift + Keys.Tab, Previous_Tab), (Keys.Ctrl + Keys.Page_Up, Previous_Tab),
      (Keys.Ctrl + Keys.Shift + Keys.Page_Up, Move_Tab_Left), (Keys.Ctrl + Keys.Shift + Keys.Page_Down, Move_Tab_Right),
      (Keys.Ctrl + Keys.T, New_Tab), (Keys.Ctrl + Keys.Shift + Keys.T, Restore_Tab),
      (Keys.Ctrl + Keys.W, Close_Tab), (Keys.Ctrl + Keys.Shift + Keys.W, Close_Pane),
      (Keys.Ctrl + Keys.N, Add_Pane), (+Keys.F9, Single_Pane),
      (Keys.Ctrl + Keys.F1, Toggle_Left_Pane), (Keys.Ctrl + Keys.F2, Toggle_Right_Pane),
      (Keys.Ctrl + Keys.U, Swap_Panes),
      (+Keys.Insert, Mark_And_Next), (+Keys.Space, Toggle_Mark), (Keys.Ctrl + Keys.A, Mark_All),
      (Keys.Ctrl + Keys.Shift + Keys.A, Unmark_All), (Keys.Ctrl + Keys.I, Invert_Marks),
      (+Keys.F1, Help), (+Keys.F2, Refresh), (Keys.Ctrl + Keys.R, Refresh), (+Keys.F3, View), (+Keys.F4, Edit),
      (+Keys.F5, Copy), (+Keys.F6, Move), (Keys.Shift + Keys.F6, Rename), (+Keys.F7, New_Folder),
      (+Keys.F8, Delete), (+Keys.Delete, Delete), (+Keys.F10, Quit),
      (Keys.Ctrl + Keys.F3, Sort_By_Name), (Keys.Ctrl + Keys.F4, Sort_By_Extension),
      (Keys.Ctrl + Keys.F5, Sort_By_Modified), (Keys.Ctrl + Keys.F6, Sort_By_Size),
      (Keys.Ctrl + Keys.B, Toggle_Drawer), (Keys.Ctrl + Keys.D, Bookmark_Folder),
      (Keys.Shift + Keys.F10, Context_Menu), (+Keys.Menu, Context_Menu),
      (+Keys.F12, Toggle_Overlay), (Keys.Ctrl + Keys.F12, Toggle_Key_Bar)];

   function Viewer return Files_Keymaps.Map.Binding_List is
     [(+Keys.Escape, Close_Viewer), (+Keys.F3, Close_Viewer), (+Keys.F10, Close_Viewer),
      (+Keys.Enter, Close_Viewer), (+Keys.Up, Viewer_Line_Up), (+Keys.Down, Viewer_Line_Down),
      (+Keys.Page_Up, Viewer_Page_Up), (+Keys.Page_Down, Viewer_Page_Down), (+Keys.Home, Viewer_To_Top),
      (+Keys.End_Key, Viewer_To_Bottom), (+Keys.F4, Viewer_Text_Or_Hex)];

   function Prompt return Files_Keymaps.Map.Binding_List is
     [(+Keys.Enter, Prompt_Accept), (+Keys.Escape, Prompt_Cancel), (+Keys.Backspace, Prompt_Erase),
      (+Keys.S, Skip_Existing), (+Keys.O, Overwrite_Existing), (+Keys.K, Keep_Both), (+Keys.A, Ask_Each),
      (Keys.Shift + Keys.S, Skip_All), (Keys.Shift + Keys.O, Overwrite_All), (Keys.Shift + Keys.K, Keep_Both_All)];
end Files_Commands;
