------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  CuBit Files' command set and its keymap type (CuBit.UI.Keymaps).
------------------------------------------------------------------------------
with CuBit.UI.Keymaps;

package Files_Keymaps is
   type Command is
     (--  Moving in a pane (the Extend_ forms mark as they go).
      Cursor_Up, Cursor_Down, Page_Up_Rows, Page_Down_Rows, First_Row, Last_Row,
      Extend_Up, Extend_Down, Extend_Page_Up, Extend_Page_Down, Extend_First, Extend_Last,
      Open_Entry, Parent_Folder, Erase_Or_Parent, History_Back, History_Forward, Clear_Or_Cancel,
      --  Panes and tabs.
      Next_Pane, Previous_Pane, Next_Tab, Previous_Tab, Move_Tab_Left, Move_Tab_Right,
      New_Tab, Restore_Tab, Close_Tab, Close_Pane, Add_Pane, Single_Pane,
      Toggle_Left_Pane, Toggle_Right_Pane, Swap_Panes,
      --  Marks.
      Mark_And_Next, Toggle_Mark, Mark_All, Unmark_All, Invert_Marks,
      --  Actions.
      Help, Refresh, View, Edit, Copy, Move, Rename, New_Folder, Delete, Quit,
      Sort_By_Name, Sort_By_Extension, Sort_By_Modified, Sort_By_Size,
      Toggle_Drawer, Bookmark_Folder, Bookmark_Entry, Open_In_Other_Pane, Context_Menu, Columns_Menu,
      Copy_Entry_Names, Copy_Entry_Paths,
      Show_Extension, Show_Size, Show_Modified, Show_Changed, Show_Kind, Show_Mode, Show_Owner,
      Toggle_Overlay, Toggle_Key_Bar,
      --  The quick viewer.
      Close_Viewer, Viewer_Line_Up, Viewer_Line_Down, Viewer_Page_Up, Viewer_Page_Down, Viewer_To_Top,
      Viewer_To_Bottom, Viewer_Text_Or_Hex,
      --  A prompt.
      Prompt_Accept, Prompt_Cancel, Prompt_Erase, Skip_Existing, Overwrite_Existing, Keep_Both, Ask_Each,
      Skip_All, Overwrite_All, Keep_Both_All);

   package Map is new CuBit.UI.Keymaps (Command);
end Files_Keymaps;
