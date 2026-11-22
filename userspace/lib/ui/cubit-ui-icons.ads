------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Bluecurve stock icons at any density: each is held at 16 and 32 pixels
--  (tools/generate_icon_atlas.py) and drawn from the one that suits the
--  canvas, so icons stay sharp at 2x.
------------------------------------------------------------------------------
package CuBit.UI.Icons is
   type Icon is
     (Go_Back, Go_Forward, Go_Up, Refresh, New_Folder, Copy, Move, Delete, Search, Bookmarks, Home, Drawer,
      New_Tab, Columns, Properties, Add_Pane, Close, Paste, Rename, History, Folder, File, Drive, Image_File,
      Document_File, Archive_File, Program_File, Trash, Network, Recent, Favorites, Home_Folder);

   --  An icon's logical size.
   ICON_SIZE : constant := 16;

   procedure Draw (c : Canvas; x, y : Natural; Item : Icon; Enabled : Boolean := True);

   --  A flat toolbar button: the icon alone, a raised frame when hot,
   --  sunken when pressed or toggled on (Checked).
   procedure Draw_Tool_Button
     (c : Canvas; Bounds : Rect; Colors : Theme; Item : Icon; Enabled, Hot, Pressed : Boolean;
      Checked : Boolean := False);
end CuBit.UI.Icons;
