------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A collapsible, resizable side drawer with a list of sections and
--  shortcuts (places, bookmarks, recent locations). The drawer takes the
--  left of an area; dragging its edge (retained, CuBit.UI.Controls) changes
--  its width between a minimum and a maximum; the application toggles it.
--  The list registers one surface control; Row_At maps a point to a row.
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.Icons;

package CuBit.UI.Drawers is
   EDGE_WIDTH : constant := 5;
   ROW_HEIGHT : constant := 24;
   SECTION_HEIGHT : constant := 26;

   type Drawer_State is record
      Open : Boolean := True;
      Width : Natural := 220;
      Minimum : Natural := 140;
      Maximum : Natural := 480;
   end record;

   --  The drawer's part of Area (empty when closed), the rest, and the
   --  draggable edge between them.
   procedure Layout (S : Drawer_State; Area : Rect; Drawer, Content, Edge : out Rect);

   --  Registers the edge drag (Edge_ID) and applies a retained drag to
   --  S.Width before laying out; call first in a frame.
   procedure Track_Edge
     (S : in out Drawer_State; Map : in out Controls.Control_Map; Area : Rect; Edge_ID : Controls.Control_ID);

   MAXIMUM_ROWS : constant := 64;
   MAXIMUM_CAPTION : constant := 64;
   subtype Row_Count is Natural range 0 .. MAXIMUM_ROWS;
   subtype Row_Index is Row_Count range 1 .. MAXIMUM_ROWS;
   type Row is record
      Section : Boolean := False;
      Caption : String (1 .. MAXIMUM_CAPTION) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_CAPTION := 0;
      Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.Folder;
      --  The application's value for the row (an index into its places).
      Value : Natural := 0;
      Pinned : Boolean := False;
   end record;
   type Row_Table is array (Row_Index) of Row;
   type Shortcut_List is record
      Rows : Row_Table;
      Count : Row_Count := 0;
   end record;
   procedure Clear (L : out Shortcut_List);
   procedure Add_Section (L : in out Shortcut_List; Caption : String);
   procedure Add_Shortcut
     (L : in out Shortcut_List; Caption : String; Picture : CuBit.UI.Icons.Icon; Value : Natural;
      Pinned : Boolean := False);

   --  The row whose top is Top pixels below the list's, for hits.
   function Row_At (L : Shortcut_List; Area : Rect; X, Y : Natural) return Row_Count;
   function Row_Area (L : Shortcut_List; Area : Rect; Index : Row_Index) return Rect;

   --  The drawer: its list (Selected highlighted, Hot under the pointer)
   --  and edge. Registers List_ID over Drawer.
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; Drawer, Edge : Rect; L : Shortcut_List;
      Selected, Hot : Row_Count; Colors : Theme; List_ID : Controls.Control_ID; Edge_Active : Boolean);
end CuBit.UI.Drawers;
