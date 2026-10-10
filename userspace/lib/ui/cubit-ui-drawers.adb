package body CuBit.UI.Drawers is
   TEXT_INSET : constant := 8;
   ICON_GAP : constant := 6;

   procedure Layout (S : Drawer_State; Area : Rect; Drawer, Content, Edge : out Rect) is
      Width : constant Natural :=
        (if not S.Open or else Area.w <= EDGE_WIDTH then 0
         else Natural'Min (Natural'Max (S.Width, S.Minimum), Natural'Min (S.Maximum, Area.w - EDGE_WIDTH)));
   begin
      if Width = 0 then
         Drawer := (Area.x, Area.y, 0, Area.h);
         Edge := (Area.x, Area.y, 0, Area.h);
         Content := Area;
         return;
      end if;
      Drawer := (Area.x, Area.y, Width, Area.h);
      Edge := (Area.x + Width, Area.y, EDGE_WIDTH, Area.h);
      Content := (Area.x + Width + EDGE_WIDTH, Area.y, Area.w - Width - EDGE_WIDTH, Area.h);
   end Layout;

   procedure Track_Edge
     (S : in out Drawer_State; Map : in out Controls.Control_Map; Area : Rect; Edge_ID : Controls.Control_ID)
   is
      Drawer, Content, Edge : Rect;
      Value : Natural;
      Available : Boolean;
      Maximum : constant Natural :=
        Natural'Max (S.Minimum, Natural'Min (S.Maximum, (if Area.w > EDGE_WIDTH then Area.w - EDGE_WIDTH else 0)));
   begin
      Layout (S, Area, Drawer, Content, Edge);
      if Is_Empty (Edge) then
         return;
      end if;
      --  The edge's left is where the width ends: dragging it sets width.
      Controls.Add_Horizontal_Drag
        (Map, Edge_ID, Edge, Area, Drawer.w, S.Minimum, Maximum, Area.x);
      Controls.Take_Value (Map, Edge_ID, Value, Available);
      if Available then
         S.Width := Natural'Min (Natural'Max (Value, S.Minimum), Maximum);
      end if;
   end Track_Edge;

   procedure Clear (L : out Shortcut_List) is
   begin
      L := (others => <>);
   end Clear;

   procedure Append (L : in out Shortcut_List; Item : Row) is
   begin
      if L.Count < MAXIMUM_ROWS then
         L.Count := L.Count + 1;
         L.Rows (L.Count) := Item;
      end if;
   end Append;

   function Make (Caption : String) return Row is
      Result : Row;
   begin
      Result.Length := Natural'Min (Caption'Length, MAXIMUM_CAPTION);
      Result.Caption (1 .. Result.Length) := Caption (Caption'First .. Caption'First + Result.Length - 1);
      return Result;
   end Make;

   procedure Add_Section (L : in out Shortcut_List; Caption : String) is
      Item : Row := Make (Caption);
   begin
      Item.Section := True;
      Append (L, Item);
   end Add_Section;

   procedure Add_Shortcut
     (L : in out Shortcut_List; Caption : String; Picture : CuBit.UI.Icons.Icon; Value : Natural;
      Pinned : Boolean := False)
   is
      Item : Row := Make (Caption);
   begin
      Item.Picture := Picture;
      Item.Value := Value;
      Item.Pinned := Pinned;
      Append (L, Item);
   end Add_Shortcut;

   function Height (Item : Row) return Natural is (if Item.Section then SECTION_HEIGHT else ROW_HEIGHT);

   function Row_Area (L : Shortcut_List; Area : Rect; Index : Row_Index) return Rect is
      Y : Natural := Area.y;
   begin
      for K in 1 .. Natural'Min (Index, L.Count) - 1 loop
         Y := Y + Height (L.Rows (K));
      end loop;
      return (if Index > L.Count then (others => 0) else (Area.x, Y, Area.w, Height (L.Rows (Index))));
   end Row_Area;

   function Row_At (L : Shortcut_List; Area : Rect; X, Y : Natural) return Row_Count is
      Top : Natural := Area.y;
   begin
      if not Point_In_Rect (X, Y, Area) then
         return 0;
      end if;
      for K in 1 .. L.Count loop
         if Y >= Top and then Y < Top + Height (L.Rows (K)) then
            return (if L.Rows (K).Section then 0 else K);
         end if;
         Top := Top + Height (L.Rows (K));
      end loop;
      return 0;
   end Row_At;

   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; Drawer, Edge : Rect; L : Shortcut_List;
      Selected, Hot : Row_Count; Colors : Theme; List_ID : Controls.Control_ID; Edge_Active : Boolean)
   is
      Clipped : constant Canvas := With_Clip (C, Drawer);
      Below : Natural := Drawer.y;
   begin
      if Is_Empty (Drawer) then
         return;
      end if;
      Controls.Add_Surface (Map, List_ID, Drawer);
      for K in 1 .. L.Count loop
         declare
            Item : Row renames L.Rows (K);
            Box : constant Rect := Row_Area (L, Drawer, K);
            Chosen : constant Boolean := K = Selected;
            Back : constant Color :=
              (if Item.Section then Colors.panel elsif Chosen then Colors.selection
               elsif K = Hot then Colors.face else Colors.panel);
            Ink : constant Color :=
              (if Item.Section then Colors.muted elsif Chosen then Colors.selectionText else Colors.text);
         begin
            Fill_Rect (Clipped, Box, Back);
            if Item.Section then
               Draw_UI_Text_Transparent
                 (Clipped, Box.x + TEXT_INSET, Box.y + (SECTION_HEIGHT - UI_Text_Height) - 3,
                  Item.Caption (1 .. Item.Length), Ink);
               Fill_Rect (Clipped, (Box.x + TEXT_INSET, Box.y + Box.h - 1,
                                    (if Box.w > 2 * TEXT_INSET then Box.w - 2 * TEXT_INSET else 0), 1), Colors.edge);
            else
               CuBit.UI.Icons.Draw
                 (Clipped, Box.x + TEXT_INSET, Box.y + (ROW_HEIGHT - CuBit.UI.Icons.ICON_SIZE) / 2, Item.Picture);
               Draw_UI_Text_Transparent
                 (Clipped, Box.x + TEXT_INSET + CuBit.UI.Icons.ICON_SIZE + ICON_GAP, Center_Text_Y (Box),
                  Item.Caption (1 .. Item.Length), Ink);
            end if;
            Below := Box.y + Box.h;
         end;
      end loop;
      if Below < Drawer.y + Drawer.h then
         Fill_Rect (C, (Drawer.x, Below, Drawer.w, Drawer.y + Drawer.h - Below), Colors.panel);
      end if;
      Draw_Vertical_Splitter (C, Edge, Colors, False, Edge_Active);
   end Draw;
end CuBit.UI.Drawers;
