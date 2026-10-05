------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Table controls
------------------------------------------------------------------------------
package body CuBit.UI.Tables is
   DIVIDER_HIT_WIDTH : constant Positive := 7;

   function Clamp
      (value, minimum, maximum : Natural) return Natural
   is
   begin
      if maximum <= minimum then
         return minimum;
      end if;
      return Natural'Max (minimum, Natural'Min (value, maximum));
   end Clamp;

   function Divider_Bounds
      (header : CuBit.UI.Rect; offset : Natural) return CuBit.UI.Rect
   is
      center : constant Natural :=
        header.x + Natural'Min (offset, header.w);
      left : constant Natural :=
        (if center >= header.x + DIVIDER_HIT_WIDTH / 2
         then center - DIVIDER_HIT_WIDTH / 2 else header.x);
   begin
      if CuBit.UI.Is_Empty (header) or else left >= header.x + header.w then
         return (others => 0);
      end if;
      return
        (x => left, y => header.y,
         w => Natural'Min
           (DIVIDER_HIT_WIDTH, header.x + header.w - left),
         h => header.h);
   end Divider_Bounds;

   procedure Resizable_Header
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       firstDividerId, secondDividerId : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       c1, c2, c3 : String;
       layout : in out CuBit.UI.Table_Column_Layout;
       minimumFirst, minimumSecond, minimumThird : Natural := 32;
       retainedInput : Boolean := False)
   is
      firstDivider, secondDivider : CuBit.UI.Rect := (others => 0);
      firstResult, secondResult : CuBit.UI.Widget_Result;
      desired, firstMaximum, secondMaximum : Natural := 0;
      retainedValue : Natural;
      retainedAvailable : Boolean;
      resizable : constant Boolean :=
        bounds.w >= minimumFirst + minimumSecond + minimumThird;

      procedure Draw_Hot_Divider
         (offset : Natural; result : CuBit.UI.Widget_Result)
      is
         x : constant Natural := bounds.x + Natural'Min (offset, bounds.w);
      begin
         if (result.hot or else result.active) and then
           x < bounds.x + bounds.w and then bounds.h > 4
         then
            CuBit.UI.Fill_Rect
              (c, (x => x, y => bounds.y + 2, w => 1, h => bounds.h - 4),
               colors.accent);
         end if;
      end Draw_Hot_Divider;
   begin
      if not resizable then
         CuBit.UI.Draw_Table_Header
           (c, bounds, colors, c1, c2, c3, layout);
         return;
      end if;

      firstMaximum := bounds.w - minimumSecond - minimumThird;
      layout.First_Width :=
        Clamp (layout.First_Width, minimumFirst, firstMaximum);
      secondMaximum := bounds.w - layout.First_Width - minimumThird;
      layout.Second_Width :=
        Clamp (layout.Second_Width, minimumSecond, secondMaximum);

      firstDivider := Divider_Bounds (bounds, layout.First_Width);
      if retainedInput then
         CuBit.UI.Controls.Add_Horizontal_Drag
           (controls, firstDividerId, firstDivider, damage,
            layout.First_Width, minimumFirst, firstMaximum, bounds.x);
         CuBit.UI.Controls.Take_Value
           (controls, firstDividerId, retainedValue, retainedAvailable);
         if retainedAvailable then
            layout.First_Width := retainedValue;
         end if;
         firstResult :=
           (hot => st.pointer.enabled and then
              CuBit.UI.Point_In_Rect
                (st.pointer.x, st.pointer.y,
                 CuBit.UI.Controls.Bounds (controls, firstDividerId)),
            active => CuBit.UI.Controls.Is_Active
              (controls, firstDividerId),
            activated => False);
      else
         CuBit.UI.Controls.Add
           (controls, firstDividerId, firstDivider, damage,
            CuBit.UI.Pointer_Resize_Horizontal,
            continuousAction => True);
         firstResult := CuBit.UI.State.Button
           (st, CuBit.UI.Controls.Bounds (controls, firstDividerId),
            CuBit.UI.State.Widget_ID (firstDividerId));
         if CuBit.UI.State.Is_Last_Widget_Captured (st) then
            firstResult.active := True;
            desired :=
              (if st.pointer.x <= bounds.x then 0
               else st.pointer.x - bounds.x);
            layout.First_Width :=
              Clamp (desired, minimumFirst, firstMaximum);
         end if;
      end if;
      if firstResult.active then
         secondMaximum := bounds.w - layout.First_Width - minimumThird;
         layout.Second_Width :=
           Clamp (layout.Second_Width, minimumSecond, secondMaximum);
      end if;

      secondDivider := Divider_Bounds
        (bounds, layout.First_Width + layout.Second_Width);
      if retainedInput then
         CuBit.UI.Controls.Add_Horizontal_Drag
           (controls, secondDividerId, secondDivider, damage,
            layout.Second_Width, minimumSecond, secondMaximum,
            bounds.x + layout.First_Width);
         CuBit.UI.Controls.Take_Value
           (controls, secondDividerId, retainedValue, retainedAvailable);
         if retainedAvailable then
            layout.Second_Width := retainedValue;
         end if;
         secondResult :=
           (hot => st.pointer.enabled and then
              CuBit.UI.Point_In_Rect
                (st.pointer.x, st.pointer.y,
                 CuBit.UI.Controls.Bounds (controls, secondDividerId)),
            active => CuBit.UI.Controls.Is_Active
              (controls, secondDividerId),
            activated => False);
      else
         CuBit.UI.Controls.Add
           (controls, secondDividerId, secondDivider, damage,
            CuBit.UI.Pointer_Resize_Horizontal,
            continuousAction => True);
         secondResult := CuBit.UI.State.Button
           (st, CuBit.UI.Controls.Bounds (controls, secondDividerId),
            CuBit.UI.State.Widget_ID (secondDividerId));
         if CuBit.UI.State.Is_Last_Widget_Captured (st) then
            secondResult.active := True;
            desired :=
              (if st.pointer.x <= bounds.x + layout.First_Width then 0
               else st.pointer.x - bounds.x - layout.First_Width);
            layout.Second_Width :=
              Clamp (desired, minimumSecond, secondMaximum);
         end if;
      end if;

      CuBit.UI.Draw_Table_Header
        (c, bounds, colors, c1, c2, c3, layout);
      Draw_Hot_Divider (layout.First_Width, firstResult);
      Draw_Hot_Divider
        (layout.First_Width + layout.Second_Width, secondResult);
   end Resizable_Header;

   procedure Row
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       c1, c2, c3 : String;
       rowIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button
        (st, CuBit.UI.Controls.Bounds (controls, id),
         CuBit.UI.State.Widget_ID (id));
      if result.activated and then selectedIndex /= rowIndex then
         selectedIndex := rowIndex;
         CuBit.UI.State.Request_Followup_Render (st);
      end if;
      CuBit.UI.Draw_Table_Row
        (c, bounds, colors, selectedIndex = rowIndex, result.hot, c1, c2, c3);
   end Row;

   function Column_Left (Layout : Column_Layout; Column : Column_Index; Width : Natural) return Natural is
      Left : Natural := 0;
   begin
      for I in 1 .. Column - 1 loop
         Left := Natural'Min (Left + Layout.Width (I), Width);
      end loop;
      return Left;
   end Column_Left;

   function Column_Width (Layout : Column_Layout; Column : Column_Index; Width : Natural) return Natural is
      Left : constant Natural := Column_Left (Layout, Column, Width);
   begin
      return (if Column = Layout.Count then Width - Left else Natural'Min (Layout.Width (Column), Width - Left));
   end Column_Width;

   procedure Toggle_Sort (Layout : in out Column_Layout; Column : Column_Index) is
   begin
      if Layout.Sort_Column = Column then
         Layout.Order := (if Layout.Order = Ascending then Descending else Ascending);
      else
         Layout.Sort_Column := Column;
         Layout.Order := Ascending;
      end if;
   end Toggle_Sort;

   function Edge_ID (Base : Column_ID_Base; Column : Column_Index) return CuBit.UI.Controls.Control_ID is
     (Base + Column - 1);
   function Header_ID (Base : Column_ID_Base; Column : Column_Index) return CuBit.UI.Controls.Control_ID is
     (Base + MAX_COLUMNS + Column - 1);

   procedure Handle_Header_Release
     (Layout : in out Column_Layout;
      controls : in out CuBit.UI.Controls.Control_Map;
      Base : Column_ID_Base;
      Target : CuBit.UI.Controls.Control_ID;
      Changed : out Boolean) is
   begin
      Changed := False;
      if Layout.Sortable and then Target >= Header_ID (Base, 1) and then
        Target <= Header_ID (Base, Column_Index'Max (1, Layout.Count)) and then Layout.Count > 0 and then
        CuBit.UI.Controls.Take_Activated (controls, Target)
      then
         Toggle_Sort (Layout, Target - Header_ID (Base, 1) + 1);
         Changed := True;
      end if;
   end Handle_Header_Release;

   --  A small filled triangle, apex up (ascending) or down, ending at Right.
   procedure Draw_Sort_Mark
     (c : CuBit.UI.Canvas; Right, Middle : Natural; Order : Sort_Order; Ink : CuBit.UI.Color)
   is
      HALF_BASE : constant := 4;
   begin
      if Right < 2 * HALF_BASE + 1 or else Middle < HALF_BASE then
         return;
      end if;
      for Step in 0 .. HALF_BASE loop
         declare
            Span : constant Natural := (if Order = Ascending then Step else HALF_BASE - Step);
            Y : constant Natural := Middle - HALF_BASE / 2 + Step;
         begin
            CuBit.UI.Fill_Rect
              (c, (x => Right - HALF_BASE - Span, y => Y, w => 2 * Span + 1, h => 1), Ink);
         end;
      end loop;
   end Draw_Sort_Mark;

   procedure Columns_Header
      (c : CuBit.UI.Canvas;
       st : CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       Base : Column_ID_Base;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       Layout : in out Column_Layout)
   is
      HEADER_FRAME_WIDTH : constant Natural := 2;
      Edge : constant CuBit.UI.Color := CuBit.UI.Control_Edge (colors);
      Value : Natural;
      Available : Boolean;

      function Pointer_In (Area : CuBit.UI.Rect) return Boolean is
        (st.pointer.enabled and then CuBit.UI.Point_In_Rect (st.pointer.x, st.pointer.y, Area));
      --  The widest Column may be with every later column kept and the last at its minimum.
      function Widest (Column : Column_Index) return Natural is
         Others_Width : Natural := Layout.Minimum (Layout.Count);
      begin
         for I in 1 .. Layout.Count - 1 loop
            if I /= Column then
               Others_Width := Others_Width + Layout.Width (I);
            end if;
         end loop;
         return (if bounds.w > Others_Width then bounds.w - Others_Width else 0);
      end Widest;
   begin
      if CuBit.UI.Is_Empty (bounds) or else Layout.Count = 0 then
         return;
      end if;
      --  Edges first: a drag this frame moves everything to its right.
      for Column in 1 .. Layout.Count - 1 loop
         declare
            Maximum : constant Natural := Natural'Max (Layout.Minimum (Column), Widest (Column));
            Left : constant Natural := Column_Left (Layout, Column, bounds.w);
         begin
            Layout.Width (Column) := Clamp (Layout.Width (Column), Layout.Minimum (Column), Maximum);
            CuBit.UI.Controls.Add_Horizontal_Drag
              (controls, Edge_ID (Base, Column),
               Divider_Bounds (bounds, Left + Layout.Width (Column)), damage,
               Layout.Width (Column), Layout.Minimum (Column), Maximum, bounds.x + Left);
            CuBit.UI.Controls.Take_Value (controls, Edge_ID (Base, Column), Value, Available);
            if Available then
               Layout.Width (Column) := Clamp (Value, Layout.Minimum (Column), Maximum);
            end if;
         end;
      end loop;

      CuBit.UI.Fill_Rect (c, bounds, colors.panel);
      CuBit.UI.Fill_Rect (c, (bounds.x, bounds.y + bounds.h - 1, bounds.w, 1), Edge);
      for Column in 1 .. Layout.Count loop
         declare
            Left : constant Natural := Column_Left (Layout, Column, bounds.w);
            Width : constant Natural := Column_Width (Layout, Column, bounds.w);
            Cell : constant CuBit.UI.Rect := (x => bounds.x + Left, y => bounds.y, w => Width, h => bounds.h);
            Label_Area : constant CuBit.UI.Rect :=
              (if bounds.h > HEADER_FRAME_WIDTH * 2
               then (Cell.x, Cell.y + HEADER_FRAME_WIDTH, Cell.w, Cell.h - HEADER_FRAME_WIDTH * 2) else Cell);
            Sorted : constant Boolean := Layout.Sortable and then Layout.Sort_Column = Column;
            Mark_Room : constant Natural := (if Sorted then 14 else 0);
         begin
            if Layout.Sortable and then Width > 0 then
               CuBit.UI.Controls.Add_Button (controls, Header_ID (Base, Column), Cell, damage);
               if Pointer_In (Cell) or else CuBit.UI.Controls.Is_Active (controls, Header_ID (Base, Column)) then
                  CuBit.UI.Fill_Rect
                    (c, (Cell.x, Cell.y, Cell.w, (if Cell.h > 0 then Cell.h - 1 else 0)), colors.face);
               end if;
            end if;
            if Column < Layout.Count and then Width > 0 and then Left + Width < bounds.w then
               CuBit.UI.Fill_Rect (c, (bounds.x + Left + Width - 1, bounds.y, 1, bounds.h), Edge);
               if Pointer_In (CuBit.UI.Controls.Bounds (controls, Edge_ID (Base, Column))) or else
                 CuBit.UI.Controls.Is_Active (controls, Edge_ID (Base, Column))
               then
                  CuBit.UI.Fill_Rect
                    (c, (bounds.x + Left + Width - 1, bounds.y + 2, 1,
                         (if bounds.h > 4 then bounds.h - 4 else 0)), colors.accent);
               end if;
            end if;
            CuBit.UI.Draw_UI_Text
              (CuBit.UI.With_Clip
                 (c, CuBit.UI.Content_Rect
                    ((Cell.x, Cell.y, (if Cell.w > Mark_Room then Cell.w - Mark_Room else 0), Cell.h),
                     Layout.Cell_Padding, HEADER_FRAME_WIDTH)),
               Cell.x + Layout.Cell_Padding, CuBit.UI.Center_Text_Y (Label_Area), Title (Column),
               colors.text, colors.panel);
            if Sorted and then Width > Mark_Room then
               Draw_Sort_Mark
                 (CuBit.UI.With_Clip (c, Cell), Cell.x + Cell.w - Layout.Cell_Padding,
                  Cell.y + Cell.h / 2, Layout.Order, colors.muted);
            end if;
         end;
      end loop;
   end Columns_Header;

   procedure Draw_Columns_Row
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       Layout : Column_Layout;
       selected, hot : Boolean;
       textStyle : CuBit.UI.Table_Text_Style := CuBit.UI.Table_Interface_Text)
   is
      Background : constant CuBit.UI.Color :=
        (if selected then colors.selection elsif hot then colors.panel else colors.field);
      Foreground : constant CuBit.UI.Color := (if selected then colors.selectionText else colors.text);
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;
      CuBit.UI.Fill_Rect (c, bounds, Background);
      CuBit.UI.Fill_Rect (c, (bounds.x, bounds.y + bounds.h - 1, bounds.w, 1), colors.edge);
      for Column in 1 .. Layout.Count loop
         declare
            Left : constant Natural := Column_Left (Layout, Column, bounds.w);
            Width : constant Natural := Column_Width (Layout, Column, bounds.w);
            Cell_Area : constant CuBit.UI.Rect := (bounds.x + Left, bounds.y, Width, bounds.h);
            Clipped : constant CuBit.UI.Canvas :=
              CuBit.UI.With_Clip (c, CuBit.UI.Content_Rect (Cell_Area, Layout.Cell_Padding, 1));
            Text_Ink : constant CuBit.UI.Color := Ink (Column, Foreground);
         begin
            if Column < Layout.Count and then Width > 0 and then Left + Width < bounds.w then
               CuBit.UI.Fill_Rect (c, (bounds.x + Left + Width - 1, bounds.y, 1, bounds.h), colors.edge);
            end if;
            if textStyle = CuBit.UI.Table_Code_Text then
               --  Text cells over the row's own colour: cached blended glyphs.
               CuBit.UI.Draw_Code_Text
                 (Clipped, Cell_Area.x + Layout.Cell_Padding,
                  (if Cell_Area.h > CuBit.UI.Code_Text_Height
                   then Cell_Area.y + (Cell_Area.h - CuBit.UI.Code_Text_Height) / 2 else Cell_Area.y),
                  Cell (Column), Text_Ink, Background);
            else
               CuBit.UI.Draw_UI_Text
                 (Clipped, Cell_Area.x + Layout.Cell_Padding, CuBit.UI.Center_Text_Y (Cell_Area),
                  Cell (Column), Text_Ink, Background);
            end if;
         end;
      end loop;
   end Draw_Columns_Row;
end CuBit.UI.Tables;
