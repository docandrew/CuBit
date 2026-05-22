------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Stateful widgets that draw themselves and register hit/damage metadata
------------------------------------------------------------------------------
package body CuBit.UI.Widgets is
   procedure Label
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       muted : Boolean := False)
   is
      fg : constant CuBit.UI.Color :=
         (if muted then colors.muted else colors.text);
      bg : constant CuBit.UI.Color := colors.face;
      y : Natural := bounds.y;
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      if bounds.h > CuBit.UI.UI_Text_Height then
         y := bounds.y + (bounds.h - CuBit.UI.UI_Text_Height) / 2;
      end if;
      CuBit.UI.Draw_UI_Text (c, bounds.x, y, text, fg, bg);
   end Label;

   function Inner_Rect
      (r : CuBit.UI.Rect;
       left, top, right, bottom : Natural) return CuBit.UI.Rect
   is
      nx : constant Natural := r.x + Natural'Min (left, r.w);
      ny : constant Natural := r.y + Natural'Min (top, r.h);
      usedW : constant Natural := Natural'Min (left + right, r.w);
      usedH : constant Natural := Natural'Min (top + bottom, r.h);
   begin
      return (x => nx, y => ny, w => r.w - usedW, h => r.h - usedH);
   end Inner_Rect;

   function Clamp_Natural
      (value, lo, hi : Natural) return Natural
   is
   begin
      if value < lo then
         return lo;
      elsif value > hi then
         return hi;
      end if;
      return value;
   end Clamp_Natural;

   procedure Panel
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       content : out CuBit.UI.Rect;
       padding : Natural := 8)
   is
   begin
      CuBit.UI.Fill_Rect (c, bounds, colors.face);
      CuBit.UI.Stroke_Rect (c, bounds, colors.edge, colors.shadow);
      content := Inner_Rect (bounds, padding, padding, padding, padding);
   end Panel;

   procedure Group_Box
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       title : String;
       content : out CuBit.UI.Rect;
       padding : Natural := 8)
   is
   begin
      CuBit.UI.Draw_Pane (c, bounds, colors, title);
      content :=
         Inner_Rect
           (bounds,
            padding,
            padding + CuBit.UI.UI_Text_Height,
            padding,
            padding);
   end Group_Box;

   procedure Split_Pane
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       vertical : Boolean;
       position : in out Natural;
       first : out CuBit.UI.Rect;
       second : out CuBit.UI.Rect;
       splitterSize : Natural := 6;
       minFirst : Natural := 96;
       minSecond : Natural := 96)
   is
      usable : Natural;
      maxPos : Natural;
      split : CuBit.UI.Rect := (others => 0);
      result : CuBit.UI.Widget_Result;
      light : CuBit.UI.Color := colors.edge;
      dark : CuBit.UI.Color := colors.shadow;

      procedure Assign_Rects is
      begin
         if vertical then
            first := (x => bounds.x, y => bounds.y,
                      w => position, h => bounds.h);
            split := (x => bounds.x + position, y => bounds.y,
                      w => splitterSize, h => bounds.h);
            second := (x => split.x + split.w, y => bounds.y,
                       w => bounds.x + bounds.w - (split.x + split.w),
                       h => bounds.h);
         else
            first := (x => bounds.x, y => bounds.y,
                      w => bounds.w, h => position);
            split := (x => bounds.x, y => bounds.y + position,
                      w => bounds.w, h => splitterSize);
            second := (x => bounds.x, y => split.y + split.h,
                       w => bounds.w,
                       h => bounds.y + bounds.h - (split.y + split.h));
         end if;
      end Assign_Rects;
   begin
      first := (others => 0);
      second := (others => 0);
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      if vertical then
         usable := (if bounds.w > splitterSize then bounds.w - splitterSize
                    else 0);
      else
         usable := (if bounds.h > splitterSize then bounds.h - splitterSize
                    else 0);
      end if;

      if usable <= minFirst + minSecond then
         position := usable / 2;
      else
         maxPos := usable - minSecond;
         position := Clamp_Natural (position, minFirst, maxPos);
      end if;

      Assign_Rects;

      CuBit.UI.Controls.Add (controls, id, split, damage);
      result := CuBit.UI.State.Button (st, split);
      if result.active then
         if vertical then
            if st.pointer.x > bounds.x then
               position := Clamp_Natural (st.pointer.x - bounds.x,
                                          minFirst,
                                          usable - minSecond);
            end if;
         else
            if st.pointer.y > bounds.y then
               position := Clamp_Natural (st.pointer.y - bounds.y,
                                          minFirst,
                                          usable - minSecond);
            end if;
         end if;
         Assign_Rects;
      end if;

      if result.active then
         light := colors.shadow;
         dark := colors.edge;
      elsif result.hot then
         light := colors.accent;
         dark := colors.shadow;
      end if;

      CuBit.UI.Fill_Rect (c, split, colors.panel);
      CuBit.UI.Stroke_Rect (c, split, light, dark);
   end Split_Pane;

   procedure Scroll_Area
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       contentHeight : Natural;
       scrollY : in out Natural;
       viewport : out CuBit.UI.Rect;
       contentOriginY : out Natural;
       padding : Natural := 4)
   is
      scrollBar : CuBit.UI.Rect := (others => 0);
      maxScroll : Natural := 0;
      result : CuBit.UI.Widget_Result;
   begin
      Panel (c, bounds, colors, viewport, padding);
      if viewport.w > 18 then
         viewport.w := viewport.w - 18;
         scrollBar :=
           (x => viewport.x + viewport.w + 4,
            y => viewport.y,
            w => 14,
            h => viewport.h);
      end if;

      if contentHeight > viewport.h then
         maxScroll := contentHeight - viewport.h;
      end if;
      if scrollY > maxScroll then
         scrollY := maxScroll;
      end if;

      Vertical_Scrollbar
        (c, st, controls, id, scrollBar, damage, colors,
         0, maxScroll, scrollY, result);

      contentOriginY := viewport.y;
      if scrollY < contentOriginY then
         contentOriginY := contentOriginY - scrollY;
      else
         contentOriginY := 0;
      end if;
   end Scroll_Area;

   procedure Button
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       result : out CuBit.UI.Widget_Result)
   is
      style : CuBit.UI.Button_Style;
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      style :=
         (if result.active then CuBit.UI.Button_Pressed
          elsif result.hot then CuBit.UI.Button_Hot
          else CuBit.UI.Button_Normal);
      CuBit.UI.Draw_Button (c, bounds, colors, style, label);

      if CuBit.UI.State.Is_Last_Widget_Focused (st) then
         CuBit.UI.Stroke_Rect
           (c, CuBit.UI.Inflate_Rect (bounds, 1),
            colors.accent,
            colors.accent);
      end if;
   end Button;

   procedure Disabled_Button
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String)
   is
   begin
      CuBit.UI.Draw_Button
        (c, bounds, colors, CuBit.UI.Button_Disabled, label);
   end Disabled_Button;

   procedure Checkbox
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       checked : in out Boolean;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Checkbox (st, bounds, checked);
      CuBit.UI.Draw_Checkbox
        (c, bounds, colors, checked, result.hot, result.active);
   end Checkbox;

   procedure Radio_Button
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       selectedValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         value := selectedValue;
      end if;
      CuBit.UI.Draw_Radio_Button
        (c, bounds, colors, value = selectedValue,
         result.hot, result.active, label);
   end Radio_Button;

   procedure List_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       itemIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := itemIndex;
      end if;
      CuBit.UI.Draw_List_Item
        (c, bounds, colors, selectedIndex = itemIndex, result.hot, label);
   end List_Item;

   procedure Menu_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       enabled : Boolean;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      if enabled then
         CuBit.UI.Controls.Add (controls, id, bounds, damage);
         result := CuBit.UI.State.Button (st, bounds);
      else
         result := (others => False);
      end if;
      CuBit.UI.Draw_Menu_Item
        (c, bounds, colors, result.hot, result.active, enabled, label);
   end Menu_Item;

   procedure Menu_Title
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       open : Boolean;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      CuBit.UI.Draw_Menu_Title
        (c, bounds, colors, result.hot, open or else result.active, label);
   end Menu_Title;

   procedure Table_Row
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
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := rowIndex;
      end if;
      CuBit.UI.Draw_Table_Row
        (c, bounds, colors, selectedIndex = rowIndex, result.hot, c1, c2, c3);
   end Table_Row;

   procedure Tab
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       tabIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := tabIndex;
      end if;
      CuBit.UI.Draw_Tab
        (c, bounds, colors, selectedIndex = tabIndex,
         result.hot, result.active, label);
   end Tab;

   procedure Tab_Panel
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       firstID : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       labels : Tab_Title_List;
       selectedIndex : in out Natural;
       page : out CuBit.UI.Rect;
       stripHeight : Natural := 30)
   is
      padding : constant Natural := 24;
      stripH : constant Natural := Natural'Min (stripHeight, bounds.h);
      tabH : constant Natural :=
         (if stripH > 4 then stripH - 4 else stripH);
      tabY : constant Natural := bounds.y + (stripH - tabH);
      tabX : Natural := bounds.x + 2;
      strip : constant CuBit.UI.Rect :=
        (x => bounds.x, y => bounds.y, w => bounds.w, h => stripH);
      result : CuBit.UI.Widget_Result;
      id : CuBit.UI.Controls.Control_ID;
      index : Natural;
      tabBounds : CuBit.UI.Rect;
      tabW : Natural;
   begin
      if labels'Length = 0 then
         selectedIndex := 0;
      elsif selectedIndex = 0 or else selectedIndex > labels'Length then
         selectedIndex := 1;
      end if;

      page := (others => 0);
      if bounds.h > stripH then
         page :=
           (x => bounds.x, y => bounds.y + stripH,
            w => bounds.w, h => bounds.h - stripH);
      end if;

      CuBit.UI.Draw_Tab_Strip (c, strip, colors);
      CuBit.UI.Fill_Rect (c, page, colors.face);
      CuBit.UI.Stroke_Rect (c, page, colors.edge, colors.shadow);

      for i in labels'Range loop
         if labels (i) /= null and then tabX < bounds.x + bounds.w then
            index := Natural (i - labels'First) + 1;
            tabW := CuBit.UI.UI_Text_Width (labels (i).all) + padding;
            tabW := Natural'Min (tabW, bounds.x + bounds.w - tabX);
            tabBounds := (x => tabX, y => tabY, w => tabW, h => tabH);
            id := firstID + index - 1;

            CuBit.UI.Controls.Add (controls, id, tabBounds, damage);
            result := CuBit.UI.State.Button (st, tabBounds);
            if result.activated then
               selectedIndex := index;
            end if;
            CuBit.UI.Draw_Tab
              (c, tabBounds, colors, selectedIndex = index,
               result.hot, result.active, labels (i).all);

            tabX := tabX + tabW;
         end if;
      end loop;
   end Tab_Panel;

   procedure Horizontal_Slider
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       minValue, maxValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result :=
         CuBit.UI.State.Horizontal_Slider
           (st, bounds, value, minValue, maxValue);
      CuBit.UI.Draw_Horizontal_Slider
        (c, bounds, colors, minValue, maxValue, value,
         result.hot, result.active);
   end Horizontal_Slider;

   procedure Vertical_Scrollbar
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       minValue, maxValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result :=
         CuBit.UI.State.Vertical_Scrollbar
           (st, bounds, value, minValue, maxValue);
      CuBit.UI.Draw_Vertical_Scrollbar
        (c, bounds, colors, minValue, maxValue, value,
         result.hot, result.active);
   end Vertical_Scrollbar;

   procedure Text_Field
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Text_Field (st, bounds, text);
      CuBit.UI.Draw_Text_Edit_Field
        (c, bounds, colors, text,
         st.textCursor,
         st.textSelectionStart,
         st.textSelectionEnd,
         CuBit.UI.State.Is_Last_Widget_Focused (st),
         result.hot);
   end Text_Field;
end CuBit.UI.Widgets;
