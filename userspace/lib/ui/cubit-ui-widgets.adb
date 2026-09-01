------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Stateful widgets that draw themselves and register hit/damage metadata
------------------------------------------------------------------------------
package body CuBit.UI.Widgets is
   function Parent_Canvas
      (c : CuBit.UI.Canvas;
       parent : CuBit.UI.Rect) return CuBit.UI.Canvas
   is
   begin
      if CuBit.UI.Is_Empty (parent) then
         return c;
      end if;
      return CuBit.UI.With_Clip (c, parent);
   end Parent_Canvas;

   function Intersect
      (a, b : CuBit.UI.Rect) return CuBit.UI.Rect
   is
      x1 : Natural;
      y1 : Natural;
      x2 : Natural;
      y2 : Natural;
   begin
      if CuBit.UI.Is_Empty (a) or else CuBit.UI.Is_Empty (b) then
         return (others => 0);
      end if;

      x1 := Natural'Max (a.x, b.x);
      y1 := Natural'Max (a.y, b.y);
      x2 := Natural'Min (a.x + a.w, b.x + b.w);
      y2 := Natural'Min (a.y + a.h, b.y + b.h);

      if x1 >= x2 or else y1 >= y2 then
         return (others => 0);
      end if;
      return (x => x1, y => y1, w => x2 - x1, h => y2 - y1);
   end Intersect;

   function State_Bounds
      (bounds, parent : CuBit.UI.Rect) return CuBit.UI.Rect
   is
   begin
      if CuBit.UI.Is_Empty (parent) then
         return bounds;
      end if;
      return Intersect (bounds, parent);
   end State_Bounds;

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
      tc : constant CuBit.UI.Canvas := CuBit.UI.With_Clip (c, bounds);
      y : Natural := bounds.y;
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      if bounds.h > CuBit.UI.UI_Text_Height then
         y := bounds.y + (bounds.h - CuBit.UI.UI_Text_Height) / 2;
      end if;
      CuBit.UI.Draw_UI_Text (tc, bounds.x, y, text, fg, bg);
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

   procedure Badge
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       style : Badge_Style := Badge_Neutral)
   is
      fill : CuBit.UI.Color := colors.face;
      fg   : CuBit.UI.Color := colors.text;
      tc : constant CuBit.UI.Canvas := CuBit.UI.With_Clip (c, bounds);
      textY : Natural := bounds.y;
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      case style is
         when Badge_Neutral =>
            fill := colors.panel;
            fg := colors.text;
         when Badge_Good =>
            fill := 16#DDEFE6#;
            fg := colors.good;
         when Badge_Danger =>
            fill := 16#F8E2DF#;
            fg := colors.danger;
      end case;

      if bounds.h > CuBit.UI.UI_Text_Height then
         textY := bounds.y + (bounds.h - CuBit.UI.UI_Text_Height) / 2;
      end if;

      CuBit.UI.Fill_Rect (c, bounds, fill);
      CuBit.UI.Stroke_Rect (c, bounds, colors.edge, colors.edge);
      CuBit.UI.Draw_UI_Text (tc, bounds.x + 6, textY, label, fg, fill);
   end Badge;

   procedure Key_Value
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       key : String;
       value : String;
       mutedValue : Boolean := False)
   is
      keyW : constant Natural := Natural'Min (96, bounds.w / 2);
      valueX : constant Natural := bounds.x + keyW + 8;
      fgValue : constant CuBit.UI.Color :=
         (if mutedValue then colors.muted else colors.text);
      tc : constant CuBit.UI.Canvas := CuBit.UI.With_Clip (c, bounds);
      y : Natural := bounds.y;
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      if bounds.h > CuBit.UI.UI_Text_Height then
         y := bounds.y + (bounds.h - CuBit.UI.UI_Text_Height) / 2;
      end if;

      CuBit.UI.Draw_UI_Text (tc, bounds.x, y, key, colors.muted, colors.face);
      if valueX < bounds.x + bounds.w then
         CuBit.UI.Draw_UI_Text
           (tc, valueX, y, value, fgValue, colors.face);
      end if;
   end Key_Value;

   procedure Toolbar
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme)
   is
   begin
      CuBit.UI.Fill_Rect (c, bounds, colors.face);
      if bounds.h > 0 then
         CuBit.UI.Fill_Rect
           (c, (x => bounds.x, y => bounds.y + bounds.h - 1,
                w => bounds.w, h => 1), colors.shadow);
      end if;
   end Toolbar;

   procedure Toolbar_Button
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       icon : Toolbar_Icon;
       enabled : Boolean := True;
       pressed : Boolean := False)
   is
      Style : constant CuBit.UI.Button_Style :=
        (if not enabled then CuBit.UI.Button_Disabled
         elsif pressed then CuBit.UI.Button_Pressed
         else CuBit.UI.Button_Normal);
      Ink : constant CuBit.UI.Color :=
        (if not enabled then colors.muted
         elsif icon = Run_Program then colors.good
         elsif icon = Interpret_Source then colors.accent
         elsif icon = Stop_Program then colors.danger
         else colors.text);
      X : constant Natural := bounds.x + (if pressed and enabled then 1 else 0);
      Y : constant Natural := bounds.y + (if pressed and enabled then 1 else 0);
   begin
      CuBit.UI.Draw_Button (c, bounds, colors, Style, "");
      case icon is
         when Open_Document =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 6, y => Y + 9, w => 13, h => 9), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 7, y => Y + 7, w => 6, h => 3), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 8, y => Y + 11, w => 11, h => 5), colors.face);
         when Save_Document =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 7, y => Y + 6, w => 12, h => 13), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 9, y => Y + 7, w => 7, h => 4), colors.face);
            CuBit.UI.Fill_Rect
              (c, (x => X + 10, y => Y + 14, w => 6, h => 4), colors.face);
         when Compile_Program =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 7, y => Y + 7, w => 5, h => 5), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 14, y => Y + 7, w => 5, h => 5), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 10, y => Y + 14, w => 6, h => 6), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 11, y => Y + 11, w => 4, h => 5), Ink);
         when Interpret_Source =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 12, y => Y + 5, w => 7, h => 3), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 10, y => Y + 8, w => 7, h => 5), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 8, y => Y + 12, w => 7, h => 3), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 8, y => Y + 15, w => 3, h => 6), Ink);
         when Run_Program =>
            for Row in 0 .. 10 loop
               CuBit.UI.Fill_Rect
                 (c, (x => X + 8, y => Y + 7 + Row,
                      w => 1 + Natural'Min (Row, 10 - Row), h => 1), Ink);
            end loop;
         when Pause_Program =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 8, y => Y + 7, w => 4, h => 11), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 15, y => Y + 7, w => 4, h => 11), Ink);
         when Stop_Program =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 8, y => Y + 7, w => 11, h => 11), Ink);
         when Step_Into =>
            CuBit.UI.Fill_Rect
              (c, (x => X + 12, y => Y + 6, w => 3, h => 8), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 9, y => Y + 11, w => 9, h => 3), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 11, y => Y + 14, w => 5, h => 3), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 7, y => Y + 19, w => 13, h => 2), Ink);
         when Step_Over =>
            CuBit.UI.Stroke_Rect
              (c, (x => X + 7, y => Y + 7, w => 11, h => 9), Ink, Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 15, y => Y + 5, w => 3, h => 7), Ink);
            CuBit.UI.Fill_Rect
              (c, (x => X + 17, y => Y + 9, w => 3, h => 3), Ink);
      end case;
   end Toolbar_Button;

   procedure Toolbar_Separator
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme)
   is
      Middle : constant Natural := bounds.x + bounds.w / 2;
   begin
      if bounds.w > 0 and then bounds.h > 8 then
         CuBit.UI.Fill_Rect
           (c, (x => Middle, y => bounds.y + 4, w => 1, h => bounds.h - 8),
            colors.shadow);
         if Middle + 1 < bounds.x + bounds.w then
            CuBit.UI.Fill_Rect
              (c, (x => Middle + 1, y => bounds.y + 4,
                   w => 1, h => bounds.h - 8), colors.edge);
         end if;
      end if;
   end Toolbar_Separator;

   procedure Metric_Card
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       title : String;
       value : Natural)
   is
      content : CuBit.UI.Rect;
   begin
      Panel (c, bounds, colors, content, DEFAULT_PADDING);
      Label
        (c, (x => content.x, y => content.y,
             w => content.w, h => CuBit.UI.UI_Text_Height),
         colors, title, muted => True);
      CuBit.UI.Draw_Natural_Value
        (c, (x => content.x, y => content.y + 24,
             w => content.w, h => CuBit.UI.UI_Text_Height),
         colors, value);
   end Metric_Card;

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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      style :=
         (if result.active then CuBit.UI.Button_Pressed
          elsif result.hot then CuBit.UI.Button_Hot
          else CuBit.UI.Button_Normal);
      CuBit.UI.Draw_Button (pc, bounds, colors, style, label);

      if CuBit.UI.State.Is_Last_Widget_Focused (st) then
         CuBit.UI.Stroke_Rect
           (pc, CuBit.UI.Inflate_Rect (bounds, 1),
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Checkbox (st, hitBounds, checked);
      CuBit.UI.Draw_Checkbox
        (pc, bounds, colors, checked, result.hot, result.active);
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      if result.activated then
         value := selectedValue;
      end if;
      CuBit.UI.Draw_Radio_Button
        (pc, bounds, colors, value = selectedValue,
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      if result.activated then
         selectedIndex := itemIndex;
      end if;
      CuBit.UI.Draw_List_Item
        (pc, bounds, colors, selectedIndex = itemIndex, result.hot, label);
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      if enabled then
         CuBit.UI.Controls.Add (controls, id, bounds, damage);
         result := CuBit.UI.State.Button (st, hitBounds);
      else
         result := (others => False);
      end if;
      CuBit.UI.Draw_Menu_Item
        (pc, bounds, colors, result.hot, result.active, enabled, label);
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      CuBit.UI.Draw_Menu_Title
        (pc, bounds, colors, result.hot, open or else result.active, label);
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      if result.activated then
         selectedIndex := rowIndex;
      end if;
      CuBit.UI.Draw_Table_Row
        (pc, bounds, colors, selectedIndex = rowIndex, result.hot, c1, c2, c3);
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, hitBounds);
      if result.activated then
         selectedIndex := tabIndex;
      end if;
      CuBit.UI.Draw_Tab
        (pc, bounds, colors, selectedIndex = tabIndex,
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
       changed : out Boolean;
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
   begin
      changed := False;
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

      CuBit.UI.Draw_Tab_Strip (pc, strip, colors);
      CuBit.UI.Fill_Rect (pc, page, colors.face);
      CuBit.UI.Stroke_Rect (pc, page, colors.edge, colors.shadow);

      for i in labels'Range loop
         if labels (i) /= null and then tabX < bounds.x + bounds.w then
            index := Natural (i - labels'First) + 1;
            tabW := CuBit.UI.UI_Text_Width (labels (i).all) + padding;
            tabW := Natural'Min (tabW, bounds.x + bounds.w - tabX);
            tabBounds := (x => tabX, y => tabY, w => tabW, h => tabH);
            id := firstID + index - 1;

            CuBit.UI.Controls.Add (controls, id, tabBounds, damage);
            result :=
               CuBit.UI.State.Button
                 (st, State_Bounds (tabBounds, damage));
            if result.activated then
               if selectedIndex /= index then
                  selectedIndex := index;
                  changed := True;
               end if;
            end if;
            CuBit.UI.Draw_Tab
              (pc, tabBounds, colors, selectedIndex = index,
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result :=
         CuBit.UI.State.Horizontal_Slider
           (st, hitBounds, value, minValue, maxValue);
      CuBit.UI.Draw_Horizontal_Slider
        (pc, bounds, colors, minValue, maxValue, value,
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result :=
         CuBit.UI.State.Vertical_Scrollbar
           (st, hitBounds, value, minValue, maxValue);
      CuBit.UI.Draw_Vertical_Scrollbar
        (pc, bounds, colors, minValue, maxValue, value,
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
      pc : constant CuBit.UI.Canvas := Parent_Canvas (c, damage);
      hitBounds : constant CuBit.UI.Rect := State_Bounds (bounds, damage);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Text_Field (st, hitBounds, text);
      CuBit.UI.Draw_Text_Edit_Field
        (pc, bounds, colors, text,
         st.textCursor,
         st.textSelectionStart,
         st.textSelectionEnd,
         CuBit.UI.State.Is_Last_Widget_Focused (st),
         result.hot);
   end Text_Field;
end CuBit.UI.Widgets;
