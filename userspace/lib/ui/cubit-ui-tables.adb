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
end CuBit.UI.Tables;
