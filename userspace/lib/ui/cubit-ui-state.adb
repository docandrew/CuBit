------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Immediate-mode UI state
------------------------------------------------------------------------------
package body CuBit.UI.State is
   KEYMOD_SHIFT : constant Natural := 1;
   KEYMOD_CTRL  : constant Natural := 2;

   KEY_TAB       : constant Natural := 16#0F#;
   KEY_ENTER     : constant Natural := 16#1C#;
   KEY_BACKSPACE : constant Natural := 16#0E#;
   KEY_LEFT      : constant Natural := 16#4B#;
   KEY_RIGHT     : constant Natural := 16#4D#;
   KEY_UP        : constant Natural := 16#48#;
   KEY_DOWN      : constant Natural := 16#50#;
   KEY_HOME      : constant Natural := 16#47#;
   KEY_END       : constant Natural := 16#4F#;
   KEY_DELETE    : constant Natural := 16#53#;

   DOUBLE_CLICK_FRAMES : constant Natural := 20;
   DOUBLE_CLICK_SLOP   : constant Natural := 4;

   function Min (a, b : Natural) return Natural is
   begin
      return (if a < b then a else b);
   end Min;

   function Max (a, b : Natural) return Natural is
   begin
      return (if a > b then a else b);
   end Max;

   function Is_Word_End (ch : Character) return Boolean is
      use ASCII;
   begin
      return ch = ' ' or else ch = '.' or else ch = '_' or else
             ch = '/' or else ch = '\' or else ch = ':' or else
             ch = '-' or else ch = CR or else ch = LF or else ch = HT;
   end Is_Word_End;

   function Abs_Diff (a, b : Natural) return Natural is
   begin
      return (if a > b then a - b else b - a);
   end Abs_Diff;

   function Text_Index_At_X
      (bounds : CuBit.UI.Rect; text : String; x : Natural) return Natural
   is
      textX : Natural := bounds.x + 6;
      charW : Natural;
      idx : Natural := 0;
   begin
      for i in text'Range loop
         charW := CuBit.UI.UI_Text_Width (text (i .. i));
         if x <= textX + charW / 2 then
            return idx;
         end if;
         textX := textX + charW;
         idx := idx + 1;
      end loop;

      return text'Length;
   end Text_Index_At_X;

   procedure Clamp_Text_State (st : in out UI_State; len : Natural) is
   begin
      st.textCursor := Min (st.textCursor, len);
      st.textSelectionStart := Min (st.textSelectionStart, len);
      st.textSelectionEnd := Min (st.textSelectionEnd, len);
      st.textSelectionAnchor := Min (st.textSelectionAnchor, len);
   end Clamp_Text_State;

   procedure Set_Cursor
      (st : in out UI_State; pos : Natural; len : Natural; shift : Boolean)
   is
      newPos : constant Natural := Min (pos, len);
   begin
      if shift then
         if st.textSelectionStart = st.textSelectionEnd then
            st.textSelectionAnchor := st.textCursor;
         end if;
         st.textCursor := newPos;
         st.textSelectionStart := Min (st.textSelectionAnchor, newPos);
         st.textSelectionEnd := Max (st.textSelectionAnchor, newPos);
      else
         st.textCursor := newPos;
         st.textSelectionStart := newPos;
         st.textSelectionEnd := newPos;
         st.textSelectionAnchor := newPos;
      end if;
   end Set_Cursor;

   function Word_Left (text : String; cursor : Natural) return Natural is
      pos : Natural := cursor;
   begin
      while pos > 0 and then Is_Word_End (text (text'First + pos - 1)) loop
         pos := pos - 1;
      end loop;
      while pos > 0 and then not Is_Word_End (text (text'First + pos - 1)) loop
         pos := pos - 1;
      end loop;
      return pos;
   end Word_Left;

   function Word_Right (text : String; cursor : Natural) return Natural is
      pos : Natural := cursor;
   begin
      while pos < text'Length and then
         not Is_Word_End (text (text'First + pos))
      loop
         pos := pos + 1;
      end loop;
      while pos < text'Length and then Is_Word_End (text (text'First + pos)) loop
         pos := pos + 1;
      end loop;
      return pos;
   end Word_Right;

   procedure Select_Word
      (st : in out UI_State; text : String; cursor : Natural)
   is
      first : Natural := cursor;
      last : Natural := cursor;
   begin
      if text'Length = 0 then
         Set_Cursor (st, 0, 0, False);
         return;
      end if;

      if first = text'Length then
         first := first - 1;
      end if;
      last := first;

      while first > 0 and then
         not Is_Word_End (text (text'First + first - 1))
      loop
         first := first - 1;
      end loop;
      while last < text'Length and then
         not Is_Word_End (text (text'First + last))
      loop
         last := last + 1;
      end loop;

      st.textSelectionStart := first;
      st.textSelectionEnd := last;
      st.textSelectionAnchor := first;
      st.textCursor := last;
   end Select_Word;

   procedure Delete_Range
      (text : in out String;
       textLen : in out Natural;
       first, last : Natural)
   is
      count : constant Natural := last - first;
   begin
      if count = 0 then
         return;
      end if;

      for i in first + 1 .. textLen - count loop
         text (i) := text (i + count);
      end loop;
      for i in textLen - count + 1 .. textLen loop
         text (i) := ' ';
      end loop;
      textLen := textLen - count;
   end Delete_Range;

   procedure Begin_Frame (st : in out UI_State) is
   begin
      st.frameCounter := st.frameCounter + 1;
      st.hotItem := NO_ITEM;
      st.hotScope := NO_SCOPE;
      st.rootLastID := NO_ITEM;
      st.lastIDs := (others => NO_ITEM);
      st.currentScope := NO_SCOPE;
      st.scopeXOffset := 0;
      st.scopeYOffset := 0;
      st.scopeError := False;
      st.followupRenderRequested := False;
   end Begin_Frame;

   procedure Request_Followup_Render (st : in out UI_State) is
   begin
      st.followupRenderRequested := True;
   end Request_Followup_Render;

   function Followup_Render_Requested (st : UI_State) return Boolean is
     (st.followupRenderRequested);

   procedure Finish_Frame (st : in out UI_State) is
   begin
      if not st.pointer.down then
         st.activeItem := NO_ITEM;
         st.activeScope := NO_SCOPE;
         st.scrollbarPart := CuBit.UI.Scrollbar_None;
         st.scrollbarGrabOffset := 0;
         st.sliderGrabOffset := 0;
      elsif st.activeItem = NO_ITEM then
         st.activeItem := INVALID_ITEM;
         st.activeScope := NO_SCOPE;
      end if;

      --  If a focused widget was not drawn this frame, clear keyboard focus.
      --  This prevents hidden panels/dialogs from keeping stale focus forever.
      if not st.keyboardHeartbeat then
         st.keyboardItem := NO_ITEM;
         st.keyboardScope := NO_SCOPE;
      end if;
      st.keyboardHeartbeat := False;
      st.pointer.pressed := False;
      st.pointer.released := False;
      st.textDoubleClick := False;

      if st.currentScope /= NO_SCOPE then
         st.scopeError := True;
         st.currentScope := NO_SCOPE;
         st.scopeXOffset := 0;
         st.scopeYOffset := 0;
      end if;
   end Finish_Frame;

   procedure Set_Pointer
      (st : in out UI_State;
       x, y : Natural;
       down : Boolean;
       pressed : Boolean := False;
       released : Boolean := False;
       enabled : Boolean := True)
   is
   begin
      if pressed then
         --  A physical press begins a new capture transaction.  Do not let a
         --  lost or reordered release leave an older widget permanently
         --  active; the widget under this press will claim the empty slot
         --  when the frame is evaluated.
         st.activeItem := NO_ITEM;
         st.activeScope := NO_SCOPE;
         st.scrollbarPart := CuBit.UI.Scrollbar_None;
         st.scrollbarGrabOffset := 0;
         st.sliderGrabOffset := 0;
         st.textDoubleClick :=
            st.lastClickFrame /= 0 and then
            st.frameCounter <= st.lastClickFrame + DOUBLE_CLICK_FRAMES and then
            Abs_Diff (x, st.lastClickX) <= DOUBLE_CLICK_SLOP and then
            Abs_Diff (y, st.lastClickY) <= DOUBLE_CLICK_SLOP;
         st.lastClickFrame := st.frameCounter;
         st.lastClickX := x;
         st.lastClickY := y;
      end if;

      if released then
         st.textWordSelect := False;
      end if;

      st.pointer := (x => x, y => y, down => down,
                     pressed => pressed, released => released,
                     enabled => enabled);
   end Set_Pointer;

   procedure Resynchronize_Pointer
      (st : in out UI_State;
       x, y : Natural;
       down : Boolean)
   is
   begin
      st.activeItem := NO_ITEM;
      st.activeScope := NO_SCOPE;
      st.scrollbarPart := CuBit.UI.Scrollbar_None;
      st.scrollbarGrabOffset := 0;
      st.sliderGrabOffset := 0;
      st.textDoubleClick := False;
      st.textWordSelect := False;
      st.pointer :=
        (x => x, y => y, down => down,
         pressed => False, released => False, enabled => True);
   end Resynchronize_Pointer;

   procedure Enter_Scope
      (st : in out UI_State;
       xOffset : Natural := 0;
       yOffset : Natural := 0)
   is
   begin
      if st.currentScope = Scope_ID'Last then
         st.scopeError := True;
         return;
      end if;

      st.currentScope := st.currentScope + 1;
      st.scopeXOffset := st.scopeXOffset + xOffset;
      st.scopeYOffset := st.scopeYOffset + yOffset;
   end Enter_Scope;

   procedure Exit_Scope
      (st : in out UI_State;
       xOffset : Natural := 0;
       yOffset : Natural := 0)
   is
   begin
      if st.currentScope = NO_SCOPE then
         st.scopeError := True;
         return;
      end if;

      st.currentScope := st.currentScope - 1;
      if st.scopeXOffset >= xOffset then
         st.scopeXOffset := st.scopeXOffset - xOffset;
      else
         st.scopeXOffset := 0;
         st.scopeError := True;
      end if;
      if st.scopeYOffset >= yOffset then
         st.scopeYOffset := st.scopeYOffset - yOffset;
      else
         st.scopeYOffset := 0;
         st.scopeError := True;
      end if;
   end Exit_Scope;

   function Next_ID (st : in out UI_State) return Widget_ID is
      id : Widget_ID;
   begin
      if st.currentScope = NO_SCOPE then
         st.rootLastID := st.rootLastID + 1;
         id := st.rootLastID;
      else
         st.lastIDs (st.currentScope) := st.lastIDs (st.currentScope) + 1;
         id := st.lastIDs (st.currentScope);
      end if;

      st.lastWidget := id;
      st.lastScope := st.currentScope;
      return id;
   end Next_ID;

   function Resolve_ID
      (st : in out UI_State; requested : Widget_ID) return Widget_ID
   is
   begin
      if requested = NO_ITEM or else requested = INVALID_ITEM then
         return Next_ID (st);
      end if;

      --  Public controls already have stable application-assigned IDs.  Use
      --  those IDs for capture and focus as well as damage lookup so adding,
      --  removing, or reordering a sibling cannot transfer an interaction.
      st.lastWidget := requested;
      st.lastScope := st.currentScope;
      return requested;
   end Resolve_ID;

   function Offset_Rect
      (st : UI_State; bounds : CuBit.UI.Rect) return CuBit.UI.Rect
   is
   begin
      return
        (x => bounds.x + st.scopeXOffset,
         y => bounds.y + st.scopeYOffset,
         w => bounds.w,
         h => bounds.h);
   end Offset_Rect;

   function Region_Hit
      (st : UI_State; bounds : CuBit.UI.Rect) return Boolean
   is
   begin
      return CuBit.UI.Point_In_Rect (st.pointer.x,
                                     st.pointer.y,
                                     Offset_Rect (st, bounds));
   end Region_Hit;

   function Button
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       widgetID : Widget_ID := NO_ITEM)
      return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Resolve_ID (st, widgetID);
      scope : constant Scope_ID := st.currentScope;
      hit : constant Boolean := st.pointer.enabled and then Region_Hit (st, bounds);
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.pressed then
            st.activeItem := id;
            st.activeScope := scope;
         end if;
      end if;

      if st.keyboardItem = NO_ITEM then
         st.keyboardItem := id;
         st.keyboardScope := scope;
      end if;
      if st.keyboardItem = id and then st.keyboardScope = scope then
         st.keyboardHeartbeat := True;
      end if;

      return
        (hot       => hit,
         active    => hit and then
                      st.pointer.down and then
                      st.activeItem = id and then
                      st.activeScope = scope,
         activated => hit and then st.pointer.released and then
                      st.activeItem = id and then
                      st.activeScope = scope);
   end Button;

   function Is_Last_Widget_Captured (st : UI_State) return Boolean is
     (st.pointer.down and then
      st.activeItem = st.lastWidget and then
      st.activeScope = st.lastScope);

   function Is_Last_Widget_Focused (st : UI_State) return Boolean is
   begin
      return st.keyboardItem = st.lastWidget and then
             st.keyboardScope = st.lastScope;
   end Is_Last_Widget_Focused;

   procedure Clear_Keyboard_Focus (st : in out UI_State) is
   begin
      st.keyboardItem := NO_ITEM;
      st.keyboardScope := NO_SCOPE;
      st.keyboardHeartbeat := False;
      Set_Cursor (st, 0, 0, False);
      st.textWordSelect := False;
   end Clear_Keyboard_Focus;

   function Text_Field
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       text : String;
       widgetID : Widget_ID := NO_ITEM)
      return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Resolve_ID (st, widgetID);
      scope : constant Scope_ID := st.currentScope;
      r : constant CuBit.UI.Rect := Offset_Rect (st, bounds);
      hit : constant Boolean :=
         st.pointer.enabled and then Region_Hit (st, bounds);
      focused : Boolean;
      idx : Natural;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.pressed then
            st.activeItem := id;
            st.activeScope := scope;
         end if;

         if st.pointer.pressed then
            st.keyboardItem := id;
            st.keyboardScope := scope;
            idx := Text_Index_At_X (r, text, st.pointer.x);
            if st.textDoubleClick then
               Select_Word (st, text, idx);
               st.textWordSelect := True;
            else
               Set_Cursor (st, idx, text'Length, False);
            end if;
         end if;
      elsif st.pointer.pressed then
         --  Pointer activation outside the field commits the click to the
         --  target under the cursor and removes keyboard focus from this
         --  field. Dialog focus rules can grow more nuanced later.
         if st.keyboardItem = id and then st.keyboardScope = scope then
            st.keyboardItem := NO_ITEM;
            st.keyboardScope := NO_SCOPE;
            Set_Cursor (st, st.textCursor, text'Length, False);
            st.textWordSelect := False;
         end if;
      end if;

      focused := st.keyboardItem = id and then st.keyboardScope = scope;
      if focused then
         --  Caret and selection state belongs to the focused field.  An
         --  unrelated field (especially a shorter one rendered earlier in
         --  the frame) must never clamp or otherwise mutate it.
         Clamp_Text_State (st, text'Length);
         st.keyboardHeartbeat := True;
         if st.pointer.down and then
            st.activeItem = id and then st.activeScope = scope and then
            not st.pointer.pressed
         then
            idx := Text_Index_At_X (r, text, st.pointer.x);
            if st.textWordSelect then
               Select_Word (st, text, idx);
            else
               Set_Cursor (st, idx, text'Length, True);
            end if;
         end if;
      end if;

      return
        (hot       => hit,
         active    => focused,
         activated => hit and then st.pointer.released and then
                      st.activeItem = id and then
                      st.activeScope = scope);
   end Text_Field;

   function Text_Field_Key
      (st : in out UI_State;
       text : in out String;
       textLen : in out Natural;
       keyCode : Natural;
       modifiers : Natural := 0) return Boolean
   is
      shift : constant Boolean := (modifiers / KEYMOD_SHIFT) mod 2 /= 0;
      ctrl : constant Boolean := (modifiers / KEYMOD_CTRL) mod 2 /= 0;
      selStart : Natural := st.textSelectionStart;
      selEnd : Natural := st.textSelectionEnd;
      newPos : Natural;
   begin
      if not Is_Last_Widget_Focused (st) then
         return False;
      end if;

      Clamp_Text_State (st, textLen);
      selStart := Min (selStart, textLen);
      selEnd := Min (selEnd, textLen);

      if keyCode = KEY_BACKSPACE then
         if selStart < selEnd then
            Delete_Range (text, textLen, selStart, selEnd);
            Set_Cursor (st, selStart, textLen, False);
         elsif st.textCursor > 0 then
            Delete_Range (text, textLen, st.textCursor - 1, st.textCursor);
            Set_Cursor (st, st.textCursor - 1, textLen, False);
         else
            return False;
         end if;
         return True;
      elsif keyCode = KEY_DELETE then
         if selStart < selEnd then
            Delete_Range (text, textLen, selStart, selEnd);
            Set_Cursor (st, selStart, textLen, False);
         elsif st.textCursor < textLen then
            Delete_Range (text, textLen, st.textCursor, st.textCursor + 1);
            Set_Cursor (st, st.textCursor, textLen, False);
         else
            return False;
         end if;
         return True;
      elsif keyCode = KEY_LEFT then
         if not shift and then selStart < selEnd then
            newPos := selStart;
         elsif ctrl and then textLen > 0 then
            newPos := Word_Left (text (text'First .. text'First + textLen - 1),
                                 st.textCursor);
         elsif st.textCursor > 0 then
            newPos := st.textCursor - 1;
         else
            newPos := 0;
         end if;
         Set_Cursor (st, newPos, textLen, shift);
         return True;
      elsif keyCode = KEY_RIGHT then
         if not shift and then selStart < selEnd then
            newPos := selEnd;
         elsif ctrl and then textLen > 0 then
            newPos := Word_Right (text (text'First .. text'First + textLen - 1),
                                  st.textCursor);
         else
            newPos := Min (st.textCursor + 1, textLen);
         end if;
         Set_Cursor (st, newPos, textLen, shift);
         return True;
      elsif keyCode = KEY_HOME or else keyCode = KEY_UP then
         Set_Cursor (st, 0, textLen, shift);
         return True;
      elsif keyCode = KEY_END or else keyCode = KEY_DOWN then
         Set_Cursor (st, textLen, textLen, shift);
         return True;
      elsif keyCode = KEY_ENTER or else keyCode = KEY_TAB then
         Clear_Keyboard_Focus (st);
         return True;
      end if;

      return False;
   end Text_Field_Key;

   function Text_Field_Text
      (st : in out UI_State;
       text : in out String;
       textLen : in out Natural;
       codepoint : Natural) return Boolean
   is
      ch : Character;
      selStart : Natural := st.textSelectionStart;
      selEnd : Natural := st.textSelectionEnd;
   begin
      if not Is_Last_Widget_Focused (st) then
         return False;
      end if;

      if codepoint < Character'Pos (' ') or else codepoint >= 127 then
         return False;
      end if;

      Clamp_Text_State (st, textLen);
      selStart := Min (selStart, textLen);
      selEnd := Min (selEnd, textLen);

      if selStart < selEnd then
         Delete_Range (text, textLen, selStart, selEnd);
         Set_Cursor (st, selStart, textLen, False);
      end if;

      if textLen >= text'Length then
         return False;
      end if;

      ch := Character'Val (codepoint);
      if st.textCursor < textLen then
         for i in reverse st.textCursor + 1 .. textLen loop
            text (i + 1) := text (i);
         end loop;
      end if;

      text (st.textCursor + 1) := ch;
      textLen := textLen + 1;
      Set_Cursor (st, st.textCursor + 1, textLen, False);
      return True;
   end Text_Field_Text;

   function Checkbox
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       checked : in out Boolean;
       widgetID : Widget_ID := NO_ITEM) return CuBit.UI.Widget_Result
   is
      result : constant CuBit.UI.Widget_Result :=
        Button (st, bounds, widgetID);
   begin
      if result.activated then
         checked := not checked;
      end if;
      return result;
   end Checkbox;

   function Horizontal_Slider
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       value : in out Natural;
       minValue, maxValue : Natural;
       widgetID : Widget_ID := NO_ITEM) return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Resolve_ID (st, widgetID);
      scope : constant Scope_ID := st.currentScope;
      r : constant CuBit.UI.Rect := Offset_Rect (st, bounds);
      layout : CuBit.UI.Horizontal_Slider_Layout;
      hit : constant Boolean :=
         st.pointer.enabled and then
         CuBit.UI.Point_In_Rect (st.pointer.x, st.pointer.y, r);
      active : Boolean := False;
      span : Natural := 0;
      travel : Natural := 0;
      desiredX : Natural := 0;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.pressed then
            st.activeItem := id;
            st.activeScope := scope;
            layout := CuBit.UI.Layout_Horizontal_Slider
              (r, minValue, maxValue, value);
            if CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.thumb)
            then
               st.sliderGrabOffset := st.pointer.x - layout.thumb.x;
            else
               st.sliderGrabOffset := layout.thumb.w / 2;
            end if;
         end if;
      end if;

      active := st.pointer.down and then
                st.activeItem = id and then st.activeScope = scope;
      if active and then st.pointer.down and then maxValue > minValue then
         layout := CuBit.UI.Layout_Horizontal_Slider
           (r, minValue, maxValue, value);
         span := maxValue - minValue;
         travel := layout.maximumThumbX - layout.minimumThumbX;
         if st.pointer.x > st.sliderGrabOffset then
            desiredX := st.pointer.x - st.sliderGrabOffset;
         end if;
         desiredX := Natural'Max
           (layout.minimumThumbX,
            Natural'Min (desiredX, layout.maximumThumbX));
         if travel > 0 then
            value := minValue +
              ((desiredX - layout.minimumThumbX) * span) / travel;
         end if;
      end if;

      if st.keyboardItem = NO_ITEM then
         st.keyboardItem := id;
         st.keyboardScope := scope;
      end if;
      if st.keyboardItem = id and then st.keyboardScope = scope then
         st.keyboardHeartbeat := True;
      end if;

      return
        (hot       => hit,
         active    => active,
        activated => hit and then st.pointer.released and then active);
   end Horizontal_Slider;

   function Vertical_Scrollbar
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       value : in out Natural;
       minValue, maxValue : Natural;
       pageSize : Positive := 1;
       widgetID : Widget_ID := NO_ITEM) return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Resolve_ID (st, widgetID);
      scope : constant Scope_ID := st.currentScope;
      r : constant CuBit.UI.Rect := Offset_Rect (st, bounds);
      layout : constant CuBit.UI.Vertical_Scrollbar_Layout :=
        CuBit.UI.Layout_Vertical_Scrollbar
          (r, minValue, maxValue, value, pageSize);
      hit : constant Boolean :=
         st.pointer.enabled and then
         CuBit.UI.Point_In_Rect (st.pointer.x, st.pointer.y, r);
      active : Boolean := False;
      span : Natural := 0;
      travel : Natural := 0;
      relativeY : Natural := 0;
      pageStep : constant Natural := pageSize;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.pressed then
            st.activeItem := id;
            st.activeScope := scope;
            if CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.decrementButton)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Decrement;
               if value > minValue then
                  value := value - 1;
               end if;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.incrementButton)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Increment;
               if value < layout.maximumValue then
                  value := value + 1;
               end if;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.thumb)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Thumb;
               st.scrollbarGrabOffset := st.pointer.y - layout.thumb.y;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.track)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Track;
               if not CuBit.UI.Is_Empty (layout.thumb) and then
                 st.pointer.y < layout.thumb.y
               then
                  if value - minValue > pageStep then
                     value := value - pageStep;
                  else
                     value := minValue;
                  end if;
               elsif value < layout.maximumValue then
                  value := Natural'Min
                    (layout.maximumValue, value + pageStep);
               end if;
            end if;
         end if;
      end if;

      active := st.pointer.down and then
                st.activeItem = id and then st.activeScope = scope;
      if active and then st.pointer.down and then not st.pointer.pressed and then
        st.scrollbarPart = CuBit.UI.Scrollbar_Thumb and then
        layout.maximumValue > minValue and then
        not CuBit.UI.Is_Empty (layout.thumb)
      then
         span := layout.maximumValue - minValue;
         travel := layout.track.h - layout.thumb.h;
         if st.pointer.y > layout.track.y + st.scrollbarGrabOffset then
            relativeY := Natural'Min
              (st.pointer.y - layout.track.y - st.scrollbarGrabOffset,
               travel);
         end if;
         if travel > 0 then
            value := minValue + (relativeY * span) / travel;
         end if;
      end if;

      if st.keyboardItem = NO_ITEM then
         st.keyboardItem := id;
         st.keyboardScope := scope;
      end if;
      if st.keyboardItem = id and then st.keyboardScope = scope then
         st.keyboardHeartbeat := True;
      end if;

      return
        (hot       => hit,
         active    => active,
        activated => hit and then st.pointer.released and then active);
   end Vertical_Scrollbar;

   function Horizontal_Scrollbar
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       value : in out Natural;
       minValue, maxValue : Natural;
       pageSize : Positive := 1;
       widgetID : Widget_ID := NO_ITEM) return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Resolve_ID (st, widgetID);
      scope : constant Scope_ID := st.currentScope;
      r : constant CuBit.UI.Rect := Offset_Rect (st, bounds);
      layout : constant CuBit.UI.Horizontal_Scrollbar_Layout :=
        CuBit.UI.Layout_Horizontal_Scrollbar
          (r, minValue, maxValue, value, pageSize);
      hit : constant Boolean :=
         st.pointer.enabled and then
         CuBit.UI.Point_In_Rect (st.pointer.x, st.pointer.y, r);
      active : Boolean := False;
      span : Natural := 0;
      travel : Natural := 0;
      relativeX : Natural := 0;
      pageStep : constant Natural := pageSize;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.pressed then
            st.activeItem := id;
            st.activeScope := scope;
            if CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.decrementButton)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Decrement;
               if value > minValue then
                  value := value - 1;
               end if;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.incrementButton)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Increment;
               if value < layout.maximumValue then
                  value := value + 1;
               end if;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.thumb)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Thumb;
               st.scrollbarGrabOffset := st.pointer.x - layout.thumb.x;
            elsif CuBit.UI.Point_In_Rect
              (st.pointer.x, st.pointer.y, layout.track)
            then
               st.scrollbarPart := CuBit.UI.Scrollbar_Track;
               if not CuBit.UI.Is_Empty (layout.thumb) and then
                 st.pointer.x < layout.thumb.x
               then
                  if value - minValue > pageStep then
                     value := value - pageStep;
                  else
                     value := minValue;
                  end if;
               elsif value < layout.maximumValue then
                  value := Natural'Min
                    (layout.maximumValue, value + pageStep);
               end if;
            end if;
         end if;
      end if;

      active := st.pointer.down and then
                st.activeItem = id and then st.activeScope = scope;
      if active and then st.pointer.down and then not st.pointer.pressed and then
        st.scrollbarPart = CuBit.UI.Scrollbar_Thumb and then
        layout.maximumValue > minValue and then
        not CuBit.UI.Is_Empty (layout.thumb)
      then
         span := layout.maximumValue - minValue;
         travel := layout.track.w - layout.thumb.w;
         if st.pointer.x > layout.track.x + st.scrollbarGrabOffset then
            relativeX := Natural'Min
              (st.pointer.x - layout.track.x - st.scrollbarGrabOffset,
               travel);
         end if;
         if travel > 0 then
            value := minValue + (relativeX * span) / travel;
         end if;
      end if;

      if st.keyboardItem = NO_ITEM then
         st.keyboardItem := id;
         st.keyboardScope := scope;
      end if;
      if st.keyboardItem = id and then st.keyboardScope = scope then
         st.keyboardHeartbeat := True;
      end if;

      return
        (hot       => hit,
         active    => active,
         activated => hit and then st.pointer.released and then active);
   end Horizontal_Scrollbar;

   function Active_Scrollbar_Part
      (st : UI_State) return CuBit.UI.Scrollbar_Part
   is (st.scrollbarPart);
end CuBit.UI.State;
