------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Immediate-mode UI state
------------------------------------------------------------------------------
package body CuBit.UI.State is
   procedure Begin_Frame (st : in out UI_State) is
   begin
      st.hotItem := NO_ITEM;
      st.hotScope := NO_SCOPE;
      st.rootLastID := NO_ITEM;
      st.lastIDs := (others => NO_ITEM);
      st.currentScope := NO_SCOPE;
      st.scopeXOffset := 0;
      st.scopeYOffset := 0;
      st.scopeError := False;
   end Begin_Frame;

   procedure Finish_Frame (st : in out UI_State) is
   begin
      if not st.pointer.down then
         st.activeItem := NO_ITEM;
         st.activeScope := NO_SCOPE;
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
      st.pointer := (x => x, y => y, down => down,
                     pressed => pressed, released => released,
                     enabled => enabled);
   end Set_Pointer;

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
      (st : in out UI_State; bounds : CuBit.UI.Rect)
      return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Next_ID (st);
      scope : constant Scope_ID := st.currentScope;
      hit : constant Boolean := st.pointer.enabled and then Region_Hit (st, bounds);
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.down then
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
   end Clear_Keyboard_Focus;

   function Text_Field
      (st : in out UI_State; bounds : CuBit.UI.Rect)
      return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Next_ID (st);
      scope : constant Scope_ID := st.currentScope;
      hit : constant Boolean :=
         st.pointer.enabled and then Region_Hit (st, bounds);
      focused : Boolean;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.down then
            st.activeItem := id;
            st.activeScope := scope;
         end if;

         if st.pointer.pressed then
            st.keyboardItem := id;
            st.keyboardScope := scope;
         end if;
      elsif st.pointer.pressed then
         --  Pointer activation outside the field commits the click to the
         --  target under the cursor and removes keyboard focus from this
         --  field. Dialog focus rules can grow more nuanced later.
         if st.keyboardItem = id and then st.keyboardScope = scope then
            st.keyboardItem := NO_ITEM;
            st.keyboardScope := NO_SCOPE;
         end if;
      end if;

      focused := st.keyboardItem = id and then st.keyboardScope = scope;
      if focused then
         st.keyboardHeartbeat := True;
      end if;

      return
        (hot       => hit,
         active    => focused,
         activated => hit and then st.pointer.released and then
                      st.activeItem = id and then
                      st.activeScope = scope);
   end Text_Field;

   function Text_Field_Key
      (st : UI_State;
       text : in out String;
       textLen : in out Natural;
       keyCode : Natural;
       modifiers : Natural := 0) return Boolean
   is
      pragma Unreferenced (modifiers);
      KEY_BACKSPACE : constant Natural := 16#0E#;
   begin
      if not Is_Last_Widget_Focused (st) then
         return False;
      end if;

      if keyCode = KEY_BACKSPACE and then textLen > 0 then
         text (textLen) := ' ';
         textLen := textLen - 1;
         return True;
      end if;

      return False;
   end Text_Field_Key;

   function Text_Field_Text
      (st : UI_State;
       text : in out String;
       textLen : in out Natural;
       codepoint : Natural) return Boolean
   is
   begin
      if not Is_Last_Widget_Focused (st) then
         return False;
      end if;

      if codepoint < Character'Pos (' ') or else codepoint >= 127 then
         return False;
      end if;

      if textLen < text'Length then
         textLen := textLen + 1;
         text (textLen) := Character'Val (codepoint);
         return True;
      end if;

      return False;
   end Text_Field_Text;

   function Checkbox
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       checked : in out Boolean) return CuBit.UI.Widget_Result
   is
      result : constant CuBit.UI.Widget_Result := Button (st, bounds);
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
       minValue, maxValue : Natural) return CuBit.UI.Widget_Result
   is
      id : constant Widget_ID := Next_ID (st);
      scope : constant Scope_ID := st.currentScope;
      r : constant CuBit.UI.Rect := Offset_Rect (st, bounds);
      hit : constant Boolean :=
         st.pointer.enabled and then
         CuBit.UI.Point_In_Rect (st.pointer.x, st.pointer.y, r);
      active : Boolean := False;
      span : Natural := 0;
      relativeX : Natural := 0;
   begin
      if hit then
         st.hotItem := id;
         st.hotScope := scope;

         if st.activeItem = NO_ITEM and then st.pointer.down then
            st.activeItem := id;
            st.activeScope := scope;
         end if;
      end if;

      active := st.pointer.down and then
                st.activeItem = id and then st.activeScope = scope;
      if active and then st.pointer.down and then maxValue > minValue then
         span := maxValue - minValue;
         if st.pointer.x > r.x then
            relativeX := Natural'Min (st.pointer.x - r.x, r.w);
         end if;
         if r.w > 0 then
            value := minValue + (relativeX * span) / r.w;
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
end CuBit.UI.State;
