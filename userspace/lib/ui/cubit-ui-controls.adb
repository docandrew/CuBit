------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small control map for hit testing and damage lookup
------------------------------------------------------------------------------
package body CuBit.UI.Controls is
   function Find
      (entries : Control_Entries;
       count : Control_Count;
       id : Control_ID) return Integer
   is
   begin
      if id /= NO_CONTROL and then count > 0 then
         for i in Control_Index'First .. Control_Index (count) loop
            if entries (i).enabled and then entries (i).id = id then
               return i;
            end if;
         end loop;
      end if;
      return -1;
   end Find;

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

   procedure Clear (m : in out Control_Map) is
   begin
      --  Keep the last committed state available while declarative widget
      --  calls rebuild current geometry. Retained controls recover their
      --  behavior state by stable ID rather than by draw order.
      if m.entryCount > 0 then
         m.retainedEntries
           (Control_Index'First .. Control_Index (m.entryCount)) :=
           m.entries (Control_Index'First .. Control_Index (m.entryCount));
      end if;
      m.retainedCount := m.entryCount;
      m.entryCount := 0;
      m.valid := True;
   end Clear;

   function Is_Valid (m : Control_Map) return Boolean is (m.valid);

   procedure Add
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;
       continuousAction : Boolean := False)
   is
      clippedBounds : CuBit.UI.Rect := bounds;
      clippedAction : CuBit.UI.Rect := actionDamage;
   begin
      if not m.valid or else id = NO_CONTROL or else
        CuBit.UI.Is_Empty (bounds)
      then
         return;
      end if;

      if not CuBit.UI.Is_Empty (actionDamage) then
         clippedBounds := Intersect (bounds, actionDamage);
         if CuBit.UI.Is_Empty (clippedBounds) then
            return;
         end if;
      else
         clippedAction := bounds;
      end if;

      --  Duplicate IDs make capture, cursor, and damage ownership ambiguous.
      --  Do not silently replace the earlier control.
      if Find (m.entries, m.entryCount, id) >= 0 or else
        m.entryCount = MAX_CONTROLS
      then
         m.valid := False;
         return;
      end if;

      m.entryCount := m.entryCount + 1;
      m.entries (Control_Index (m.entryCount)) :=
        (id           => id,
         bounds       => clippedBounds,
         visualDamage => clippedBounds,
         actionDamage => clippedAction,
         cursor       => cursor,
         continuousAction => continuousAction,
         behavior     => Render_Driven,
         value        => 0,
         minimumValue => 0,
         maximumValue => 0,
         pageSize     => 1,
         scrollbarPart => CuBit.UI.Scrollbar_None,
         grabOffset   => 0,
         coordinateOrigin => 0,
         dragOffset   => 0,
         active       => False,
         activated    => False,
         pendingValue => False,
         enabled      => True);
   end Add;

   procedure Add_Vertical_Scrollbar
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       value : Natural;
       minValue, maxValue : Natural;
       pageSize : Positive := 1)
   is
      currentIndex : Integer;
      retainedIndex : Integer;
      layout : CuBit.UI.Vertical_Scrollbar_Layout;
   begin
      Add
        (m, id, bounds, actionDamage, continuousAction => True);
      if not m.valid then
         return;
      end if;

      currentIndex := Find (m.entries, m.entryCount, id);
      if currentIndex < 0 then
         return;
      end if;
      retainedIndex := Find (m.retainedEntries, m.retainedCount, id);

      declare
         current : Control_Entry
           renames m.entries (Control_Index (currentIndex));
      begin
         current.behavior := Retained_Vertical_Scrollbar;
         current.minimumValue := minValue;
         current.maximumValue := maxValue;
         current.pageSize := pageSize;

         if retainedIndex >= 0 and then
           m.retainedEntries (Control_Index (retainedIndex)).behavior =
             Retained_Vertical_Scrollbar
         then
            declare
               retained : Control_Entry renames
                 m.retainedEntries (Control_Index (retainedIndex));
            begin
               current.value :=
                 (if retained.pendingValue then retained.value else value);
               current.scrollbarPart := retained.scrollbarPart;
               current.grabOffset := retained.grabOffset;
               current.active := retained.active;
               current.pendingValue := retained.pendingValue;
            end;
         else
            current.value := value;
         end if;

         layout := CuBit.UI.Layout_Vertical_Scrollbar
           (current.bounds, minValue, maxValue, current.value, pageSize);
         current.value := Natural'Max
           (minValue, Natural'Min (current.value, layout.maximumValue));
      end;
   end Add_Vertical_Scrollbar;

   procedure Add_Horizontal_Drag
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       value : Natural;
       minValue, maxValue : Natural;
       coordinateOrigin : Natural)
   is
      currentIndex : Integer;
      retainedIndex : Integer;
   begin
      Add
        (m, id, bounds, actionDamage,
         cursor => CuBit.UI.Pointer_Resize_Horizontal,
         continuousAction => True);
      if not m.valid then
         return;
      end if;

      currentIndex := Find (m.entries, m.entryCount, id);
      if currentIndex < 0 then
         return;
      end if;
      retainedIndex := Find (m.retainedEntries, m.retainedCount, id);

      declare
         current : Control_Entry
           renames m.entries (Control_Index (currentIndex));
      begin
         current.behavior := Retained_Horizontal_Drag;
         current.minimumValue := minValue;
         current.maximumValue := Natural'Max (minValue, maxValue);
         current.coordinateOrigin := coordinateOrigin;

         if retainedIndex >= 0 and then
           m.retainedEntries (Control_Index (retainedIndex)).behavior =
             Retained_Horizontal_Drag
         then
            declare
               retained : Control_Entry renames
                 m.retainedEntries (Control_Index (retainedIndex));
            begin
               current.value :=
                 (if retained.pendingValue then retained.value else value);
               current.dragOffset := retained.dragOffset;
               current.active := retained.active;
               current.pendingValue := retained.pendingValue;
            end;
         else
            current.value := value;
         end if;
         current.value := Natural'Max
           (current.minimumValue,
            Natural'Min (current.value, current.maximumValue));
      end;
   end Add_Horizontal_Drag;

   procedure Add_Button
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default)
   is
      currentIndex : Integer;
      retainedIndex : Integer;
   begin
      Add (m, id, bounds, actionDamage, cursor);
      if not m.valid then
         return;
      end if;

      currentIndex := Find (m.entries, m.entryCount, id);
      if currentIndex < 0 then
         return;
      end if;
      retainedIndex := Find (m.retainedEntries, m.retainedCount, id);

      declare
         current : Control_Entry
           renames m.entries (Control_Index (currentIndex));
      begin
         current.behavior := Retained_Button;
         if retainedIndex >= 0 and then
           m.retainedEntries (Control_Index (retainedIndex)).behavior =
             Retained_Button
         then
            current.active :=
              m.retainedEntries (Control_Index (retainedIndex)).active;
            current.activated :=
              m.retainedEntries (Control_Index (retainedIndex)).activated;
         end if;
      end;
   end Add_Button;

   procedure Dispatch_Pointer
      (m : in out Control_Map;
       id : Control_ID;
       action : Pointer_Action;
       x, y : Natural;
       changed : out Boolean;
       handled : out Boolean)
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
      oldValue : Natural;
      layout : CuBit.UI.Vertical_Scrollbar_Layout;
      span : Natural;
      travel : Natural;
      relativeY : Natural := 0;
      pageStep : Natural;
   begin
      changed := False;
      handled := False;
      if not m.valid or else index < 0 then
         return;
      end if;

      declare
         control : Control_Entry renames
           m.entries (Control_Index (index));
      begin
         if control.behavior = Retained_Button then
            handled := True;
            case action is
               when Pointer_Press =>
                  if CuBit.UI.Point_In_Rect (x, y, control.bounds) then
                     control.active := True;
                  end if;
               when Pointer_Move =>
                  null;
               when Pointer_Release =>
                  changed := control.active and then
                    CuBit.UI.Point_In_Rect (x, y, control.bounds);
                  control.active := False;
                  if changed then
                     control.activated := True;
                  end if;
               when Pointer_Cancel =>
                  control.active := False;
                  control.activated := False;
            end case;
            return;
         end if;

         if control.behavior /= Retained_Vertical_Scrollbar then
            if control.behavior = Retained_Horizontal_Drag then
               handled := True;
               oldValue := control.value;
               case action is
                  when Pointer_Press =>
                     if not CuBit.UI.Point_In_Rect
                       (x, y, control.bounds)
                     then
                        return;
                     end if;
                     control.active := True;
                     control.dragOffset := Long_Long_Integer (x) -
                       Long_Long_Integer (control.coordinateOrigin) -
                       Long_Long_Integer (control.value);

                  when Pointer_Move =>
                     if control.active then
                        declare
                           desired : constant Long_Long_Integer :=
                             Long_Long_Integer (x) -
                             control.dragOffset -
                             Long_Long_Integer (control.coordinateOrigin);
                        begin
                           if desired <= Long_Long_Integer
                             (control.minimumValue)
                           then
                              control.value := control.minimumValue;
                           elsif desired >= Long_Long_Integer
                             (control.maximumValue)
                           then
                              control.value := control.maximumValue;
                           else
                              control.value := Natural (desired);
                           end if;
                        end;
                     end if;

                  when Pointer_Release =>
                     control.active := False;
                     control.dragOffset := 0;
                  when Pointer_Cancel =>
                     control.active := False;
                     control.dragOffset := 0;
               end case;
               changed := control.value /= oldValue;
               if changed then
                  control.pendingValue := True;
               end if;
               return;
            else
               return;
            end if;
         end if;
         handled := True;
         oldValue := control.value;
         layout := CuBit.UI.Layout_Vertical_Scrollbar
           (control.bounds, control.minimumValue, control.maximumValue,
            control.value, control.pageSize);
         pageStep := control.pageSize;

         case action is
            when Pointer_Press =>
               if not CuBit.UI.Point_In_Rect (x, y, control.bounds) then
                  return;
               end if;
               control.active := False;
               control.scrollbarPart := CuBit.UI.Scrollbar_None;
               if CuBit.UI.Point_In_Rect
                 (x, y, layout.decrementButton) and then
                 control.value > control.minimumValue
               then
                  control.active := True;
                  control.scrollbarPart := CuBit.UI.Scrollbar_Decrement;
                  control.value := control.value - 1;
               elsif CuBit.UI.Point_In_Rect
                 (x, y, layout.incrementButton) and then
                 control.value < layout.maximumValue
               then
                  control.active := True;
                  control.scrollbarPart := CuBit.UI.Scrollbar_Increment;
                  control.value := control.value + 1;
               elsif layout.maximumValue > control.minimumValue and then
                 CuBit.UI.Point_In_Rect (x, y, layout.thumb)
               then
                  control.active := True;
                  control.scrollbarPart := CuBit.UI.Scrollbar_Thumb;
                  control.grabOffset := y - layout.thumb.y;
               elsif CuBit.UI.Point_In_Rect (x, y, layout.track) then
                  if not CuBit.UI.Is_Empty (layout.thumb) and then
                    y < layout.thumb.y and then
                    control.value > control.minimumValue
                  then
                     control.active := True;
                     control.scrollbarPart := CuBit.UI.Scrollbar_Track;
                     if control.value - control.minimumValue > pageStep then
                        control.value := control.value - pageStep;
                     else
                        control.value := control.minimumValue;
                     end if;
                  elsif not CuBit.UI.Is_Empty (layout.thumb) and then
                    y >= layout.thumb.y + layout.thumb.h and then
                    control.value < layout.maximumValue
                  then
                     control.active := True;
                     control.scrollbarPart := CuBit.UI.Scrollbar_Track;
                     control.value := Natural'Min
                       (layout.maximumValue, control.value + pageStep);
                  end if;
               end if;

            when Pointer_Move =>
               if control.active and then
                 control.scrollbarPart = CuBit.UI.Scrollbar_Thumb and then
                 layout.maximumValue > control.minimumValue and then
                 not CuBit.UI.Is_Empty (layout.thumb)
               then
                  span := layout.maximumValue - control.minimumValue;
                  travel := layout.track.h - layout.thumb.h;
                  if y > layout.track.y + control.grabOffset then
                     relativeY := Natural'Min
                       (y - layout.track.y - control.grabOffset, travel);
                  end if;
                  if travel > 0 then
                     control.value := control.minimumValue +
                       (relativeY * span) / travel;
                  end if;
               end if;

            when Pointer_Release =>
               control.active := False;
               control.scrollbarPart := CuBit.UI.Scrollbar_None;
               control.grabOffset := 0;

            when Pointer_Cancel =>
               control.active := False;
               control.scrollbarPart := CuBit.UI.Scrollbar_None;
               control.grabOffset := 0;
         end case;

         changed := control.value /= oldValue;
         if changed then
            control.pendingValue := True;
         end if;
      end;
   end Dispatch_Pointer;

   procedure Take_Value
      (m : in out Control_Map;
       id : Control_ID;
       value : out Natural;
       available : out Boolean)
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      value := 0;
      available := False;
      if m.valid and then index >= 0 and then
        m.entries (Control_Index (index)).behavior /= Render_Driven
      then
         value := m.entries (Control_Index (index)).value;
         m.entries (Control_Index (index)).pendingValue := False;
         available := True;
      end if;
   end Take_Value;

   function Is_Active
      (m : Control_Map; id : Control_ID) return Boolean
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      return m.valid and then index >= 0 and then
        m.entries (Control_Index (index)).active;
   end Is_Active;

   function Take_Activated
      (m : in out Control_Map; id : Control_ID) return Boolean
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
      result : Boolean := False;
   begin
      if m.valid and then index >= 0 and then
        m.entries (Control_Index (index)).behavior = Retained_Button
      then
         result := m.entries (Control_Index (index)).activated;
         m.entries (Control_Index (index)).activated := False;
      end if;
      return result;
   end Take_Activated;

   function Active_Scrollbar_Part
      (m : Control_Map; id : Control_ID) return CuBit.UI.Scrollbar_Part
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if m.valid and then index >= 0 then
         return m.entries (Control_Index (index)).scrollbarPart;
      end if;
      return CuBit.UI.Scrollbar_None;
   end Active_Scrollbar_Part;

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID
   is
   begin
      if not m.valid then
         return NO_CONTROL;
      end if;
      if m.entryCount > 0 then
         for i in reverse Control_Index'First ..
           Control_Index (m.entryCount)
         loop
            if CuBit.UI.Point_In_Rect (x, y, m.entries (i).bounds) then
               return m.entries (i).id;
            end if;
         end loop;
      end if;

      return NO_CONTROL;
   end Hit;

   function Bounds
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if not m.valid or else index < 0 then
         return (others => 0);
      end if;
      return m.entries (Control_Index (index)).bounds;
   end Bounds;

   function Visual_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if not m.valid or else index < 0 then
         return (others => 0);
      end if;
      return m.entries (Control_Index (index)).visualDamage;
   end Visual_Damage;

   function Action_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if not m.valid or else index < 0 then
         return (others => 0);
      end if;
      return m.entries (Control_Index (index)).actionDamage;
   end Action_Damage;

   function Cursor
      (m : Control_Map; id : Control_ID)
      return CuBit.UI.Pointer_Cursor_Style
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if m.valid and then index >= 0 then
         return m.entries (Control_Index (index)).cursor;
      end if;
      return CuBit.UI.Pointer_Default;
   end Cursor;

   function Has_Continuous_Action
      (m : Control_Map; id : Control_ID) return Boolean
   is
      index : constant Integer := Find (m.entries, m.entryCount, id);
   begin
      if m.valid and then index >= 0 then
         return m.entries (Control_Index (index)).continuousAction;
      end if;
      return False;
   end Has_Continuous_Action;

   procedure Mark_Visual_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID)
   is
   begin
      dirty := CuBit.UI.Union_Rect (dirty, Visual_Damage (m, id));
   end Mark_Visual_Dirty;

   procedure Mark_Action_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID)
   is
   begin
      dirty := CuBit.UI.Union_Rect (dirty, Action_Damage (m, id));
   end Mark_Action_Dirty;
end CuBit.UI.Controls;
