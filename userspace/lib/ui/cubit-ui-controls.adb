------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small control map for hit testing and damage lookup
------------------------------------------------------------------------------
package body CuBit.UI.Controls is
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
      m.entries := (others => (others => <>));
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
      for i in m.entries'Range loop
         if m.entries (i).enabled and then m.entries (i).id = id then
            m.valid := False;
            return;
         end if;
      end loop;

      for i in m.entries'Range loop
         if not m.entries (i).enabled then
            m.entries (i) :=
              (id           => id,
               bounds       => clippedBounds,
               visualDamage => clippedBounds,
               actionDamage => clippedAction,
               cursor       => cursor,
               continuousAction => continuousAction,
               enabled      => True);
            return;
         end if;
      end loop;
      m.valid := False;
   end Add;

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID
   is
   begin
      if not m.valid then
         return NO_CONTROL;
      end if;
      for i in reverse m.entries'Range loop
         if m.entries (i).enabled and then
            CuBit.UI.Point_In_Rect (x, y, m.entries (i).bounds)
         then
            return m.entries (i).id;
         end if;
      end loop;

      return NO_CONTROL;
   end Hit;

   function Bounds
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
   begin
      if not m.valid or else id = NO_CONTROL then
         return (others => 0);
      end if;

      for i in m.entries'Range loop
         if m.entries (i).enabled and then m.entries (i).id = id then
            return m.entries (i).bounds;
         end if;
      end loop;
      return (others => 0);
   end Bounds;

   function Visual_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
   begin
      if not m.valid or else id = NO_CONTROL then
         return (others => 0);
      end if;

      for i in m.entries'Range loop
         if m.entries (i).enabled and then m.entries (i).id = id then
            return m.entries (i).visualDamage;
         end if;
      end loop;

      return (others => 0);
   end Visual_Damage;

   function Action_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
   begin
      if not m.valid or else id = NO_CONTROL then
         return (others => 0);
      end if;

      for i in m.entries'Range loop
         if m.entries (i).enabled and then m.entries (i).id = id then
            return m.entries (i).actionDamage;
         end if;
      end loop;

      return (others => 0);
   end Action_Damage;

   function Cursor
      (m : Control_Map; id : Control_ID)
      return CuBit.UI.Pointer_Cursor_Style
   is
   begin
      if m.valid and then id /= NO_CONTROL then
         for i in m.entries'Range loop
            if m.entries (i).enabled and then m.entries (i).id = id then
               return m.entries (i).cursor;
            end if;
         end loop;
      end if;
      return CuBit.UI.Pointer_Default;
   end Cursor;

   function Has_Continuous_Action
      (m : Control_Map; id : Control_ID) return Boolean
   is
   begin
      if m.valid and then id /= NO_CONTROL then
         for i in m.entries'Range loop
            if m.entries (i).enabled and then m.entries (i).id = id then
               return m.entries (i).continuousAction;
            end if;
         end loop;
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
