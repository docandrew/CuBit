------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small control map for hit testing and damage lookup
------------------------------------------------------------------------------
package body CuBit.UI.Controls is
   procedure Clear (m : in out Control_Map) is
   begin
      m.entries := (others => (others => <>));
   end Clear;

   procedure Add
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect)
   is
   begin
      if id = NO_CONTROL or else CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      for i in m.entries'Range loop
         if not m.entries (i).enabled or else m.entries (i).id = id then
            m.entries (i) :=
              (id      => id,
               bounds  => bounds,
               damage  => (if CuBit.UI.Is_Empty (damage) then bounds
                           else damage),
               enabled => True);
            return;
         end if;
      end loop;
   end Add;

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID
   is
   begin
      for i in reverse m.entries'Range loop
         if m.entries (i).enabled and then
            CuBit.UI.Point_In_Rect (x, y, m.entries (i).bounds)
         then
            return m.entries (i).id;
         end if;
      end loop;

      return NO_CONTROL;
   end Hit;

   function Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect
   is
   begin
      if id = NO_CONTROL then
         return (others => 0);
      end if;

      for i in m.entries'Range loop
         if m.entries (i).enabled and then m.entries (i).id = id then
            return m.entries (i).damage;
         end if;
      end loop;

      return (others => 0);
   end Damage;

   procedure Mark_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID)
   is
   begin
      dirty := CuBit.UI.Union_Rect (dirty, Damage (m, id));
   end Mark_Dirty;
end CuBit.UI.Controls;
