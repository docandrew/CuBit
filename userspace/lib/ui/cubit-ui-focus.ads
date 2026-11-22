------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Keyboard focus for an immediate-mode view: Tab order is the order the
--  view registers its focusable controls while it renders (CuBit.UI.Controls
--  IDs), so it always follows the layout; Next/Previous move along it and
--  wrap. A control the view stopped drawing loses focus to its nearest
--  successor (or the first). The view asks Is_Focused to draw the ring
--  (Draw_Focus_Ring) and to route keys; nothing here sees input itself.
------------------------------------------------------------------------------
with CuBit.UI.Controls;

package CuBit.UI.Focus with SPARK_Mode is
   subtype Control_ID is CuBit.UI.Controls.Control_ID;
   No_Control : constant Control_ID := CuBit.UI.Controls.NO_CONTROL;
   Maximum_Order : constant := CuBit.UI.Controls.MAX_CONTROLS;
   subtype Order_Count is Natural range 0 .. Maximum_Order;
   subtype Order_Index is Positive range 1 .. Maximum_Order;
   type Order_List is array (Order_Index) of Control_ID;

   type Focus_State is record
      Focused : Control_ID := No_Control;
      --  This render's order, and the last complete one.
      Building, Order : Order_List := [others => No_Control];
      Building_Count, Count : Order_Count := 0;
   end record;

   --  Start a render's registration.
   procedure Begin_Order (F : in out Focus_State)
   with Post => F.Building_Count = 0 and then F.Focused = F'Old.Focused
                and then F.Count = F'Old.Count and then F.Order = F'Old.Order;

   --  Register a focusable control (in Tab order); No_Control and IDs
   --  already registered are ignored.
   procedure Register (F : in out Focus_State; ID : Control_ID)
   with Post => F.Focused = F'Old.Focused;

   --  End the render: the order becomes current; a focused control that
   --  was not registered gives focus to the one now at its place.
   procedure End_Order (F : in out Focus_State)
   with Post => F.Count = F'Old.Building_Count
                and then (F.Focused = No_Control or else
                          (for some I in 1 .. F.Count => F.Order (I) = F.Focused));

   function Is_Focused (F : Focus_State; ID : Control_ID) return Boolean is
     (ID /= No_Control and then F.Focused = ID);

   --  Tab (Forward) or Shift+Tab; from nothing, the first (or last).
   procedure Move (F : in out Focus_State; Forward : Boolean)
   with Post => (if F.Count = 0 then F.Focused = F'Old.Focused
                 else (for some I in 1 .. F.Count => F.Order (I) = F.Focused));

   --  Focus ID (a click on it); ignored when it is not in the order.
   procedure Set (F : in out Focus_State; ID : Control_ID)
   with Post => (if (for some I in 1 .. F.Count => F.Order (I) = ID) then F.Focused = ID
                 else F.Focused = F'Old.Focused);

   procedure Clear (F : in out Focus_State) with Post => F.Focused = No_Control;
end CuBit.UI.Focus;
