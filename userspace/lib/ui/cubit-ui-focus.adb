------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body CuBit.UI.Focus with SPARK_Mode is

   function Position (L : Order_List; Count : Order_Count; ID : Control_ID) return Order_Count
   with Post => (if Position'Result /= 0 then Position'Result <= Count and then L (Position'Result) = ID)
                and then (if Position'Result = 0 then (for all I in 1 .. Count => L (I) /= ID));

   function Position (L : Order_List; Count : Order_Count; ID : Control_ID) return Order_Count is
   begin
      for I in 1 .. Count loop
         if L (I) = ID then
            return I;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => L (J) /= ID);
      end loop;
      return 0;
   end Position;

   procedure Begin_Order (F : in out Focus_State) is
   begin
      F.Building_Count := 0;
   end Begin_Order;

   procedure Register (F : in out Focus_State; ID : Control_ID) is
   begin
      if ID /= No_Control and then F.Building_Count < Maximum_Order
        and then Position (F.Building, F.Building_Count, ID) = 0
      then
         F.Building_Count := F.Building_Count + 1;
         F.Building (F.Building_Count) := ID;
      end if;
   end Register;

   procedure End_Order (F : in out Focus_State) is
      Old_Place : constant Order_Count := Position (F.Order, F.Count, F.Focused);
   begin
      F.Order := F.Building;
      F.Count := F.Building_Count;
      if F.Focused /= No_Control and then Position (F.Order, F.Count, F.Focused) = 0 then
         if F.Count = 0 then
            F.Focused := No_Control;
         else
            F.Focused := F.Order (Natural'Max (1, Natural'Min (Old_Place, F.Count)));
         end if;
      end if;
   end End_Order;

   procedure Move (F : in out Focus_State; Forward : Boolean) is
      Place : constant Order_Count := Position (F.Order, F.Count, F.Focused);
   begin
      if F.Count = 0 then
         return;
      elsif Place = 0 then
         F.Focused := (if Forward then F.Order (1) else F.Order (F.Count));
      elsif Forward then
         F.Focused := F.Order (if Place = F.Count then 1 else Place + 1);
      else
         F.Focused := F.Order (if Place = 1 then F.Count else Place - 1);
      end if;
   end Move;

   procedure Set (F : in out Focus_State; ID : Control_ID) is
   begin
      if Position (F.Order, F.Count, ID) /= 0 then
         F.Focused := ID;
      end if;
   end Set;

   procedure Clear (F : in out Focus_State) is
   begin
      F.Focused := No_Control;
   end Clear;
end CuBit.UI.Focus;
