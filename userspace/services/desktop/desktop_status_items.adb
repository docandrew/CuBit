------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the Deskbar's status button can show (see the spec).
------------------------------------------------------------------------------
package body Desktop_Status_Items with SPARK_Mode is
   function Count (Shown : Presence) return Natural is
      Total : Natural := 0;
   begin
      for I in Item loop
         if Shown (I) then
            Total := Total + 1;
         end if;
         pragma Loop_Invariant (Total <= Item'Pos (I) + 1);
      end loop;
      return Total;
   end Count;
end Desktop_Status_Items;
