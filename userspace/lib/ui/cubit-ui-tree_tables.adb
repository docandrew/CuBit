------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Tree_Tables is
   function Position (V : View; I : Item_Index) return Natural is
   begin
      for K in 1 .. V.Visible_Total loop
         if V.Visible (K) = I then
            return K;
         end if;
      end loop;
      return 0;
   end Position;

   procedure Ensure_Visible (V : in out View; Rows : Natural) is
      K : constant Natural := Position (V, V.Selected);
      Shown : constant Positive := Natural'Max (1, Rows);
   begin
      if K = 0 then
         return;
      elsif K <= V.Top then
         V.Top := K - 1;
      elsif K > V.Top + Shown then
         V.Top := K - Shown;
      end if;
   end Ensure_Visible;
end CuBit.UI.Tree_Tables;
