------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Table controls
------------------------------------------------------------------------------
package body CuBit.UI.Tables is
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
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := rowIndex;
      end if;
      CuBit.UI.Draw_Table_Row
        (c, bounds, colors, selectedIndex = rowIndex, result.hot, c1, c2, c3);
   end Row;
end CuBit.UI.Tables;
