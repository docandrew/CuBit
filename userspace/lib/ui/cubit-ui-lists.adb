------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  List controls
------------------------------------------------------------------------------
package body CuBit.UI.Lists is
   procedure List_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       itemIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result)
   is
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := itemIndex;
      end if;
      CuBit.UI.Draw_List_Item
        (c, bounds, colors, selectedIndex = itemIndex, result.hot, label);
   end List_Item;
end CuBit.UI.Lists;
