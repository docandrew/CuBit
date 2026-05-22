------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tree controls
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.State;

package CuBit.UI.Trees is
   procedure Tree_Item
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
       depth : Natural := 0;
       expanded : Boolean := False;
       hasChildren : Boolean := False;
       result : out CuBit.UI.Widget_Result);
end CuBit.UI.Trees;
