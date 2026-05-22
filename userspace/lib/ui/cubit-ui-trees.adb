------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tree controls
------------------------------------------------------------------------------
package body CuBit.UI.Trees is
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
       result : out CuBit.UI.Widget_Result)
   is
      bg : CuBit.UI.Color := colors.face;
      fg : CuBit.UI.Color := colors.text;
      indent : constant Natural := depth * 14;
      glyph : Character := ' ';
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button (st, bounds);
      if result.activated then
         selectedIndex := itemIndex;
      end if;

      if selectedIndex = itemIndex then
         bg := colors.accent;
         fg := colors.edge;
      elsif result.hot then
         bg := colors.panel;
      end if;

      if hasChildren then
         glyph := (if expanded then '-' else '+');
      end if;

      CuBit.UI.Fill_Rect (c, bounds, bg);
      CuBit.UI.Draw_UI_Text
        (c, bounds.x + indent + 4, bounds.y + 3,
         (1 => glyph), fg, bg);
      CuBit.UI.Draw_UI_Text
        (c, bounds.x + indent + 18, bounds.y + 3,
         label, fg, bg);
   end Tree_Item;
end CuBit.UI.Trees;
