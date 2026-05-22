------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Passive text controls
------------------------------------------------------------------------------
package body CuBit.UI.Labels is
   procedure Label
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       muted : Boolean := False)
   is
      fg : constant CuBit.UI.Color :=
         (if muted then colors.muted else colors.text);
      bg : constant CuBit.UI.Color := colors.face;
      y : Natural := bounds.y;
   begin
      if CuBit.UI.Is_Empty (bounds) then
         return;
      end if;

      if bounds.h > CuBit.UI.UI_Text_Height then
         y := bounds.y + (bounds.h - CuBit.UI.UI_Text_Height) / 2;
      end if;
      CuBit.UI.Draw_UI_Text (c, bounds.x, y, text, fg, bg);
   end Label;
end CuBit.UI.Labels;
