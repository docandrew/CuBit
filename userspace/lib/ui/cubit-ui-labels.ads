------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Passive text controls
------------------------------------------------------------------------------
package CuBit.UI.Labels is
   procedure Label
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       muted : Boolean := False);
end CuBit.UI.Labels;
