------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's hottest drawing primitive: a one-pixel-wide
--  vertical span (a plot column's line step, band or bar), written straight
--  into the canvas at 1:1 density after the toolkit's own clipping
--  (CuBit.UI.Clamp_Rect); any other density goes through Fill_Rect.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.UI;

package Dash_Pixels is
   procedure Span (C : CuBit.UI.Canvas; X, Y, Height : Natural; Ink : CuBit.UI.Color);
end Dash_Pixels;
