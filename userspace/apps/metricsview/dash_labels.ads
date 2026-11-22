------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Cached glyph runs for the metrics viewer's axis and legend labels: a
--  label (its text, ink and the solid card it sits on) is rasterized once
--  and drawn again as a block copy, so a zoom or a pan that moves every
--  tick label costs copies, not glyph blending.
--
--  @description
--  UI thread only. Slots are made once (Slots of at most Slot_Width x
--  Slot_Height pixels) and reused least recently made first; a longer
--  label, or a canvas at another density than 1:1, is drawn directly.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.UI;

package Dash_Labels is
   Slots : constant := 192;
   Slot_Width : constant := 160;
   Slot_Height : constant := 32;
   Maximum_Text : constant := 32;

   --  Text at X, Y (its line box's top-left), as Draw_UI_Text_Transparent
   --  would over a run filled with Under.
   procedure Draw (C : CuBit.UI.Canvas; X, Y : Natural; Text : String; Ink, Under : CuBit.UI.Color);

   --  Labels drawn from the cache, and rasterized into it, so far.
   function Hits return Natural;
   function Misses return Natural;
end Dash_Labels;
