------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The teapot gallery on a GPU canvas: tessellate once, then each step
--  acquires a target, renders the nine pots and presents. Shared by the
--  native application (main.adb) and the Linux-hosted harness
--  (tests/gpu-canvas), which differ only in how they get a window.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.UI;
with CuBit.UI.GPU_Canvas;

package Teapot_Gallery is

   --  Tessellate and build pipelines when the canvas has a device. OK is
   --  False only on a GPU failure; an Unavailable canvas is not a failure
   --  (it paints its placeholder).
   procedure Start (Canvas : in out CuBit.UI.GPU_Canvas.Canvas_Widget; OK : out Boolean);

   --  Render one frame for scene time Seconds if the canvas lends a target.
   procedure Step (Canvas : in out CuBit.UI.GPU_Canvas.Canvas_Widget; Seconds : Float;
                   Status : out CuBit.UI.GPU_Canvas.Acquire_Status);

   --  The shading names under each pot, in Into's logical coordinates.
   procedure Draw_Labels (Into : CuBit.UI.Canvas; Area : CuBit.UI.Rect);

   --  Frames presented per second, in tenths, over the last second.
   function Rate_Tenths return Natural;
   --  Monotonic milliseconds of the caller's clock, for Rate_Tenths.
   procedure Note_Presented (Now_Milliseconds : Unsigned_64);

   procedure Stop;

end Teapot_Gallery;
