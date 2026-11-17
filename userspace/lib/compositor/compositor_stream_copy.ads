------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Write a composed area out to a scanout buffer with non-temporal stores
--  (LAT-003).
--
--  @description
--  The compositor composes into a private buffer that stays in the cache
--  (the same lines every frame) and then streams the frame's boxes to the
--  scanout buffer the display engine reads: the stores bypass the cache
--  (no read for ownership of lines the driver has just flushed, no
--  eviction of the compositor's working set). Fence once after the last
--  box, before the frame is presented: it orders the streamed stores
--  before the Present that hands the buffer to the display.
--
--  Not SPARK: x86-64 SSE2 stores through raw addresses. The caller
--  guarantees both images hold the box (the boxes are clipped to the
--  output by the damage tracker, both images are the output's size).
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
package Compositor_Stream_Copy with SPARK_Mode => Off is
   --  Box (X, Y, Width, Height in pixels of 4 bytes) of Source into
   --  Target, rows at their pitches (bytes).
   procedure Copy_Box
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      X, Y, Width, Height : Natural);
   --  Order every streamed store before what follows (sfence).
   procedure Fence;
end Compositor_Stream_Copy;
