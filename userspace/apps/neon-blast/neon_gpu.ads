------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's Vulkan path on the toolkit's GPU canvas: each step
--  acquires a target, builds the scene at the target's size and records one
--  pass (the sky, sun and lit floor in a full-screen shader, then the
--  additive glowing lines and particles), and presents.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.UI.GPU_Canvas;
with Neon_Game;
with Neon_Scene;

package Neon_GPU is
   --  Build the pipelines when the canvas has a device. OK is False only on
   --  a GPU failure; an Unavailable canvas is not a failure.
   procedure Start (Canvas : in out CuBit.UI.GPU_Canvas.Canvas_Widget; OK : out Boolean);
   function Started return Boolean;
   --  Render the game's current state if the canvas lends a target.
   procedure Draw (Canvas : in out CuBit.UI.GPU_Canvas.Canvas_Widget; F : in out Neon_Scene.Frame;
                   G : Neon_Game.Game; Status : out CuBit.UI.GPU_Canvas.Acquire_Status);
   --  CPU microseconds the last Draw spent building and recording.
   function Last_Record_Us return Unsigned_64;
   --  GPU execution time of the newest finished frame, microseconds (0:
   --  not measured: no timestamp queries on this device).
   function Last_GPU_Us return Unsigned_64;
   procedure Stop;
end Neon_GPU;
