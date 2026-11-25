------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's software present path with dynamic resolution (APP-008):
--  the scene built and rendered at the internal resolution
--  Neon_Resolution picks, scaled up to the window by Neon_Upscale.
--
--  @description
--  At Native it renders straight into the window, as before. Below it,
--  into its own frame (grown to the largest size asked, reused after),
--  without scanlines, which the scaler then lays at the window's
--  resolution. Each Paint times the render and the scale on Now_Us and
--  feeds them to the controller, which picks the next frame's level.
--
--  Only what is drawn depends on the level: the scene is built from the
--  game's state, which Paint does not change.
------------------------------------------------------------------------------
with System;
with Interfaces; use Interfaces;
with Neon_Game;
with Neon_Resolution;
with Neon_Scene;
with Neon_Soft;
with Neon_Upscale;

generic
   --  Microseconds on a monotonic clock.
   with function Now_Us return Unsigned_64;
   --  Held while the scene is built from the game's state, and released
   --  before rasterizing: the soundtrack's thread steps the same game
   --  (Neon_Audio), so a slow frame must not hold it up.
   with procedure Lock_Game is null;
   with procedure Unlock_Game is null;
package Neon_Present is

   type Presenter is limited private;

   --  What the last Paint did.
   type Report is record
      Output_Width, Output_Height : Positive := 1;
      Internal_Width, Internal_Height : Positive := 1;
      Scale : Neon_Resolution.Percent := 100;
      Render_Us, Scale_Us : Neon_Resolution.Microseconds := 0;
   end record;

   --  Draw G's current moment into Target (Width x Height, Pitch_Pixels a
   --  row, 16#00RRGGBB#), building F for the internal view.
   procedure Paint
     (P : in out Presenter; R : in out Neon_Soft.Renderer; F : in out Neon_Scene.Frame;
      G : Neon_Game.Game; Target : System.Address; Width, Height, Pitch_Pixels : Positive)
     with Pre => Width <= Neon_Resolution.Maximum_Extent and then Height <= Neon_Resolution.Maximum_Extent
                 and then Pitch_Pixels >= Width and then Pitch_Pixels <= Neon_Upscale.Maximum_Extent;

   function Last (P : Presenter) return Report;
   function Control (P : Presenter) return Neon_Resolution.Controller;

private
   type Pixel_Access is access Neon_Upscale.Pixel_Array;
   type Index_Access is access Neon_Upscale.Index_Array;
   type Weight_Access is access Neon_Upscale.Weight_Array;

   type Presenter is limited record
      Controller : Neon_Resolution.Controller := Neon_Resolution.Initial;
      Frame, Rows : Pixel_Access;
      Left, Right : Index_Access;
      Weights : Weight_Access;
      Latest : Report;
   end record;

   function Last (P : Presenter) return Report is (P.Latest);
   function Control (P : Presenter) return Neon_Resolution.Controller is (P.Controller);
end Neon_Present;
