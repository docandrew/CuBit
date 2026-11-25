------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's software renderer: the same scene the Vulkan path draws,
--  rendered by the CPU into a 32-bit XRGB frame, for launches without a
--  GPU render session (QEMU, software starts) and for the deterministic
--  replay hashes.
--
--  @description
--  One pass shades the sky, the banded sun, the mountains and the bent
--  floor a row at a time (each row meets the floor at one depth). The
--  floor's lights and ripples come from a small world-space light map
--  splatted once a frame. Lines and particles are drawn additively,
--  anti-aliased, into the frame and into a quarter-size glow buffer, which
--  is blurred and added back over the frame (the bloom), with scanlines
--  and a vignette in the same final pass.
--
--  It allocates its buffers from the frame's size (they grow, never a fixed
--  maximum, and are reused for smaller frames) and keeps nothing between
--  frames but those buffers.
------------------------------------------------------------------------------
with System;
with Interfaces; use Interfaces;
with Neon_Scene;

package Neon_Soft is

   type Renderer is limited private;

   --  Render F into Target: Width x Height pixels, Pitch_Pixels apart a row,
   --  each 16#00RRGGBB#. F's size is the view the scene was built for.
   procedure Render (R : in out Renderer; F : Neon_Scene.Frame; Target : System.Address;
                     Width, Height, Pitch_Pixels : Positive);

   --  How often R's buffers grew: a frame no larger than one it already
   --  rendered reuses them.
   function Buffer_Growths (R : Renderer) return Natural;

   --  FNV-1a 64 of the last rendered frame's pixels (the replay check).
   function Frame_Hash (Target : System.Address; Width, Height, Pitch_Pixels : Positive)
     return Unsigned_64;

private
   type Float_Plane is array (Natural range <>) of Float;
   type Plane_Access is access Float_Plane;
   type Light_Plane is array (Natural range <>) of Float;
   type Light_Access is access Light_Plane;

   type Renderer is limited record
      Glow_Width, Glow_Height : Natural := 0;
      Glow_R, Glow_G, Glow_B, Scratch : Plane_Access;
      Growths : Natural := 0;
      Light_R, Light_G, Light_B : Light_Access;
   end record;
end Neon_Soft;
