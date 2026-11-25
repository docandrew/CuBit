------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's present scaler (APP-008 dynamic resolution): a frame drawn
--  at the internal resolution, scaled up bilinearly to the window, with
--  the scanlines laid at the window's own resolution.
--
--  @description
--  Pixels are 16#00RRGGBB#. A column table gives each window column its
--  two source columns and weight; each source row the window needs is
--  widened through it to the window's width (two cached rows), and each
--  window row is then the blend of the two source rows around it, so the
--  work is about one blend per window pixel plus one per widened source
--  pixel, in loops without branches (GCC vectorises them). Blends
--  are in 8-bit fixed point, red and blue together in one word and green
--  in another; blending a pixel with itself gives it back exactly, so a
--  frame scaled to its own size is copied unchanged. Sample positions
--  are pixel centres: the window's pixel X samples the source at
--  (X + 0.5) * Source_Width / Width - 0.5, clamped to the edge.
--
--  The scanlines (a darker odd line, Scanline_Weight of 256) are drawn
--  here when the frame was rendered without them: at a reduced internal
--  resolution they would otherwise be scaled into a blur or a beat.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Neon_Upscale with SPARK_Mode, Pure is

   Maximum_Extent : constant := 16_384;
   subtype Extent is Positive range 1 .. Maximum_Extent;

   type Pixel_Array is array (Natural range <>) of Unsigned_32;
   --  A window column's two source columns and its weight toward the second.
   type Index_Array is array (Natural range <>) of Natural;
   subtype Weight is Unsigned_32 range 0 .. 255;
   type Weight_Array is array (Natural range <>) of Weight;

   --  The odd lines' brightness, in 256ths (0.9, as Neon_Soft draws them).
   Scanline_Weight : constant := 230;

   --  Scale Source (Source_Width x Source_Height, packed rows) into Target
   --  (Width x Height, Pitch pixels a row). Rows (two widened rows), Left,
   --  Right and Weights (the column table) are scratch of the window's
   --  width. Pixels of Target past Width in a row are left alone.
   procedure Scale
     (Source : Pixel_Array; Source_Width, Source_Height : Extent;
      Target : in out Pixel_Array; Width, Height, Pitch : Extent;
      Rows : in out Pixel_Array; Left, Right : in out Index_Array; Weights : in out Weight_Array;
      Scanlines : Boolean)
     with Pre => Source'First = 0 and then Target'First = 0 and then Rows'First = 0
                 and then Left'First = 0 and then Right'First = 0 and then Weights'First = 0
                 and then Source'Last >= Source_Width * Source_Height - 1
                 and then Pitch >= Width
                 and then Target'Last >= Pitch * Height - 1
                 and then Rows'Last >= 2 * Width - 1
                 and then Left'Last >= Width - 1 and then Right'Last >= Width - 1
                 and then Weights'Last >= Width - 1;

end Neon_Upscale;
