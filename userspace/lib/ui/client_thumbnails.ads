------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A small picture of a large one (UI-061: the Deskbar's hover card shows a
--  live thumbnail of a window): the size that fits, and a box-filtered
--  downscale of opaque BGRA pixels. Pure and proved (tests/ui061).
--
--  @description
--  Fit keeps the picture's shape inside a bound and never enlarges it.
--  Scale gives each target pixel the mean of Samples_Per_Axis squared
--  source pixels on a rotated grid over the source box it covers (no two
--  share a column or a row, so fine patterns average instead of aliasing).
--  Reading a fixed number of samples, not the whole box, bounds the cost by
--  the target's size whatever the source's: a 336 x 189 thumbnail reads
--  about a million source pixels from a window of any size. Text stays
--  legible where nearest-pixel sampling breaks it into noise.
--
--  Pixels are 32-bit words, B in the low byte (BGRA8888 as the desktop
--  composes them); the result is opaque. Source and target are flat arrays
--  with a row pitch in pixels; the caller overlays them on its mappings.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Client_Thumbnails with SPARK_Mode, Pure is
   --  The largest window a source can be, per axis (the protocol's surface
   --  limit is smaller), and the largest thumbnail.
   Maximum_Source_Extent : constant := 16_384;
   Maximum_Target_Extent : constant := 1_024;
   subtype Source_Extent is Positive range 1 .. Maximum_Source_Extent;
   subtype Target_Extent is Positive range 1 .. Maximum_Target_Extent;

   type Size is record
      Width, Height : Source_Extent := 1;
   end record;

   --  Source scaled by one factor so it fits Bound, never enlarged: the
   --  axis that binds is exactly Bound's (or Source's), the other rounds
   --  down, and neither is below 1.
   function Fit (Source, Bound : Size) return Size
     with Post => Fit'Result.Width <= Bound.Width and then Fit'Result.Height <= Bound.Height
                  and then Fit'Result.Width <= Source.Width and then Fit'Result.Height <= Source.Height;

   Samples_Per_Axis : constant := 4;

   subtype Pixel is Unsigned_32;
   subtype Pixel_Index is Natural range 0 .. Maximum_Source_Extent * Maximum_Source_Extent - 1;
   type Pixel_Array is array (Pixel_Index range <>) of Pixel;

   --  Source_Size pixels (row pitch Source_Pitch) into the Target_Width x
   --  Target_Height box at (Target_X, Target_Y) of Target (row pitch
   --  Target_Pitch): the box's rows First_Row .. First_Row + Row_Count - 1,
   --  so a caller can spread one picture over several frames. Target's
   --  other pixels keep their values.
   procedure Scale
     (Source : Pixel_Array; Source_Size : Size; Source_Pitch : Source_Extent;
      Target : in out Pixel_Array; Target_Pitch : Source_Extent;
      Target_X, Target_Y : Natural; Target_Width, Target_Height : Target_Extent;
      First_Row : Natural; Row_Count : Natural)
     with Pre => Source'First = 0 and then Target'First = 0
                 and then First_Row <= Target_Height and then Row_Count <= Target_Height - First_Row
                 and then Source_Pitch >= Source_Size.Width
                 and then Source'Length >= Source_Size.Height * Source_Pitch
                 and then Target_X <= Maximum_Source_Extent - Target_Width
                 and then Target_Y <= Maximum_Source_Extent - Target_Height
                 and then Target_X + Target_Width <= Target_Pitch
                 and then Target'Length >= (Target_Y + Target_Height) * Target_Pitch;
end Client_Thumbnails;
