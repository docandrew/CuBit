------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which exported icon edge to draw at an output density, and where.
--
--  @description
--  Icons are exported at a few edges (Desktop: 16, 24, 32, 48 and 64
--  pixels; the toolkit's stock set: 16 and 32) and drawn 1:1 in physical
--  pixels, never resampled, so they stay crisp at every density. An icon
--  designed Design logical pixels square covers Design * Numerator /
--  Denominator physical pixels; Nearest picks the exported edge whose
--  1:1 footprint, measured back in logical pixels, is closest to the
--  design (at 125% a 24-pixel icon is drawn from the 32-pixel export, a
--  16-pixel one from the 16-pixel export). Ties go to the earlier edge
--  of the list (the smaller, when the list is ascending), so an icon never
--  outgrows its box for nothing. Centered places the square in the
--  physical span of the icon's logical box.
------------------------------------------------------------------------------
with Client_Canvas_Geometry;

package Icon_Edge_Choice with SPARK_Mode, Pure is
   subtype Scale_Part is Client_Canvas_Geometry.Component;

   --  The largest edge an icon set may export.
   Largest_Edge : constant := 4_096;
   subtype Edge is Positive range 1 .. Largest_Edge;
   type Edge_List is array (Positive range <>) of Edge;

   --  The footprint error of drawing Candidate 1:1 for a Design-pixel icon,
   --  in 1/Numerator logical pixels: |Candidate * Denominator / Numerator
   --  - Design| * Numerator. Numerator is the same for every candidate,
   --  so comparing these compares the logical errors exactly.
   Largest_Distance : constant := Largest_Edge * Scale_Part'Last;
   subtype Distance_Value is Natural range 0 .. Largest_Distance;
   function Distance (Candidate, Design : Edge; Numerator, Denominator : Scale_Part)
     return Distance_Value is
     (abs (Candidate * Denominator - Design * Numerator));

   --  The index in Available of the edge to draw.
   function Nearest
     (Available : Edge_List; Design : Edge; Numerator, Denominator : Scale_Part) return Positive
     with Pre  => Available'Length > 0,
          Post => Nearest'Result in Available'Range and then
                  (for all I in Available'Range =>
                     Distance (Available (Nearest'Result), Design, Numerator, Denominator) <=
                     Distance (Available (I), Design, Numerator, Denominator)) and then
                  (for all I in Available'First .. Nearest'Result - 1 =>
                     Distance (Available (I), Design, Numerator, Denominator) >
                     Distance (Available (Nearest'Result), Design, Numerator, Denominator));

   --  Physical pixel coordinates, wide enough for any output and origin.
   Pixel_Bound : constant := 2 ** 24;
   subtype Pixel is Integer range -Pixel_Bound .. Pixel_Bound;
   subtype Centered_Pixel is Integer range -2 * Pixel_Bound .. 2 * Pixel_Bound;

   --  The first pixel of a Size-pixel square centred in [First, Past). A
   --  square larger than the span starts before First.
   function Centered (First, Past : Pixel; Size : Edge) return Centered_Pixel is
     (First + (Past - First - Size) / 2)
     with Pre => First <= Past;
end Icon_Edge_Choice;
