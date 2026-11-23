------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Battery item's glyph (PWR-004 slice 1, UI-027) as square parts within
--  the 24-pixel icon box, like Desktop_Network_Glyphs: an outline and its
--  terminal, a fill as wide as the charge (Desktop_Power_Text.Fill_Width
--  of Fill_Width columns), a bolt cut into the fill while charging, and an
--  exclamation mark while low. Static: nothing blinks.
------------------------------------------------------------------------------
pragma Ada_2022;

package Desktop_Battery_Glyph with SPARK_Mode, Pure is
   type Part is record
      X, Y, W, H : Natural := 0;   --  from the box's top left
   end record;

   --  The outline (stroked) and the terminal on its right.
   Body_X : constant := 1;
   Body_Y : constant := 7;
   Body_W : constant := 19;
   Body_H : constant := 10;
   Terminal_X : constant := Body_X + Body_W;
   Terminal_W : constant := 2;
   Terminal_H : constant := 4;
   Terminal_Y : constant := Body_Y + (Body_H - Terminal_H) / 2;

   --  The fill, one pixel inside the outline's stroke.
   Inset : constant := 2;
   Fill_X : constant := Body_X + Inset;
   Fill_Y : constant := Body_Y + Inset;
   Fill_Width : constant := Body_W - 2 * Inset;
   Fill_H : constant := Body_H - 2 * Inset;

   --  The bolt, cut into the fill (drawn in the button's own colour).
   Bolt_Parts : constant := 3;
   subtype Bolt_Index is Positive range 1 .. Bolt_Parts;
   Bolt_Centre : constant := Fill_X + Fill_Width / 2;
   function Bolt (P : Bolt_Index) return Part is
     (case P is
         when 1 => (Bolt_Centre, Fill_Y, 2, 2),
         when 2 => (Bolt_Centre - 2, Fill_Y + 2, 5, 2),
         when 3 => (Bolt_Centre - 1, Fill_Y + 4, 2, 2));

   --  The low mark: an exclamation bar and dot at the body's centre.
   Mark_W : constant := 2;
   Mark_X : constant := Fill_X + (Fill_Width - Mark_W) / 2;
   Mark_Y : constant := Fill_Y;
   Mark_H : constant := Fill_H - 3;
   Dot_Y  : constant := Fill_Y + Fill_H - 2;
   Dot_H  : constant := 2;
end Desktop_Battery_Glyph;
