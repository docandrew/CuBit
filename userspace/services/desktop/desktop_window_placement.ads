------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where and how large a new application window opens.
--
--  @description
--  Desktop policy, not per-app constants: a new window opens at least a
--  share of the logical work area (two thirds of its width, three quarters
--  of its height), or larger when the application asks for more, and never
--  larger than the work area. Windows cascade down a diagonal from the work
--  area's corner, one step per slot, starting over before a window would
--  leave the work area. The application's own limits (minimum, maximum,
--  fixed size) then apply as for any resize.
------------------------------------------------------------------------------
package Desktop_Window_Placement with SPARK_Mode, Pure is
   --  Logical pixels. Bounded so the arithmetic below cannot overflow.
   Maximum_Extent : constant := 2 ** 20;
   subtype Extent is Natural range 0 .. Maximum_Extent;
   subtype Slot_Number is Natural;

   --  The default share of the work area: Numerator / Denominator.
   Maximum_Share_Term : constant := 16;
   subtype Share_Term is Positive range 1 .. Maximum_Share_Term;
   type Share is record
      Numerator : Share_Term;
      Denominator : Share_Term;
   end record
     with Dynamic_Predicate => Share.Numerator <= Share.Denominator;
   Width_Share : constant Share := (Numerator => 2, Denominator => 3);
   Height_Share : constant Share := (Numerator => 3, Denominator => 4);

   --  The first window's corner, and the step between cascaded windows.
   Cascade_Left : constant := 80;
   Cascade_Top : constant := 64;
   Cascade_Step : constant := 18;

   type Placement is record
      X, Y, Width, Height : Extent;
   end record;

   function Part (Of_Extent : Extent; Portion : Share) return Extent is
     (Of_Extent * Portion.Numerator / Portion.Denominator)
     with Post => Part'Result <= Of_Extent;

   --  A window asked at Requested_Width x Requested_Height (0: no
   --  preference), the Slot-th, in a Work_Width x Work_Height work area
   --  (coordinates relative to the work area's corner). It ends inside the
   --  work area (cut to fit, like any window that would leave it) and is
   --  never smaller than Minimum_* where the work area has that room.
   function Place
     (Work_Width, Work_Height : Extent;
      Requested_Width, Requested_Height : Extent;
      Minimum_Width, Minimum_Height : Extent;
      Slot : Slot_Number) return Placement
     with Post =>
       Place'Result.X + Place'Result.Width <= Work_Width
       and then Place'Result.Y + Place'Result.Height <= Work_Height
       and then Place'Result.Width >= Natural'Min (Minimum_Width, Work_Width)
       and then Place'Result.Height >= Natural'Min (Minimum_Height, Work_Height)
       and then Place'Result.Width >= Natural'Min (Requested_Width, Work_Width - Place'Result.X)
       and then Place'Result.Height >= Natural'Min (Requested_Height, Work_Height - Place'Result.Y);
end Desktop_Window_Placement;
