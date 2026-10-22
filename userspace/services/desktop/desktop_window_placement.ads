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
--  larger than the work area. An application that declares a preferred
--  size (Desktop_Protocol.Preferred_Size: a compact tool) opens at that
--  size instead. Windows cascade down a diagonal from the work
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

   --  How the request is taken: At_Least_Share opens the window at least
   --  the default share (the request is a preference); As_Requested opens
   --  it at the request (an application's declared preferred size), the
   --  share only standing in for a missing (zero) request.
   type Sizing_Policy is (At_Least_Share, As_Requested);

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
   --  As_Requested with a nonzero request opens it no larger than asked
   --  (or the minimum, when that is larger).
   function Place
     (Work_Width, Work_Height : Extent;
      Requested_Width, Requested_Height : Extent;
      Minimum_Width, Minimum_Height : Extent;
      Slot : Slot_Number;
      Policy : Sizing_Policy) return Placement
     with Post =>
       Place'Result.X + Place'Result.Width <= Work_Width
       and then Place'Result.Y + Place'Result.Height <= Work_Height
       and then Place'Result.Width >= Natural'Min (Minimum_Width, Work_Width)
       and then Place'Result.Height >= Natural'Min (Minimum_Height, Work_Height)
       and then Place'Result.Width >= Natural'Min (Requested_Width, Work_Width - Place'Result.X)
       and then Place'Result.Height >= Natural'Min (Requested_Height, Work_Height - Place'Result.Y)
       and then (if Policy = As_Requested and then Requested_Width > 0 then
                   Place'Result.Width <= Natural'Max (Requested_Width, Minimum_Width))
       and then (if Policy = As_Requested and then Requested_Height > 0 then
                   Place'Result.Height <= Natural'Max (Requested_Height, Minimum_Height));
end Desktop_Window_Placement;
