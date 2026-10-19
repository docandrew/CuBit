------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's arithmetic: where task buttons stand, the volume slider's
--  level under the pointer, and the panel's date words.
--
--  @description
--  Task buttons stand in [Left, Right) in their places' order (first
--  opened first), each as wide as its content (Preferred: padding, icon,
--  label as the taskbar draws it, count badge) while they all fit. When
--  they do not, the inactive buttons shrink in proportion to how far each
--  is above Minimum (their labels are cut with an ellipsis), and the active
--  button only once every other is at Minimum, so it stays readable. Past
--  everything at Minimum, the buttons that would not fit are not shown
--  (W = 0). Proved: every shown button lies inside [Left, Right), is
--  between Minimum and its Preferred width, and comes after the ones
--  before it without overlapping them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Desktop_Surface_Limits;

package Desktop_Deskbar_Layout with SPARK_Mode is
   Max_Coordinate : constant := 32_767;
   subtype Coordinate is Natural range 0 .. Max_Coordinate;

   --  One button per app with a window: as many as Desktop's surface
   --  table can hold.
   Max_Buttons : constant := Desktop_Surface_Limits.Maximum_Surfaces;
   subtype Button_Count is Natural range 0 .. Max_Buttons;
   subtype Button_Position is Positive range 1 .. Max_Buttons;

   Max_Button_Width : constant := 1_024;
   Max_Gap          : constant := 64;
   subtype Button_Width_Range is Positive range 1 .. Max_Button_Width;
   subtype Gap_Range is Natural range 0 .. Max_Gap;

   --  Per button, in place order from 1.
   type Width_List is array (Button_Position range <>) of Button_Width_Range;

   --  Each button's width in [Left, Right), Gap apart; Active is the active
   --  button's place (0: none).
   function Widths
     (Left, Right : Coordinate; Preferred : Width_List;
      Minimum : Button_Width_Range; Gap : Gap_Range; Active : Button_Count) return Width_List
     with Pre  => Left <= Right and then Preferred'First = 1 and then Active <= Preferred'Last
                  and then (for all P in Preferred'Range => Minimum <= Preferred (P)),
          Post => Widths'Result'First = 1 and then Widths'Result'Last = Preferred'Last
                  and then (for all P in Preferred'Range =>
                     Widths'Result (P) >= Minimum and then Widths'Result (P) <= Preferred (P));

   type Span is record
      X : Coordinate := 0;
      W : Natural := 0;
   end record;
   type Span_List is array (Button_Position range <>) of Span;

   --  Where each button stands; W = 0 for one that does not fit.
   function Spans
     (Left, Right : Coordinate; Preferred : Width_List;
      Minimum : Button_Width_Range; Gap : Gap_Range; Active : Button_Count) return Span_List
     with Pre  => Left <= Right and then Preferred'First = 1 and then Active <= Preferred'Last
                  and then (for all P in Preferred'Range => Minimum <= Preferred (P)),
          Post => Spans'Result'First = 1 and then Spans'Result'Last = Preferred'Last
                  and then
                  (for all P in Preferred'Range =>
                     (if Spans'Result (P).W /= 0 then
                        Spans'Result (P).X >= Left
                        and then Spans'Result (P).X + Spans'Result (P).W <= Right
                        and then Spans'Result (P).W >= Minimum
                        and then Spans'Result (P).W <= Preferred (P)))
                  and then
                  (for all P in Preferred'Range =>
                     (for all Q in P + 1 .. Preferred'Last =>
                        (if Spans'Result (P).W /= 0 and then Spans'Result (Q).W /= 0 then
                           Spans'Result (P).X + Spans'Result (P).W <= Spans'Result (Q).X)));

   --  The volume slider: the level at pointer X on a track at Track_X,
   --  Track_W wide (clamped to the ends), and the thumb's offset for Level.
   subtype Percent is Natural range 0 .. 100;
   subtype Track_Width is Positive range 1 .. Max_Coordinate;
   function Level_At (Track_X : Coordinate; Track_W : Track_Width; X : Coordinate)
     return Percent;
   function Offset_Of (Track_W : Track_Width; Level : Percent) return Natural
     with Post => Offset_Of'Result <= Track_W;

   --  The panel's date line, e.g. "Sunday 18 October".
   subtype Year_Number is Natural range 1970 .. 2399;
   subtype Month_Number is Positive range 1 .. 12;
   subtype Day_Number is Positive range 1 .. 31;
   type Weekday is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   function Day_Of_Week (Year : Year_Number; Month : Month_Number; Day : Day_Number)
     return Weekday;
   function Weekday_Name (Day : Weekday) return String;
   function Month_Name (Month : Month_Number) return String;
end Desktop_Deskbar_Layout;
