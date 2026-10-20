------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  "Keep this display mode?" (DISP-016). After Settings > Monitor switches
--  an output, the new mode stays only if the user keeps it before the
--  deadline; no answer, like Revert, goes back to the previous mode (as
--  Windows and GNOME do: a mode the monitor cannot show leaves the user
--  nothing to click). Times are the desktop's GETTIME milliseconds.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Output_Modes;

package Desktop_Mode_Trial with SPARK_Mode, Pure is
   package OM renames CuBit.Output_Modes;
   use type OM.Mode_Request;
   --  Milliseconds since boot (GETTIME), saturating: a clock this far out
   --  (146 million years) reads as Time'Last.
   type Time is range 0 .. 2 ** 62;
   function Time_Of (Clock : Unsigned_64) return Time is
     (if Clock >= Unsigned_64 (Time'Last) then Time'Last else Time (Clock));
   --  How long the question waits: an explicit, named limit (Windows asks
   --  for 15 s, GNOME for 20 s).
   Confirmation_Window : constant Time := 15_000;
   Second : constant Time := 1_000;
   subtype Countdown is Natural range 0 .. Natural (Confirmation_Window / Second);

   type Phase is (Idle, Asking);
   type State (Current : Phase := Idle) is record
      case Current is
         when Asking =>
            --  The mode to go back to and the one on trial; Auto: keeping
            --  it stores Auto (the best admitted mode), not this mode.
            Previous, Trial : OM.Mode_Request;
            Auto : Boolean;
            Deadline : Time;
         when Idle => null;
      end case;
   end record;

   function Start (Previous, Trial : OM.Mode_Request; Auto : Boolean; Now : Time) return State
     with Pre => Now <= Time'Last - Confirmation_Window,
          Post => Start'Result.Current = Asking and then Start'Result.Deadline = Now + Confirmation_Window
                  and then Start'Result.Previous = Previous and then Start'Result.Trial = Trial;
   --  No answer by the deadline: revert.
   function Expired (Item : State; Now : Time) return Boolean is
     (Item.Current = Asking and then Now >= Item.Deadline);
   --  Whole seconds left, rounded up: what the question counts down.
   function Seconds_Left (Item : State; Now : Time) return Countdown
     with Pre => Item.Current = Asking,
          Post => (Seconds_Left'Result = 0) = (Now >= Item.Deadline);
   --  When the desktop must look again while the question is open: the
   --  next change of the countdown, at the latest the deadline.
   function Next_Due (Item : State; Now : Time) return Time
     with Pre => Item.Current = Asking,
          Post => (if Now >= Item.Deadline then Next_Due'Result = Now
                   else Next_Due'Result > Now and then Next_Due'Result <= Item.Deadline);
end Desktop_Mode_Trial;
