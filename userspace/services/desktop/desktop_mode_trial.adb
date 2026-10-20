------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Desktop_Mode_Trial with SPARK_Mode is
   function Start (Previous, Trial : OM.Mode_Request; Auto : Boolean; Now : Time) return State is
     ((Current => Asking, Previous => Previous, Trial => Trial, Auto => Auto,
       Deadline => Now + Confirmation_Window));

   function Seconds_Left (Item : State; Now : Time) return Countdown is
      Remaining : Time;
   begin
      if Now >= Item.Deadline then
         return 0;
      end if;
      --  A clock read before the start cannot show more than the window.
      Remaining := Time'Min (Item.Deadline - Now, Confirmation_Window);
      return Countdown ((Remaining + Second - 1) / Second);
   end Seconds_Left;

   function Next_Due (Item : State; Now : Time) return Time is
      Remaining, Step : Time;
   begin
      if Now >= Item.Deadline then
         return Now;
      end if;
      --  The countdown changes when the time left crosses a whole second.
      Remaining := Item.Deadline - Now;
      Step := Remaining mod Second;
      if Step = 0 then
         Step := Second;
      end if;
      return Now + Time'Min (Step, Remaining);
   end Next_Due;
end Desktop_Mode_Trial;
