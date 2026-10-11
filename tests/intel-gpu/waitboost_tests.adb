pragma Ada_2022;
with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_SLPC; use type Intel_GPU_SLPC.Request;
with Intel_GPU_Waitboost; use Intel_GPU_Waitboost;
procedure Waitboost_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
   --  The NUC: RPe 300, RP0 1200.
   S : State := Start (300, 1200, 1_000);
   Boosts, Hints, Sends_Count, Boosted_Time, Ramps, Unreached, Avg, Max : Unsigned_64;
   Ms : constant := 1_000;
   Frame_Us : constant := 4_167;   --  240 Hz
   Now : Microseconds := 1_000;
   Sends : Natural := 0;
   --  One driver loop turn: step, then at most one send of the newest value.
   procedure Turn (Demand : Boolean; Hint : Boolean := False) is
   begin
      Step (S, Demand, Hint, Now);
      if Need_Send (S) then
         Note_Sent (S, Desired (S), True, Now);
         Sends := Sends + 1;
      end if;
   end Turn;
begin
   Check (Intel_GPU_SLPC.Fast_Min_Request (1200) = [16#2000_3003#, 16#602#, 6, 1200],
          "fast min request: FAST_REQUEST header, PARAMETER_SET, min param");
   Check (S.Current = Idle and not Need_Send (S) and Desired (S) = 300, "starts at RPe, nothing to send");
   --  An input hint boosts before any job.
   Turn (False, Hint => True);
   Check (S.Current = Boosted and Desired (S) = 1200 and Sends = 1 and S.Ramping, "input hint boosts at once");
   Now := Now + 300; Note_Actual (S, 600, Now);
   Check (S.Ramping, "ramp not done at 600 MHz");
   Now := Now + 400; Note_Actual (S, 1200, Now);
   Check (not S.Ramping and S.Ramps = 1 and S.Ramp_Max_Us = 700, "ramp 700 us recorded");
   --  Typing: a key every 400 ms, frames only after keys: one boost, no sends.
   for Key in 1 .. 10 loop
      Now := Now + 400 * Ms; Turn (False, Hint => True);
      for F in 1 .. 2 loop Now := Now + Frame_Us; Turn (True); end loop;
   end loop;
   Check (Sends = 1 and S.Current = Boosted, "typing keeps one boost");
   --  240 Hz drag: frames every 4.17 ms with gaps: no ramp per frame.
   for F in 1 .. 240 loop
      Now := Now + Frame_Us; Turn (F mod 2 = 0, Hint => F mod 3 = 0);
   end loop;
   Check (Sends = 1, "a 240 Hz drag never decays between frames");
   --  Work alone (no input): held 50 ms after the last job.
   Now := Now + Input_Hold_Us; Turn (True);
   Now := Now + 49 * Ms; Turn (False);
   Check (S.Current = Boosted, "held 49 ms after the last job");
   Now := Now + Ms; Turn (False);
   Check (S.Current = Idle and Desired (S) = 300 and Sends = 2, "back to RPe after both holds");
   --  The input hold alone keeps it up for a second.
   Turn (False, Hint => True);
   Now := Now + Input_Hold_Us - Ms; Turn (False);
   Check (S.Current = Boosted, "input hold 1 s");
   Now := Now + Ms; Turn (False);
   Check (S.Current = Idle, "input hold expired");
   --  A ramp cut short by the decay is counted as unreached.
   Check (S.Ramping, "ramp still open");
   Note_Actual (S, 300, Now);
   Check (not S.Ramping and S.Unreached = 1, "unreached ramp counted");
   --  Coalescing: a boost and decay between sends cost nothing.
   Step (S, True, False, Now); Now := Now + Input_Hold_Us; Step (S, False, False, Now);
   Check (S.Current = Idle and not Need_Send (S), "a boost and decay between sends: no send");
   Step (S, True, False, Now);
   Note_Sent (S, Desired (S), False, Now);
   Check (Need_Send (S) and S.Sent = 300, "backpressure: retried next turn");
   Note_Sent (S, Desired (S), True, Now);
   Check (not Need_Send (S) and S.Sent = 1200, "sent once accepted");
   --  A clock that goes back never ends a boost.
   Step (S, False, False, Now - 100 * Ms);
   Check (S.Current = Boosted, "clock regression keeps the boost");
   Take_Stats (S, Now, Boosts, Hints, Sends_Count, Boosted_Time, Ramps, Unreached, Avg, Max);
   Check (Boosts >= 3 and Hints >= 10 and Boosted_Time > 0 and Ramps = 1 and Avg = 700 and Max = 700,
          "stats: boosts, hints, boosted time, ramp avg/max");
   Take_Stats (S, Now, Boosts, Hints, Sends_Count, Boosted_Time, Ramps, Unreached, Avg, Max);
   Check (Boosts = 0 and Hints = 0 and Ramps = 0 and Avg = 0, "stats reset");
   Ada.Text_IO.Put_Line ("SLPC waitboost PASS:" & Natural'Image (Checks) & " checks");
end Waitboost_Tests;
