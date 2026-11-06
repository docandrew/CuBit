--  procmgr supervision (BUG-001): an entry declared (restart on-failure) is
--  started again after a failure with a growing delay; a clean exit, or an
--  entry without the declaration, stays ended. Hosted; procmgr's launches
--  are covered by the headless desktop-restart test.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Plan_Restarts; use Plan_Restarts;
procedure Restart_Tests is
   R : Run;
   Scheduled : Boolean;
   Now : Unsigned_64 := 5_000;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then raise Program_Error with What; end if;
      Checks := Checks + 1;
   end Check;
   Doubling : constant array (1 .. 6) of Unsigned_64 :=
     [2_000, 4_000, 8_000, 16_000, Longest_Delay_Ms, Longest_Delay_Ms];
begin
   --  The desktop exits with code 1 seconds after starting: restart after 1 s.
   Started (R, Now);
   Now := Now + 3_000;
   Ended (R, Now, Failed => True, Restartable => True, Scheduled => Scheduled);
   Check (Scheduled and R.State = Waiting and R.Delay_Ms = First_Delay_Ms, "first restart delay");
   Check (not Due (R, Now + First_Delay_Ms - 1) and Due (R, Now + First_Delay_Ms), "due after the delay");
   --  It keeps failing quickly: 2, 4, 8, 16 s, then the longest delay.
   for Expected of Doubling loop
      Now := R.Due_At;
      Started (R, Now);
      Now := Now + 500;
      Ended (R, Now, True, True, Scheduled);
      Check (Scheduled and R.Delay_Ms = Expected, "delay doubles to the longest" & Expected'Image);
   end loop;
   --  A run that lasted starts the delays over.
   Now := R.Due_At;
   Started (R, Now);
   Now := Now + Settled_Run_Ms;
   Ended (R, Now, True, True, Scheduled);
   Check (Scheduled and R.Delay_Ms = First_Delay_Ms, "a settled run starts over");
   --  A refused relaunch waits longer.
   Refused (R, Now);
   Check (R.State = Waiting and R.Delay_Ms = 2 * First_Delay_Ms and R.Due_At = Now + 2 * First_Delay_Ms,
          "refused relaunch backs off");
   --  Clean exit, or no (restart on-failure): stays ended.
   Started (R, Now);
   Ended (R, Now + 10, Failed => False, Restartable => True, Scheduled => Scheduled);
   Check (not Scheduled and R.State = Idle, "a clean exit is not restarted");
   Started (R, Now);
   Ended (R, Now + 10, Failed => True, Restartable => False, Scheduled => Scheduled);
   Check (not Scheduled and R.State = Idle and not Due (R, Unsigned_64'Last), "undeclared: never restarted");
   --  The clock at its end never wraps the due time.
   Started (R, Unsigned_64'Last - 10);
   Ended (R, Unsigned_64'Last - 5, True, True, Scheduled);
   Check (Scheduled and R.Due_At = Unsigned_64'Last, "due time saturates");
   Ada.Text_IO.Put_Line ("Plan restarts PASS:" & Checks'Image & " checks (hosted)");
end Restart_Tests;
