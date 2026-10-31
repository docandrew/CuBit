pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Delays;
with ACPI_Delay_Waits;
procedure Policy_Tests is
   use type AML_Delays.Sleep_Milliseconds;
   Time, Step : Unsigned_64 := 0;
   Reads, Sleeps : Natural := 0;
   Ready, Refuse, Regress, Early : Boolean := False;
   procedure Read_Raw (Value : out Unsigned_64; Available : out Boolean) is
   begin
      Reads := Reads + 1;
      Available := Ready;
      if Regress and Reads = 2 then Value := Time - 1;
      else Value := Time; Time := Time + Step;
      end if;
   end Read_Raw;
   procedure Sleep_Raw (Until_Time : Unsigned_64; Accepted : out Boolean) is
   begin
      Sleeps := Sleeps + 1;
      Accepted := not Refuse;
      if Accepted then
         if Early then Time := Unsigned_64'Min (Until_Time, Time + 100);
         else Time := Until_Time;
         end if;
      end if;
   end Sleep_Raw;
   package W is new ACPI_Delay_Waits (Read_Raw, Sleep_Raw, 1_000);
   use type W.Status;
   R : W.Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   procedure Reset (Now : Unsigned_64 := 0; Advance : Unsigned_64 := 0) is
   begin
      Time := Now; Step := Advance; Reads := 0; Sleeps := 0;
      Ready := True; Refuse := False; Regress := False; Early := False;
   end Reset;
   procedure Verify (Expected : W.Status; Budget : W.Observation_Budget) is
   begin
      Check (R.Code = Expected, "typed wait outcome " & R.Code'Image);
      Check (R.Samples = Reads and then R.Sleeps = Sleeps, "every callback accounted");
      Check (R.Samples <= Budget, "stopped and unavailable clocks consume finite work");
      if Expected = W.Completed then
         Check (R.Last >= R.Target and then R.Last - R.Began >= R.Target - R.Began,
           "completion establishes full requested elapsed interval");
      end if;
   end Verify;
begin
   for Duration in AML_Delays.Stall_Microseconds loop
      Reset (17, 1);
      W.Run ((AML_Delays.Stall_Delay, Duration), 10_000, 300, R);
      Verify (W.Completed, 300);
      Check (R.Target = 17 + Unsigned_64 (Duration), "stall microseconds never scaled as milliseconds");
      Check (Sleeps = 0, "Stall never calls a relinquishing operation");
   end loop;
   for Duration in AML_Delays.Sleep_Milliseconds loop
      if Duration in 0 | 1 | 2 | AML_Delays.Sleep_Milliseconds'Last then
         Reset (31);
         W.Run ((AML_Delays.Sleep_Delay, Duration), 3_000_000, 4_096, R);
         Verify (W.Completed, 4_096);
         Check (R.Target = 31 + Unsigned_64 (Duration) * 1_000, "Sleep uses milliseconds");
         Check (Sleeps = (if Duration = 0 then 1 else Natural (Duration)),
           "bounded slices and zero Sleep relinquish");
      end if;
   end loop;
   Reset; Early := True;
   W.Run ((AML_Delays.Sleep_Delay, 2), 10_000, 30, R);
   Verify (W.Completed, 30); Check (Sleeps = 20, "early wakes resample rather than complete early");
   Reset; Refuse := True;
   W.Run ((AML_Delays.Sleep_Delay, 1), 10_000, 30, R);
   Verify (W.Sleep_Refused, 30); Check (Reads = 1 and Sleeps = 1, "no replay after rejected sleep");
   Reset (100); Regress := True;
   W.Run ((AML_Delays.Stall_Delay, 1), 10_000, 30, R);
   Verify (W.Clock_Regressed, 30);
   Reset; Ready := False;
   W.Run ((AML_Delays.Sleep_Delay, 1), 10_000, 30, R);
   Verify (W.Clock_Unavailable, 30); Check (Sleeps = 0, "no scheduling without a clock");
   Reset;
   W.Run ((AML_Delays.Stall_Delay, 1), 10_000, W.Observation_Budget'Last, R);
   Verify (W.Observation_Limit, W.Observation_Budget'Last);
   Check (Reads = W.Maximum_Observations and Sleeps = 0, "stopped busy clock is bounded");
   Reset;
   W.Run ((AML_Delays.Sleep_Delay, 0), 10_000, 1, R);
   Verify (W.Observation_Limit, 1); Check (Sleeps = 0, "no callback without a remaining verification sample");
   Reset (Unsigned_64'Last - 5);
   W.Run ((AML_Delays.Stall_Delay, 10), Unsigned_64'Last, 30, R);
   Verify (W.Deadline_Overflow, 30); Check (Sleeps = 0, "absolute target cannot wrap");
   Reset (90);
   W.Run ((AML_Delays.Stall_Delay, 10), 100, 30, R);
   Verify (W.Delay_Exceeds_Deadline, 30);
   Reset (100);
   W.Run ((AML_Delays.Sleep_Delay, 0), 100, 30, R);
   Verify (W.Deadline_Expired, 30); Check (Sleeps = 0, "expired owner cannot replenish with zero Sleep");
   Ada.Text_IO.Put_Line ("Native delay policy checks" & Checks'Image);
end Policy_Tests;
