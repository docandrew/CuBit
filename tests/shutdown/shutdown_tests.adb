--  Hosted tests of the root stop's pure parts (PWR-003 slice 1):
--    * Shutdown_Sequences: units stop in stage order (Apps, Services, then
--      after storage Drivers), each stage in reverse start order; kept units
--      never; a unit that obeys is Stopped_Clean or Exited_Failing; one
--      that hangs is ended only at its grace, Forced, or Not_Ended when it
--      cannot be ended or its exit never comes; one that ends on its own
--      before its turn is Ended_Early and never sent Stop; then storage,
--      devmgr's drivers (refused, polled, timed out), Answer; Finished means
--      every unit is kept or decided. Contracts are checked (-gnata).
--    * Power_Decisions: every answer to a request, the button's policy and
--      the watch's baseline.
--    * The wire: CuBit.System_Stops' request check and packed counts,
--      CuBit.Power_Requests' badges, CuBit.Power_Event_Exchange's PM1 value.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.System_Stops;
with CuBit.Power_Requests;
with CuBit.Power_Event_Exchange;
with CuBit.Close_Policies;
with Shutdown_Sequences;
with Power_Decisions;

procedure Shutdown_Tests is
   package SQ renames Shutdown_Sequences;
   package STP renames CuBit.System_Stops;
   package PR renames CuBit.Power_Requests;
   package PE renames CuBit.Power_Event_Exchange;
   package PD renames Power_Decisions;
   use type SQ.Action, SQ.Unit_State, SQ.Phase;
   use type STP.Storage_Outcome, STP.Driver_Outcome;
   use type PR.Answer, PR.Action, PR.Requester, PE.Watch_Status;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Unit (Process : Unsigned_64; Stage : SQ.Stop_Stage; Key : Unsigned_64; Grace : SQ.Grace_Ms;
                  Kept : Boolean := False) return SQ.Unit is
     ((Process => Process, Stage => Stage, Key => Key, Grace => Grace,
       State => (if Kept then SQ.Kept else SQ.Pending), others => <>));

   ---------------------------------------------------------------------------
   --  A whole stop in order; every unit obeys at once.
   ---------------------------------------------------------------------------
   procedure Test_Order is
      --  Array order deliberately not stop order.
      --  Keys are launch order; array order deliberately not stop order.
      Units : SQ.Unit_Array :=
        [Unit (11, SQ.Services, 1, 100),            --  logstore, started first
         Unit (12, SQ.Services, 3, 100),
         Unit (13, SQ.Apps, 7, 100),                --  an app started 7th
         Unit (14, SQ.Services, 2, 100, Kept => True),  --  the requester
         Unit (15, SQ.Apps, 9, 100),                --  a later app (a child of 13)
         Unit (16, SQ.Power_Tail, 4, 100),          --  declared Power_Tail (a Halt stops it)
         Unit (17, SQ.Power_Tail, 8, 100),
         Unit (18, SQ.Services, 5, 100)];           --  a child of 12 (started after it)
      S : SQ.Sequence;
      Next : SQ.Action;
      Now : SQ.Time := 1_000;
      Expected : constant array (1 .. 5) of Unsigned_64 := [15, 13, 18, 12, 11];
      --  (18 started after 12: a child stops before its supervisor.)
      Expected_Tail : constant array (1 .. 2) of Unsigned_64 := [17, 16];
   begin
      SQ.Begin_Stop (S, Units, Units'Length, 500, False, Now, Next);
      for E of Expected loop
         Check (Next = SQ.Send_Stop and then Units (S.Current).Process = E,
                "order: stop" & Unsigned_64'Image (E) & " next");
         SQ.Stop_Sent (S, Units, Units'Length, True);
         Now := Now + 10;
         SQ.Exited (S, Units, Units'Length, E, True, Now, Next);
      end loop;
      Check (Next = SQ.Commit_Storage and then S.State = SQ.Committing, "order: storage after the services");
      Check (Units (4).State = SQ.Kept, "order: the kept unit was never stopped");
      SQ.Storage_Done (S, Units, Units'Length, STP.Committed, Now, Next);
      Check (S.Storage = STP.Committed and then Next = SQ.Ask_Drivers and then S.State = SQ.Asking_Drivers,
             "order: storage, then devmgr's drivers");
      SQ.Drivers_Asked (S, Units, Units'Length, True, Now, Next);
      Check (Next = SQ.Nothing and then S.State = SQ.Stopping_Drivers, "drivers: asked");
      SQ.Tick (S, Units, Units'Length, Now + SQ.Driver_Poll_Ms - 1, Next);
      Check (Next = SQ.Nothing, "drivers: no poll before its interval");
      SQ.Tick (S, Units, Units'Length, Now + SQ.Driver_Poll_Ms, Next);
      Check (Next = SQ.Poll_Drivers, "drivers: polled at its interval");
      SQ.Drivers_Polled (S, Units, Units'Length, False, Now + SQ.Driver_Poll_Ms, Next);
      Check (Next = SQ.Nothing and then S.State = SQ.Stopping_Drivers, "drivers: still stopping");
      SQ.Drivers_Polled (S, Units, Units'Length, True, Now + 2 * SQ.Driver_Poll_Ms, Next);
      Check (S.Drivers = STP.Drivers_Stopped and then S.Tail, "drivers: done, then the Power_Tail");
      for E of Expected_Tail loop
         Check (Next = SQ.Send_Stop and then Units (S.Current).Process = E,
                "order: Power_Tail" & Unsigned_64'Image (E) & " last");
         SQ.Stop_Sent (S, Units, Units'Length, True);
         SQ.Exited (S, Units, Units'Length, E, False, Now, Next);
      end loop;
      Check (Next = SQ.Answer and then S.State = SQ.Finished, "the tail done: answer");
      Check (Units (6).State = SQ.Exited_Failing, "order: a failing exit is Exited_Failing");
      Check (SQ.Count (Units, Units'Length, SQ.Stopped_Clean) = 5
             and then SQ.Count (Units, Units'Length, SQ.Exited_Failing) = 2
             and then SQ.Count (Units, Units'Length, SQ.Kept) = 1, "order: counts");
      Check (SQ.Wake_At (S) = 0, "finished: nothing to wake for");
   end Test_Order;

   ---------------------------------------------------------------------------
   --  A unit that hangs: ended only at its grace, Forced on its exit; one
   --  that cannot be ended; one whose exit never comes.
   ---------------------------------------------------------------------------
   procedure Test_Hang is
      Units : SQ.Unit_Array :=
        [Unit (21, SQ.Services, 3, 500),
         Unit (22, SQ.Services, 2, 300),
         Unit (23, SQ.Services, 1, 200)];
      S : SQ.Sequence;
      Next : SQ.Action;
      Start : constant SQ.Time := 10_000;
   begin
      SQ.Begin_Stop (S, Units, Units'Length, 500, False, Start, Next);
      Check (Next = SQ.Send_Stop and then Units (S.Current).Process = 21, "hang: 21 first");
      Check (SQ.Wake_At (S) = Start + 500, "hang: wakes at its grace");
      SQ.Stop_Sent (S, Units, Units'Length, False);
      SQ.Tick (S, Units, Units'Length, Start + 499, Next);
      Check (Next = SQ.Nothing and then Units (1).State = SQ.Stopping, "hang: not ended before its grace");
      SQ.Tick (S, Units, Units'Length, Start + 500, Next);
      Check (Next = SQ.End_Unit and then Units (1).State = SQ.Ending, "hang: ended at its grace");
      SQ.End_Sent (S, Units, Units'Length, True, Start + 500, Next);
      Check (Next = SQ.Nothing, "hang: ending accepted");
      SQ.Exited (S, Units, Units'Length, 21, False, Start + 520, Next);
      Check (Units (1).State = SQ.Forced and then not Units (1).Stop_Taken, "hang: Forced, Stop not taken noted");
      Check (Next = SQ.Send_Stop and then Units (S.Current).Process = 22, "hang: then 22");
      SQ.Tick (S, Units, Units'Length, Start + 520 + 300, Next);
      Check (Next = SQ.End_Unit, "refused: 22 at its grace");
      SQ.End_Sent (S, Units, Units'Length, False, Start + 820, Next);
      Check (Units (2).State = SQ.Force_Failed, "refused: Not_Ended when it cannot be ended");
      Check (Next = SQ.Send_Stop and then Units (S.Current).Process = 23, "refused: then 23");
      SQ.Tick (S, Units, Units'Length, Start + 820 + 200, Next);
      Check (Next = SQ.End_Unit, "silent: 23 at its grace");
      SQ.End_Sent (S, Units, Units'Length, True, Start + 1020, Next);
      SQ.Tick (S, Units, Units'Length, Start + 1020 + SQ.End_Deadline_Ms - 1, Next);
      Check (Next = SQ.Nothing and then Units (3).State = SQ.Ending, "silent: waits for its exit");
      SQ.Tick (S, Units, Units'Length, Start + 1020 + SQ.End_Deadline_Ms, Next);
      Check (Units (3).State = SQ.Force_Failed and then Next = SQ.Commit_Storage,
             "silent: Not_Ended at the end deadline, then storage");
      SQ.Storage_Done (S, Units, Units'Length, STP.Storage_Timed_Out, Start + 4000, Next);
      Check (Next = SQ.Ask_Drivers, "storage timed out: devmgr next");
      SQ.Drivers_Asked (S, Units, Units'Length, False, Start + 4000, Next);
      Check (Next = SQ.Answer and then S.Drivers = STP.Drivers_Refused, "drivers refused, no tail: answer");
   end Test_Hang;

   ---------------------------------------------------------------------------
   --  Ended early, an unknown exit, nothing at all, devmgr timing out.
   ---------------------------------------------------------------------------
   procedure Test_Edges is
      --  33: the Power_Tail, kept for a power-off.
      Units : SQ.Unit_Array :=
        [Unit (31, SQ.Services, 2, 100), Unit (32, SQ.Services, 1, 100),
         Unit (33, SQ.Power_Tail, 1, 100, Kept => True)];
      Empty : SQ.Unit_Array (1 .. 1) := [Unit (99, SQ.Apps, 0, 100)];
      S, E : SQ.Sequence;
      Next : SQ.Action;
   begin
      SQ.Begin_Stop (S, Units, Units'Length, 1000, False, 0, Next);
      SQ.Stop_Sent (S, Units, Units'Length, True);
      SQ.Exited (S, Units, Units'Length, 32, True, 5, Next);
      Check (Next = SQ.Nothing and then Units (2).State = SQ.Ended_Early and then Units (2).Stop_At = 0,
             "early: Ended_Early, never sent Stop");
      SQ.Exited (S, Units, Units'Length, 77, True, 6, Next);
      Check (Next = SQ.Nothing and then S.State = SQ.Stopping_Unit, "unknown exit: ignored");
      SQ.Exited (S, Units, Units'Length, 31, True, 7, Next);
      Check (Next = SQ.Commit_Storage, "early: skipped, storage next");
      SQ.Storage_Done (S, Units, Units'Length, STP.Partly_Committed, 8, Next);
      Check (Next = SQ.Ask_Drivers, "then devmgr");
      SQ.Drivers_Asked (S, Units, Units'Length, True, 10, Next);
      SQ.Tick (S, Units, Units'Length, 10 + 1000 + SQ.Driver_Allowance_Ms, Next);
      Check (Next = SQ.Answer and then S.Drivers = STP.Drivers_Timed_Out and then Units (3).State = SQ.Kept,
             "devmgr timed out; the kept Power_Tail never stopped: answer");
      --  No units at all: straight to storage.
      SQ.Begin_Stop (E, Empty, 0, 1000, False, 0, Next);
      Check (Next = SQ.Commit_Storage, "nothing to stop: storage first");
   end Test_Edges;

   ---------------------------------------------------------------------------
   --  Stop_Apps_Operation, then Stop_Operation: apps first (power.svc has
   --  acpi.svc prepare to sleep between), then the rest on the same
   --  sequence.
   ---------------------------------------------------------------------------
   procedure Test_Two_Phases is
      Apps : SQ.Unit_Array := [Unit (41, SQ.Apps, 5, 100), Unit (42, SQ.Apps, 6, 100)];
      Rest : SQ.Unit_Array := [Unit (43, SQ.Services, 1, 100), Unit (44, SQ.Services, 2, 100, Kept => True)];
      None : SQ.Unit_Array (1 .. 1) := [Unit (98, SQ.Apps, 0, 100)];
      S, T : SQ.Sequence;
      Next : SQ.Action;
   begin
      SQ.Begin_Stop (S, Apps, Apps'Length, 500, True, 0, Next);
      Check (Next = SQ.Send_Stop and then Apps (S.Current).Process = 42, "apps only: the later app first");
      SQ.Exited (S, Apps, Apps'Length, 42, True, 1, Next);
      SQ.Exited (S, Apps, Apps'Length, 41, True, 2, Next);
      Check (Next = SQ.Answer and then S.State = SQ.Finished and then S.Storage = STP.Not_Asked
             and then S.Drivers = STP.Not_Asked, "apps only: answered with no storage or driver stage");
      SQ.Begin_Stop (S, Rest, Rest'Length, 500, False, 3, Next);
      Check (Next = SQ.Send_Stop and then Rest (S.Current).Process = 43 and then not S.Apps_Only,
             "then the rest: services, the kept one never");
      SQ.Exited (S, Rest, Rest'Length, 43, True, 4, Next);
      Check (Next = SQ.Commit_Storage, "then the rest: storage");
      SQ.Begin_Stop (T, None, 0, 500, True, 0, Next);
      Check (Next = SQ.Answer, "apps only with no apps: answered at once");
   end Test_Two_Phases;

   procedure Test_Close_Policy is
      package CP renames CuBit.Close_Policies;
      use type CP.Outcome;
      None : constant CP.Delay_Capability := (Granted => False, Declared_Max => 0);
      Cap : constant CP.Delay_Capability := (Granted => True, Declared_Max => 30_000);
   begin
      Check (CP.Allowed_Delay (None, 60_000) = 0 and then CP.Allowed_Delay (Cap, 60_000) = 30_000
             and then CP.Allowed_Delay (Cap, 10_000) = 10_000, "close: delay only with the capability, capped");
      Check (CP.Decide (None, 60_000, False, CP.No_Answer, 100, 2_000, 2_099) = CP.Waiting
             and then CP.Decide (None, 60_000, False, CP.No_Answer, 100, 2_000, 2_100) = CP.Forced_After_Grace,
             "close: without the capability, forced at the grace");
      Check (CP.Decide (None, 0, False, CP.Cancel_Shutdown, 100, 2_000, 200) = CP.Waiting,
             "close: without the capability, no cancel");
      Check (CP.Decide (Cap, 60_000, False, CP.No_Answer, 100, 2_000, 2_100) = CP.Waiting
             and then CP.Decide (Cap, 60_000, False, CP.No_Answer, 100, 2_000, 30_100) = CP.Forced_After_Delay,
             "close: with it, forced only at the declared maximum");
      Check (CP.Decide (Cap, 60_000, False, CP.Cancel_Shutdown, 100, 2_000, 500) = CP.Shutdown_Cancelled,
             "close: Cancel cancels the shutdown");
      Check (CP.Decide (Cap, 60_000, False, CP.Force_Confirmed, 100, 2_000, 500) = CP.Forced_By_User,
             "close: forced earlier only by confirmation");
      Check (CP.Decide (Cap, 60_000, True, CP.Discard, 100, 2_000, 500) = CP.Closed, "close: Discard, then it ends");
   end Test_Close_Policy;

   procedure Test_Decisions is
      Seen : Unsigned_64 := 0;
      Again, Pressed : Boolean;
   begin
      Check (PD.Answer_For (PD.Idle, PR.Console, True, PR.Request_Words, 1,
                            PR.Action'Enum_Rep (PR.Power_Off)) = PR.Accepted, "answer: accepted");
      Check (PD.Answer_For (PD.Idle, PR.Unknown, True, PR.Request_Words, 1,
                            PR.Action'Enum_Rep (PR.Power_Off)) = PR.Not_Approved, "answer: unbadged");
      Check (PD.Answer_For (PD.Idle, PR.Power_Button, True, PR.Request_Words, 1,
                            PR.Action'Enum_Rep (PR.Power_Off)) = PR.Not_Approved,
             "answer: the button is no requester");
      Check (PD.Answer_For (PD.Idle, PR.Desktop, True, PR.Request_Words, 0,
                            PR.Action'Enum_Rep (PR.Halt)) = PR.Malformed, "answer: id zero");
      Check (PD.Answer_For (PD.Idle, PR.Desktop, True, PR.Request_Words, 1, 9) = PR.Malformed,
             "answer: unknown action");
      Check (PD.Answer_For (PD.Idle, PR.Desktop, True, 1, 1, PR.Action'Enum_Rep (PR.Halt)) = PR.Malformed,
             "answer: short");
      Check (PD.Answer_For (PD.Stopping, PR.Desktop, True, PR.Request_Words, 1,
                            PR.Action'Enum_Rep (PR.Halt)) = PR.Busy, "answer: busy");
      Check (PD.Answer_For (PD.Idle, PR.Desktop, False, PR.Request_Words, 1,
                            PR.Action'Enum_Rep (PR.Halt)) = PR.Not_Ready, "answer: not registered");
      Check (PR.Requester_Of (PR.Desktop_Tag) = PR.Desktop and then PR.Requester_Of (PR.Console_Tag) = PR.Console
             and then PR.Requester_Of (0) = PR.Unknown and then PR.Requester_Of (PE.Authority_Tag) = PR.Unknown,
             "badges: classified");
      Check (PD.Press_Starts (PD.Idle, PD.Power_Off) and then not PD.Press_Starts (PD.Idle, PD.Ignore)
             and then not PD.Press_Starts (PD.Stopping, PD.Power_Off)
             and then PD.Press_Action (PD.Halt) = PR.Halt, "button policy");
      PD.After_Watch (PE.Pressed, 2, Seen, Again, Pressed);
      Check (Pressed and then Again and then Seen = 2, "watch: two presses coalesced");
      PD.After_Watch (PE.Superseded, 2, Seen, Again, Pressed);
      Check (not Pressed and then Again and then Seen = 2, "watch: superseded keeps the baseline");
      PD.After_Watch (PE.Counter_Reset, 0, Seen, Again, Pressed);
      Check (not Pressed and then Again and then Seen = 0, "watch: counter reset rebaselines");
      PD.After_Watch (PE.Watch_Unavailable, 0, Seen, Again, Pressed);
      Check (not Pressed and then not Again and then PD.Retry_Later (PE.Watch_Unavailable)
             and then not PD.Retry_Later (PE.Stopping) and then Seen = 0
             and then PE.To_Watch_Status (8) = PE.Watch_Unavailable,
             "watch: unavailable is retried later, the baseline kept");
      PD.After_Watch (PE.No_Power_Button, 0, Seen, Again, Pressed);
      Check (not Pressed and then not Again, "watch: no button, no more watches");
      Check (PD.Needs_Firmware (PR.Power_Off, True) and then not PD.Needs_Firmware (PR.Power_Off, False)
             and then not PD.Needs_Firmware (PR.Halt, True), "final step");
   end Test_Decisions;

   procedure Test_Wire is
      W : constant Unsigned_64 := STP.Grace_Word_Of (300, 10_000);
   begin
      Check (STP.Undeclared_Of (W) = 300 and then STP.Storage_Of (W) = 10_000, "wire: grace word");
      Check (STP.Request_Valid (4, 1, STP.Final_Step'Enum_Rep (STP.Power_Off), W, 1000), "wire: valid");
      Check (not STP.Request_Valid (4, 0, 1, W, 1000), "wire: id zero");
      Check (not STP.Request_Valid (4, 1, 3, W, 1000), "wire: final step unknown");
      Check (not STP.Request_Valid (4, 1, 1, 300, 1000), "wire: storage zero");
      Check (not STP.Request_Valid (4, 1, 1, W, 0), "wire: driver grace zero");
      Check (not STP.Request_Valid (4, 1, 1, W, STP.Maximum_Grace_Ms + 1), "wire: driver grace too long");
      Check (not STP.Request_Valid (3, 1, 1, W, 1000), "wire: short");
      declare
         C : constant Unsigned_64 := STP.Counts_Word (1, 2, 3, 70_000);
      begin
         Check (STP.Field_Of (C, 0) = 1 and then STP.Field_Of (C, 1) = 2 and then STP.Field_Of (C, 2) = 3
                and then STP.Field_Of (C, 3) = STP.Count_Ceiling, "wire: counts packed, ceiling held");
      end;
      Check ((STP.Stages_Word_Of (STP.Committed, 1, 0, STP.Drivers_Stopped, 5, 2) and 16#FF#) =
               STP.Storage_Outcome'Enum_Rep (STP.Committed), "wire: stages word");
      Check (PE.PM1_Sleep_Value (16#0001#, 5, True) = 16#3401#, "PM1: SLP_TYP 5 with SLP_EN keeps SCI_EN");
      Check (PE.Sleep_Request_Valid (3, 1, PE.Sleep_State'Enum_Rep (PE.S5), PE.Sleep_Confirmation)
             and then not PE.Sleep_Request_Valid (3, 1, 6, PE.Sleep_Confirmation)
             and then not PE.Sleep_Request_Valid (3, 1, 5, PE.Sleep_Confirmation - 1)
             and then not PE.Sleep_Request_Valid (3, 0, 5, PE.Sleep_Confirmation)
             and then not PE.Sleep_Request_Valid (2, 1, 5, PE.Sleep_Confirmation), "sleep request check");
      Check (PE.PM1_Sleep_Value (16#3C01#, 0, False) = 16#0001#, "PM1: old type and enable cleared");
   end Test_Wire;
begin
   Test_Order;
   Test_Hang;
   Test_Edges;
   Test_Two_Phases;
   Test_Close_Policy;
   Test_Decisions;
   Test_Wire;
   Put_Line ("shutdown tests:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failed");
   if Failures > 0 then
      raise Program_Error with "shutdown tests failed";
   end if;
end Shutdown_Tests;
