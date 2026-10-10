with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Initial_Completion;
-- Non-blocking completion observation against a hosted timeline slot and
-- clock (GPU-001 step 1). Observe never waits: each call is one ownership
-- check, one timeline read and one clock read. Hosted model only.
procedure Initial_Completion_Tests is
   Budget : constant Unsigned_64 := 1_000_000;
   Owner, Read_OK : Boolean;
   Marker, Clock : Unsigned_64;
   Reads, Clocks, Scenario : Natural;
   function Owned return Boolean is (Owner);
   function Now return Unsigned_64 is
   begin
      Clocks := Clocks + 1;
      if Scenario = 12 and Clocks = 3 then Owner := False; end if;
      return Clock;
   end Now;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Reads := Reads + 1; Value := Marker; OK := Read_OK;
      if Scenario = 7 and Reads = 3 then Owner := False; end if;
   end Read_Marker;
   package Completion is new Intel_GPU_Initial_Completion (Owned, Read_Marker, Now);
   use type Completion.Result;
   use type Completion.Phase;
   Status : Completion.Result;
   -- Boot/registration helper: bounded, services events between polls.
   Events, Pauses : Natural := 0;
   Event_OK : Boolean := True;
   function Service return Boolean is
   begin Events := Events + 1; return Event_OK; end Service;
   procedure Pause is
   begin
      Pauses := Pauses + 1; Clock := Clock + 1;
      if Scenario = 30 and Pauses = 3 then Marker := 1; end if;
   end Pause;
   procedure Wait is new Completion.Wait (Service, Pause);
   procedure Reset (S : Natural) is
   begin
      Scenario := S; Owner := True; Read_OK := True; Event_OK := True;
      Marker := 0; Clock := 100; Reads := 0; Clocks := 0; Events := 0; Pauses := 0;
   end Reset;
begin
   -- Arm preconditions: the slot must still hold the previous value.
   for S in 0 .. 4 loop
      declare
         Object : Completion.Attempt;
      begin
         Reset (S);
         Completion.Observe (Object, True, Status);
         pragma Assert (Status = Completion.Rejected and Clocks = 0 and Reads = 0);
         case S is
            when 1 => Marker := 1;
            when 2 => Read_OK := False;
            when 3 => Clock := Unsigned_64'Last;
            when 4 => Owner := False;
            when others => null;
         end case;
         Completion.Arm (Object, Budget, Status);
         case S is
            when 0 => pragma Assert (Status = Completion.Ready and
                                     Completion.State (Object) = Completion.Armed);
            when 1 => pragma Assert (Status = Completion.Unexpected_Marker);
            when 2 => pragma Assert (Status = Completion.Read_Failed);
            when 3 => pragma Assert (Status = Completion.Invalid_Clock and Reads = 0);
            when others => pragma Assert (Status = Completion.Ownership_Lost and Reads = 0);
         end case;
         if S /= 0 then
            pragma Assert (Completion.State (Object) = Completion.Quarantined);
            Completion.Arm (Object, Budget, Status);
            pragma Assert (Status = Completion.Rejected);
         end if;
      end;
   end loop;
   -- Zero budget is not a deadline: explicit limits only.
   declare
      Object : Completion.Attempt;
   begin
      Reset (5);
      Completion.Arm (Object, 0, Status);
      pragma Assert (Status = Completion.Rejected and Reads = 0);
   end;
   -- Observe is one step: it never loops, never pauses, never services.
   for S in 6 .. 16 loop
      declare
         Object : Completion.Attempt;
         Old_Reads : Natural;
      begin
         Reset (S);
         Completion.Arm (Object, Budget, Status);
         pragma Assert (Status = Completion.Ready);
         Old_Reads := Reads;
         Completion.Observe (Object, True, Status);
         pragma Assert (Reads = Old_Reads + 1 and Pauses = 0 and Events = 0);
         pragma Assert (Status = Completion.Pending and
                        Completion.State (Object) = Completion.Armed);
         case S is
            when 6 => Marker := 1;                       -- reached
            when 7 => Marker := 1;                       -- owner lost during read
            when 8 => Marker := 1; Clock := 100 + Budget + 5; -- reached, seen late
            when 9 => Clock := 100 + Budget;             -- deadline, not reached
            when 10 => Clock := 99;                      -- clock went backwards
            when 11 => Clock := Unsigned_64'Last;        -- clock unavailable
            when 12 => Marker := 1;                      -- owner lost at clock read
            when 13 => Read_OK := False;
            when 14 => Marker := 2;                      -- beyond published
            when 15 => Marker := 16#1_0000_0001#;        -- upper half set
            when others => Owner := False;
         end case;
         Completion.Observe (Object, True, Status);
         case S is
            when 6 | 8 => pragma Assert (Status = Completion.Complete);
            when 7 | 12 | 16 => pragma Assert (Status = Completion.Ownership_Lost);
            when 9 => pragma Assert (Status = Completion.Timed_Out);
            when 10 | 11 => pragma Assert (Status = Completion.Invalid_Clock);
            when 13 => pragma Assert (Status = Completion.Read_Failed);
            when others => pragma Assert (Status = Completion.Unexpected_Marker);
         end case;
         pragma Assert (Completion.State (Object) =
           (if S in 6 | 8 then Completion.Observed else Completion.Quarantined));
         Old_Reads := Reads;
         Completion.Observe (Object, True, Status);
         pragma Assert (Status = Completion.Rejected and Reads = Old_Reads);
      end;
   end loop;
   -- The timeline no longer has a transient zero: barriers write scratch,
   -- so zero after a non-zero previous value is a regression, not progress.
   declare
      Object : Completion.Attempt;
   begin
      Reset (17); Marker := 1;
      Completion.Arm (Object, Budget, Status, 1, 2);
      pragma Assert (Status = Completion.Ready);
      Marker := 0;
      Completion.Observe (Object, True, Status);
      pragma Assert (Status = Completion.Unexpected_Marker and
                     Completion.State (Object) = Completion.Quarantined);
   end;
   -- A closed gate (e.g. MODE_DONE not yet drained) holds a reached
   -- timeline Pending until the gate opens, within the deadline only.
   declare
      Object, Late : Completion.Attempt;
   begin
      Reset (18);
      Completion.Arm (Object, Budget, Status);
      Marker := 1;
      for Turn in 1 .. 5 loop
         Completion.Observe (Object, False, Status);
         pragma Assert (Status = Completion.Pending);
      end loop;
      Completion.Observe (Object, True, Status);
      pragma Assert (Status = Completion.Complete);
      Reset (19);
      Completion.Arm (Late, Budget, Status);
      Marker := 1; Clock := 100 + Budget;
      Completion.Observe (Late, False, Status);
      pragma Assert (Status = Completion.Timed_Out);
   end;
   -- Sequences: an observed attempt re-arms for its successor; repeats,
   -- skips and wrap are rejected before any read.
   declare
      Object : Completion.Attempt;
   begin
      Reset (20);
      for Sequence in Unsigned_32 range 1 .. 1_000 loop
         Completion.Arm (Object, Budget, Status, Sequence - 1, Sequence);
         pragma Assert (Status = Completion.Ready);
         Completion.Observe (Object, True, Status);
         pragma Assert (Status = Completion.Pending);
         Marker := Unsigned_64 (Sequence); Clock := Clock + 10;
         Completion.Observe (Object, True, Status);
         pragma Assert (Status = Completion.Complete);
      end loop;
      pragma Assert (Completion.Last_Marker (Object) = 1_000);
   end;
   for S in 21 .. 24 loop
      declare
         Object : Completion.Attempt;
         Prior : Unsigned_32 := 1;
         Target : Unsigned_32 := 2;
      begin
         Reset (S); Marker := 1;
         case S is
            when 21 => Target := 1;                                -- repeat
            when 22 => Prior := Unsigned_32'Last; Target := 0;     -- wrap
            when 23 => Target := 3;                                -- skip
            when others => Prior := Unsigned_32'Last - 1; Target := Unsigned_32'Last;
                           Marker := Unsigned_64 (Prior);
         end case;
         Completion.Arm (Object, Budget, Status, Prior, Target);
         if S = 24 then
            pragma Assert (Status = Completion.Ready);
            Marker := Unsigned_64 (Target);
            Completion.Observe (Object, True, Status);
            pragma Assert (Status = Completion.Complete);
         else
            pragma Assert (Status = Completion.Rejected and Reads = 0 and Clocks = 0);
         end if;
      end;
   end loop;
   -- The synchronous helper used only at startup and registration.
   declare
      Object, Hung, Failing : Completion.Attempt;
   begin
      Reset (30);
      Completion.Arm (Object, Budget, Status);
      Wait (Object, 10, Status);
      pragma Assert (Status = Completion.Complete and Pauses = 3 and Events = 3);
      Reset (31);
      Completion.Arm (Hung, Budget, Status);
      Wait (Hung, 10, Status);
      pragma Assert (Status = Completion.Timed_Out and Pauses = 10 and
                     Completion.State (Hung) = Completion.Quarantined);
      Reset (32); Event_OK := False;
      Completion.Arm (Failing, Budget, Status);
      Wait (Failing, 10, Status);
      pragma Assert (Status = Completion.Event_Failed and Pauses = 0);
   end;
   Ada.Text_IO.Put_Line ("Completion PASS: non-blocking observe, explicit deadlines, gate, no transient zero, re-arm, no wrap, bounded startup wait");
end Initial_Completion_Tests;
