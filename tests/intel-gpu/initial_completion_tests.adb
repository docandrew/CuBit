with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Initial_Completion;
procedure Initial_Completion_Tests is
   Owner, Read_OK, Event_OK : Boolean;
   Marker, Clock : Unsigned_64;
   Reads, Events, Pauses, Clocks, Scenario : Natural;
   function Owned return Boolean is (Owner);
   function Now return Unsigned_64 is
   begin
      Clocks := Clocks + 1;
      if Scenario = 12 and Clocks = 5 then Owner := False; end if;
      return Clock;
   end Now;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Reads := Reads + 1; Value := Marker; OK := Read_OK;
      if Scenario = 7 and Reads = 2 then Owner := False; end if;
      if Scenario = 8 and Reads = 2 then Clock := 1_000_100; end if;
   end Read_Marker;
   function Service return Boolean is
   begin
      Events := Events + 1;
      if Scenario = 9 then Owner := False; end if;
      if Scenario = 10 then Clock := 99; end if;
      if Scenario = 11 then Clock := Unsigned_64'Last; end if;
      return Event_OK;
   end Service;
   procedure Pause is
   begin
      Pauses := Pauses + 1;
      if Scenario = 0 then Marker := 1; end if;
      if Scenario = 17 then
         Marker := (if Pauses = 1 then 0 else 2);
      end if;
   end Pause;
   package Completion is new Intel_GPU_Initial_Completion
     (Owned, Read_Marker, Service, Now, Pause);
   use type Completion.Result;
   use type Completion.Phase;
   Status : Completion.Result;
begin
   for S in 0 .. 16 loop
      declare
         Object : Completion.Attempt;
         Old_Reads, Old_Events, Old_Clocks : Natural;
      begin
         Scenario := S; Owner := True; Read_OK := True; Event_OK := True;
         Marker := 0; Clock := 100; Reads := 0; Events := 0; Pauses := 0; Clocks := 0;
         if S = 1 then Marker := 1;
         elsif S = 2 then Read_OK := False;
         elsif S = 3 then Clock := Unsigned_64'Last;
         end if;
         Completion.Wait (Object, 3, Status);
         pragma Assert (Status = Completion.Rejected and Clocks = 0 and Reads = 0);
         Completion.Arm (Object, Status);
         if S in 1 .. 3 then
            pragma Assert (Completion.State (Object) = Completion.Quarantined);
            case S is
               when 1 => pragma Assert (Status = Completion.Unexpected_Marker);
               when 2 => pragma Assert (Status = Completion.Read_Failed);
               when others => pragma Assert (Status = Completion.Invalid_Clock);
            end case;
         else
            pragma Assert (Status = Completion.Ready);
            case S is
               when 4 => Event_OK := False;
               when 5 => Marker := 2;
               when 13 => Read_OK := False;
               when 14 => Clock := 1_000_100;
               when 15 => Marker := 16#100000001#;
               when 16 => Owner := False;
               when 7 | 8 | 9 | 10 | 11 | 12 => Marker := 1;
               when others => null;
            end case;
            Completion.Wait (Object, 3, Status);
            case S is
               when 0 => pragma Assert (Status = Completion.Complete and Events = 2 and Pauses = 1);
               when 4 => pragma Assert (Status = Completion.Event_Failed and Reads = 1);
               when 5 => pragma Assert (Status = Completion.Unexpected_Marker);
               when 6 => pragma Assert (Status = Completion.Timed_Out and Events = 3 and Pauses = 3);
               when 7 | 9 | 12 => pragma Assert (Status = Completion.Ownership_Lost);
               when 8 => pragma Assert (Status = Completion.Timed_Out);
               when 10 | 11 => pragma Assert (Status = Completion.Invalid_Clock);
               when 13 => pragma Assert (Status = Completion.Read_Failed);
               when 14 => pragma Assert (Status = Completion.Timed_Out and Events = 0);
               when 15 => pragma Assert (Status = Completion.Unexpected_Marker);
               when 16 => pragma Assert (Status = Completion.Ownership_Lost and Events = 0);
               when others => raise Program_Error;
            end case;
            pragma Assert (Completion.State (Object) =
              (if S = 0 then Completion.Observed else Completion.Quarantined));
         end if;
         Old_Reads := Reads; Old_Events := Events; Old_Clocks := Clocks;
         pragma Assert (Completion.Marker_Reads (Object) = Reads);
         if S = 0 then pragma Assert (Completion.Last_Marker (Object) = 1); end if;
         Completion.Arm (Object, Status);
         pragma Assert (Status = Completion.Rejected);
         Completion.Wait (Object, 3, Status);
         pragma Assert (Status = Completion.Rejected and Reads = Old_Reads and
                        Events = Old_Events and Clocks = Old_Clocks);
      end;
   end loop;
   for S in 17 .. 25 loop
      declare
         Object : Completion.Attempt;
         Prior : Unsigned_32 := 1;
         Target : Unsigned_32 := 2;
      begin
         Scenario := S; Owner := True; Read_OK := True; Event_OK := True;
         Marker := 1; Clock := 100; Reads := 0; Events := 0; Pauses := 0; Clocks := 0;
         case S is
            when 20 => Marker := 2; -- already-complete value before publication
            when 21 => Target := 1; -- repeated sequence
            when 22 => Prior := Unsigned_32'Last; Target := 0; -- wrap
            when 23 => Target := 3; -- skipped sequence
            when 24 => Prior := Unsigned_32'Last - 1; Target := Unsigned_32'Last;
                       Marker := Unsigned_64 (Prior);
            when others => null;
         end case;
         Completion.Arm (Object, Status, Prior, Target);
         if S in 21 .. 23 then
            pragma Assert (Status = Completion.Rejected and Reads = 0 and Clocks = 0);
         elsif S = 20 then
            pragma Assert (Status = Completion.Unexpected_Marker);
         else
            pragma Assert (Status = Completion.Ready);
            if S = 19 then Marker := 3;
            elsif S = 24 then Marker := Unsigned_64 (Target);
            elsif S = 25 then Marker := 16#100000002#;
            end if;
            Completion.Wait (Object, 3, Status);
            case S is
               when 17 => pragma Assert (Status = Completion.Complete and Pauses = 2);
               when 18 => pragma Assert (Status = Completion.Timed_Out);
               when 19 | 25 => pragma Assert (Status = Completion.Unexpected_Marker);
               when 24 => pragma Assert (Status = Completion.Complete);
               when others => raise Program_Error;
            end case;
         end if;
         pragma Assert (Completion.State (Object) =
           (if S in 17 | 24 then Completion.Observed else Completion.Quarantined));
         Completion.Arm (Object, Status, Prior, Target);
         pragma Assert (Status = Completion.Rejected);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Completion PASS: first/repeated sequences, stale markers, no wrap, bounded wait and failures");
end Initial_Completion_Tests;
