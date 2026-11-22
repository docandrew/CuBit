pragma Ada_2022;
with ACPI_Deadline_Conversion;
package body ACPI_Global_Lock_Maintenance with SPARK_Mode is
   use type Interfaces.Unsigned_64, Owner.Phase, Owner.Outcome, Sessions.Token;
   package C renames ACPI_Deadline_Conversion;
   function Wake (Instant : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (C.Ceiling_Milliseconds (C.Microsecond_Instant (Instant))));
   procedure Run (State : in out Context; Lock : in out Requests.Context; Result : out Report) is
      Info : Sessions.Statistics := Requests.Observe (Lock);
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
      procedure Execute is
      begin
         if Info.Engine.Stage in Owner.Acquire_Uncertain | Owner.Release_Uncertain | Owner.Signal_Uncertain then
            Result.Status := Uncertain; return;
         elsif Info.Current = Sessions.No_Token then
            State.Next_Retry := 0; State.Failed_Clock_Cleanup := False;
            State.Scheduling_Exhausted := False; return;
         elsif State.Scheduling_Exhausted then Result.Status := Clock_Stopped; return;
         end if;
         Clock (Now, Available);
         if Available and then State.Seen_Clock and then Now < State.Last_Clock then State.Clock_Broken := True; end if;
         Available := Available and then not State.Clock_Broken;
         if Available then State.Seen_Clock := True; State.Last_Clock := Now; State.Failed_Clock_Cleanup := False;
         elsif State.Failed_Clock_Cleanup then Result.Status := Clock_Stopped; return;
         end if;
         if Available and then Now < Info.Deadline and then not Requests.Closing (Lock) then
            Result.Status := Scheduled; Result.Deadline := Wake (Info.Deadline); return;
         elsif Available and then State.Next_Retry /= 0 and then Now < State.Next_Retry then
            Result.Status := Cleanup_Deferred; Result.Deadline := Wake (State.Next_Retry); return;
         end if;
         if Requests.Closing (Lock) then Requests.Cleanup (Lock, Result.Cleanup);
         else Requests.Expire (Lock, Now, Available, Result.Cleanup); end if;
         Info := Requests.Observe (Lock);
         if Info.Engine.Stage in Owner.Acquire_Uncertain | Owner.Release_Uncertain | Owner.Signal_Uncertain then
            Result.Status := Uncertain; return;
         elsif Info.Current = Sessions.No_Token then
            State.Next_Retry := 0; State.Failed_Clock_Cleanup := False;
            Result.Status := (if Result.Cleanup = Owner.Released then Released else Idle); return;
         end if;
         if Available and then Now < Interfaces.Unsigned_64'Last - Interfaces.Unsigned_64 (Retry_Microseconds) then
            State.Next_Retry := Now + Interfaces.Unsigned_64 (Retry_Microseconds);
            Result.Status := Cleanup_Deferred; Result.Deadline := Wake (State.Next_Retry);
         else
            State.Scheduling_Exhausted := Available;
            State.Failed_Clock_Cleanup := True; Result.Status := Clock_Stopped;
         end if;
      end Execute;
   begin
      Result := (others => <>);
      if State.Working then Result.Status := Reentered; return; end if;
      State.Working := True; Execute; State.Working := False;
   end Run;
end ACPI_Global_Lock_Maintenance;
