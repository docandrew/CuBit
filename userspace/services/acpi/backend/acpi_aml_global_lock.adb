pragma Ada_2022;
package body ACPI_AML_Global_Lock with SPARK_Mode is
   use type Interfaces.Unsigned_64, AML_Frame_Handles.Invocation_Domain;
   use type AML_Mutexes.Timeout, G.Operation, G.Outcome, Scope.P.Lock_Outcome;
   Microseconds_Per_Millisecond : constant Interfaces.Unsigned_64 := 1_000;
   function Observe (State : Context) return Report is (State.Info);
   procedure Clock (State : in out Context; Now : out Interfaces.Unsigned_64; Good : out Boolean) is
   begin
      Now := 0; Good := False;
      if State.Info.Clock_Broken then State.Info.Last_Reason := Clock_Reversed; return; end if;
      Read_Clock (Now, Good);
      if not Good then State.Info.Last_Reason := Clock_Unavailable; return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then
         State.Info.Clock_Broken := True; State.Info.Last_Reason := Clock_Reversed; Good := False; return;
      end if;
      State.Seen_Clock := True; State.Last_Clock := Now;
   end Clock;
   procedure Acquire (State : in out Context; Domain : AML_Frame_Handles.Invocation_Domain;
      Timeout : AML_Mutexes.Timeout; Outcome : out G.Outcome) is
      Window, Current_Window, Now, Limit, Remaining, Target : Interfaces.Unsigned_64;
      User_Limit : Boolean := False;
      Good, Accepted : Boolean;
      Token : Scope.AML_Borrow;
   begin
      Outcome := G.Refused; State.Info.Attempts := 0;
      if State.Info.Cleanup_Required or else Scope.AML_Borrowed (Transaction) then
         State.Info.Last_Reason := Already_Owned; return;
      elsif not Scope.Ready (Transaction) then State.Info.Last_Reason := Provider_Unavailable; return; end if;
      Inspect_Window (Window, Good);
      if not Good or else Window in 0 | Interfaces.Unsigned_64'Last or else Slice_Microseconds = 0 then
         State.Info.Last_Reason := Invalid_Window; return;
      end if;
      Clock (State, Now, Good); if not Good then return; end if;
      if Now >= Window then State.Info.Last_Reason := Invalid_Window; return; end if;
      Limit := Window;
      if Timeout not in AML_Mutexes.Poll_Only | AML_Mutexes.Wait_Forever then
         Remaining := Interfaces.Unsigned_64 (Timeout) * Microseconds_Per_Millisecond;
         if Remaining <= Window - Now then Limit := Now + Remaining; User_Limit := True; end if;
      end if;
      for Attempt in 1 .. Maximum_Wait_Slices loop
         Inspect_Window (Current_Window, Good);
         if not Good or else Current_Window /= Window then State.Info.Last_Reason := Window_Changed; return; end if;
         Clock (State, Now, Good); if not Good then return; end if;
         if Now >= Limit then
            State.Info.Last_Reason := (if User_Limit then Timeout_Elapsed else Invalid_Window);
            if User_Limit then Outcome := G.Timed_Out; end if; return;
         end if;
         State.Info.Attempts := Attempt_Count (Attempt);
         Scope.Begin_AML (Transaction, Domain, Token, Accepted,
           Maximum_Attempts => 1, Deadline_Limit => Limit);
         if Accepted then
            State.Owner := Domain; State.Token := Token; State.Info.Cleanup_Required := True;
            State.Info.Last_Reason := None; Outcome := G.Succeeded; return;
         elsif Scope.Pending (Transaction) then
            State.Owner := Domain; State.Token := Token; State.Info.Cleanup_Required := True;
            State.Info.Last_Reason := Physical_Indeterminate; Outcome := G.Indeterminate; return;
         elsif not Scope.Ready (Transaction)
           or else Scope.Last_Acquisition (Transaction) not in Scope.P.Contended | Scope.P.Waiting_For_Firmware | Scope.P.Expired then
            State.Info.Last_Reason := Physical_Refused; return;
         end if;
         if Timeout = AML_Mutexes.Poll_Only then
            State.Info.Last_Reason := Timeout_Elapsed; Outcome := G.Timed_Out; return;
         end if;
         Clock (State, Now, Good); if not Good then return; end if;
         if Now >= Limit then
            State.Info.Last_Reason := (if User_Limit then Timeout_Elapsed else Invalid_Window);
            if User_Limit then Outcome := G.Timed_Out; end if; return;
         elsif Attempt = Maximum_Wait_Slices then State.Info.Last_Reason := Work_Exhausted; return; end if;
         Remaining := Limit - Now;
         Target := (if Slice_Microseconds >= Remaining then Limit else Now + Slice_Microseconds);
         Wait_Until (Target, Good);
         if not Good then State.Info.Last_Reason := Wait_Unavailable; return; end if;
      end loop;
   end Acquire;
   procedure Operate
     (State : in out Context; Domain : AML_Frame_Handles.Invocation_Domain;
      Action : G.Operation; Timeout : AML_Mutexes.Timeout; Outcome : in out G.Outcome) is
      Good : Boolean;
   begin
      Outcome := G.Refused;
      if State.Info.Working then return; end if;
      if not AML_Frame_Handles.Has_Authority (Domain) then
         State.Info.Last_Reason := Invalid_Domain; State.Info.Last_Result := Outcome; return;
      end if;
      State.Info.Working := True;
      if Action = G.Acquire_Lease then Acquire (State, Domain, Timeout, Outcome);
      elsif not State.Info.Cleanup_Required or else Domain /= State.Owner then
         State.Info.Last_Reason := Invalid_Domain;
      elsif Action = G.Check_Lease then
         Scope.Check_AML (Transaction, Domain, State.Token, Good);
         Outcome := (if Good then G.Succeeded else G.Refused);
         State.Info.Last_Reason := (if Good then None else Physical_Refused);
      else
         if Scope.AML_Borrowed (Transaction) then
            Scope.End_AML (Transaction, Domain, State.Token, Good);
         else Scope.Cleanup (Transaction, Good); end if;
         if not Scope.Pending (Transaction) and then not Scope.AML_Borrowed (Transaction) then
            State.Owner := AML_Frame_Handles.No_Domain; State.Token := Scope.No_AML_Borrow;
            State.Info.Cleanup_Required := False;
            Outcome := (if Good or else Action = G.Drain_Lease then G.Succeeded else G.Refused);
         else Outcome := G.Indeterminate; end if;
         State.Info.Last_Reason := (if Outcome = G.Succeeded then None
           elsif Outcome = G.Indeterminate then Physical_Indeterminate else Physical_Refused);
      end if;
      State.Info.Last_Result := Outcome; State.Info.Working := False;
   end Operate;
   procedure Maintain (State : in out Context; Outcome : out G.Outcome) is
      Owner : constant AML_Frame_Handles.Invocation_Domain := State.Owner;
   begin
      Outcome := G.Refused;
      if State.Info.Working then return;
      elsif not State.Info.Cleanup_Required then Outcome := G.Succeeded; return; end if;
      Operate (State, Owner, G.Drain_Lease, AML_Mutexes.Poll_Only, Outcome);
   end Maintain;
end ACPI_AML_Global_Lock;
