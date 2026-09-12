with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Scheduling_Budgets; use Scheduling_Budgets;

procedure Main is
   Result : Update_Result;
   S : State := Create (200, 0);

   procedure Check_Reference is
      -- Deliberately slow, one-microsecond oracle, independent of the
      -- production constant-time quotient/remainder accounting algorithm.
      Seed : Unsigned_32 := 16#C0B17#;
      Ref_Left, Ref_Time : Microseconds := 0;
      Ref_Running, Ref_Overrun : Boolean := False;
      Budget : Allowance;
      Now : Microseconds;
      Execute : Boolean;
   begin
      for Trial in 1 .. 100 loop
         Budget := Allowance (Trial * 7);
         Ref_Time := Microseconds (Trial * 19);
         Ref_Left := Budget;
         Ref_Running := False;
         Ref_Overrun := False;
         S := Create (Budget, Ref_Time);
         for Event in 1 .. 100 loop
            Seed := Seed * 1_664_525 + 1_013_904_223;
            Now := Ref_Time + Microseconds (Seed mod 6_001);
            Execute := (Seed and 16#8000#) /= 0;
            while Ref_Time < Now loop
               if Ref_Running then
                  if Ref_Left = 0 then
                     Ref_Overrun := True;
                  else
                     Ref_Left := Ref_Left - 1;
                  end if;
               end if;
               Ref_Time := Ref_Time + 1;
               if Ref_Time mod Period = 0 then
                  Ref_Left := Budget;
               end if;
            end loop;
            Ref_Running := Execute;
            Account (S, Now, Execute, Result);
            pragma Assert (Result = Updated and Remaining (S) = Ref_Left
              and Last_Update (S) = Ref_Time and Running (S) = Ref_Running
              and Overrun (S) = Ref_Overrun);
         end loop;
      end loop;
      Put_Line ("SCHEDULER-BUDGETS: PASS 10000 deterministic events against microsecond reference");
   end Check_Reference;

   procedure Check_Shared_Ceiling is
      CPU : State := Create (1_000, 0);
      Client : State;
      Now : Microseconds := 0;
      Served : Natural := 0;
   begin
      -- Even a privileged caller replacing a client repeatedly must retain
      -- the same CPU ledger. Fresh client state cannot reset the CPU ceiling.
      for Replacement in 1 .. 100 loop
         Client := Create (200, Now);
         if Eligible (CPU) and Eligible (Client) then
            Account (CPU, Now, True, Result);
            Account (Client, Now, True, Result);
            Now := Now + 100;
            Account (Client, Now, False, Result);
            Account (CPU, Now, False, Result);
            Served := Served + 1;
         end if;
      end loop;
      pragma Assert (Served = 10 and Remaining (CPU) = 0
                     and not Overrun (CPU) and not Eligible (CPU));
      Put_Line ("SCHEDULER-BUDGETS: PASS shared CPU ceiling survives reservation replacement");
   end Check_Shared_Ceiling;

   procedure Check_Dispatch_Storm is
      CPU : State := Create (1_000, 0, 8);
      Accepted : Boolean;
      Count : Natural := 0;
   begin
      for Wakeup in 1 .. 10_000 loop
         Account (CPU, 0, False, Result);
         Claim_Dispatch (CPU, Accepted);
         if Accepted then Count := Count + 1; end if;
      end loop;
      pragma Assert (Count = 8 and Dispatches_Left (CPU) = 0
        and Eligible (CPU) and not Start_Allowed (CPU));
      Account (CPU, Period - 1, False, Result);
      pragma Assert (Dispatches_Left (CPU) = 0);
      Account (CPU, Period, False, Result);
      pragma Assert (Dispatches_Left (CPU) = 8 and Start_Allowed (CPU));
      Put_Line ("SCHEDULER-BUDGETS: PASS 10000 zero-time wakeups admit only 8 expedited dispatches");
   end Check_Dispatch_Storm;

   procedure Simulate (Bound_Dispatches : Boolean) is
      -- Toy dispatcher only. Not the native queue implementation or an
      -- interrupt/IPC/physical latency simulation. Each step is ideal 1 us.
      type Client_ID is (Input, Audio, Spammer);
      type Work_ID is (Input_Work, Audio_Work, Spam_Work, Compute_Work);
      Budgets : array (Client_ID) of State :=
        [Input => Create (100, 0, 4), Audio => Create (200, 0, 2),
         Spammer => Create (600, 0, 2)];
      CPU : State := Create (1_000, 0, 8);
      Pending : array (Client_ID) of Natural := [others => 0];
      Spent : array (Client_ID) of Microseconds := [others => 0];
      Input_Start, Max_Input_Completion : Microseconds := 0;
      Normal_Time, Expedited_Time : Microseconds := 0;
      Input_Completions, Audio_Completions, Switches : Natural := 0;
      Selected, Previous : Work_ID := Compute_Work;
      Accepted : Boolean;
      Dispatches_In_Period : Dispatch_Count := 0;
   begin
      pragma Assert (Limit (Budgets (Input)) + Limit (Budgets (Audio)) +
                     Limit (Budgets (Spammer)) <= Limit (CPU));
      for Now in Microseconds range 0 .. 199_999 loop
         if Now mod 500 = 100 then
            pragma Assert (Pending (Input) = 0);
            Pending (Input) := 20;
            Input_Start := Now;
         end if;
         if Now mod Period = 0 then
            pragma Assert (Pending (Audio) = 0);
            Pending (Audio) := 100;
         end if;
         -- Alternates runnable/blocked while retaining the same ledger.
         Pending (Spammer) := (if Now mod 2 = 0 then 1 else 0);
         Selected := Compute_Work;
         if Eligible (CPU) then
            for Client in Client_ID loop
               if Pending (Client) > 0 and then Eligible (Budgets (Client)) and then
                 (not Bound_Dispatches or else
                  Previous = Work_ID'Val (Client_ID'Pos (Client)) or else
                  (Start_Allowed (CPU) and Start_Allowed (Budgets (Client))))
               then
                  Selected := Work_ID'Val (Client_ID'Pos (Client));
                  exit;
               end if;
            end loop;
         end if;
         if Selected /= Previous then Switches := Switches + 1; end if;
         if Bound_Dispatches and Selected /= Previous and Selected /= Compute_Work then
            Claim_Dispatch (CPU, Accepted);
            pragma Assert (Accepted);
            Claim_Dispatch
              (Budgets (Client_ID'Val (Work_ID'Pos (Selected))), Accepted);
            pragma Assert (Accepted);
            Dispatches_In_Period := Dispatches_In_Period + 1;
         end if;
         Previous := Selected;
         Account (CPU, Now, Selected /= Compute_Work, Result);
         for Client in Client_ID loop
            Account (Budgets (Client), Now,
                     Selected = Work_ID'Val (Client_ID'Pos (Client)), Result);
         end loop;
         if Selected = Compute_Work then
            Normal_Time := Normal_Time + 1;
         else
            declare
               Client : constant Client_ID := Client_ID'Val (Work_ID'Pos (Selected));
            begin
               Spent (Client) := Spent (Client) + 1;
               Expedited_Time := Expedited_Time + 1;
               Pending (Client) := Pending (Client) - 1;
               if Pending (Client) = 0 then
                  case Client is
                     when Input =>
                        Input_Completions := Input_Completions + 1;
                        Max_Input_Completion := Microseconds'Max
                          (Max_Input_Completion, Now + 1 - Input_Start);
                     when Audio => Audio_Completions := Audio_Completions + 1;
                     when Spammer => null;
                  end case;
               end if;
            end;
         end if;
         Account (CPU, Now + 1, False, Result);
         pragma Assert (not Overrun (CPU));
         for Client in Client_ID loop
            Account (Budgets (Client), Now + 1, False, Result);
            pragma Assert (not Overrun (Budgets (Client)));
         end loop;
         if (Now + 1) mod Period = 0 then
            pragma Assert (Expedited_Time <= 1_000 and Normal_Time >= 1_000);
            pragma Assert (Dispatches_In_Period <= Dispatch_Limit (CPU));
            for Client in Client_ID loop
               pragma Assert (Spent (Client) <= Limit (Budgets (Client)));
            end loop;
            Spent := [others => 0];
            Expedited_Time := 0;
            Normal_Time := 0;
            Dispatches_In_Period := 0;
         end if;
      end loop;
      pragma Assert (Input_Completions = 400 and Audio_Completions = 100);
      pragma Assert (Max_Input_Completion = 20);
      if Bound_Dispatches then
         pragma Assert (Switches <= 1_600);
      else
         -- Demonstrate why CPU-time limits alone do not bound switch costs.
         pragma Assert (Switches > 100_000);
      end if;
      Put_Line ("SCHEDULER-MODEL: PASS 100 periods, 400 input bursts, 100 audio bursts, compute >=50% each period");
      Put_Line ("SCHEDULER-MODEL: synthetic dispatch changes=" & Switches'Image &
                "; dispatch limits=" & Bound_Dispatches'Image &
                "; ideal input completion us=" & Max_Input_Completion'Image &
                " (NOT a CuBit latency measurement)");
   end Simulate;
begin
   -- Spending in short bursts, including zero-duration wake/sleep spam.
   Account (S, 0, True, Result);
   for T in Microseconds range 1 .. 200 loop
      Account (S, T, False, Result);
      pragma Assert (Result = Updated and not Overrun (S));
      for Spam in 1 .. 10 loop
         Account (S, T, True, Result);
         Account (S, T, False, Result);
      end loop;
      pragma Assert (Remaining (S) = 200 - T);
      Account (S, T, T < 200, Result);
   end loop;
   pragma Assert (not Eligible (S));
   Account (S, 1_999, False, Result);
   pragma Assert (Remaining (S) = 0);
   Account (S, 2_000, False, Result);
   pragma Assert (Remaining (S) = 200 and Eligible (S));
   Account (S, 2_000, False, Result);
   pragma Assert (Remaining (S) = 200);
   Account (S, 1_999, True, Result);
   pragma Assert (Result = Clock_Reversed and Last_Update (S) = 2_000
                  and not Running (S) and Remaining (S) = 200);

   -- A late stop must remain visible even after several period boundaries.
   S := Create (200, 1_900);
   Account (S, 1_900, True, Result);
   Account (S, 2_050, False, Result);
   pragma Assert (Remaining (S) = 150 and not Overrun (S));
   Account (S, 2_050, True, Result);
   Account (S, 6_000, False, Result);
   pragma Assert (Overrun (S) and not Eligible (S));
   Account (S, Microseconds'Last, False, Result);
   pragma Assert (Result = Updated and Overrun (S) and not Eligible (S));
   S := Create (200, Microseconds'Last - 10);
   Account (S, Microseconds'Last - 10, True, Result);
   Account (S, Microseconds'Last, False, Result);
   pragma Assert (Result = Updated and not Overrun (S));
   Put_Line ("SCHEDULER-BUDGETS: PASS exhaustion, wake spam, boundaries, late stop, clock reversal, clock limit");
   Check_Reference;
   Check_Shared_Ceiling;
   Check_Dispatch_Storm;
   Simulate (Bound_Dispatches => False);
   Simulate (Bound_Dispatches => True);
end Main;
