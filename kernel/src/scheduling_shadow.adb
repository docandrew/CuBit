package body Scheduling_Shadow with SPARK_Mode is
   use type Budgets.Update_Result;

   procedure Add (Value : in out Unsigned_64; Amount : Unsigned_64;
                  Saturated : in out Boolean) is
   begin
      if Amount > Unsigned_64'Last - Value then
         Value := Unsigned_64'Last;
         Saturated := True;
      else
         Value := Value + Amount;
      end if;
   end Add;

   procedure Initialize (CPU : out CPU_State; Rate : Tick_Rate; Now : Budgets.Time_Units) is
   begin
      CPU := (Ledger => Budgets.Create
                (CPU_Allowance_Us * Rate, Now, 8, Period_Us * Rate),
              Rate => Rate, Started => True, others => <>);
   end Initialize;

   procedure Invalidate (CPU : in out CPU_State) is
   begin
      CPU.Condition := Invalid_Clock;
   end Invalidate;

   procedure Observe
     (CPU : in out CPU_State; R : in out Reservation;
      Now : Budgets.Time_Units; Event : Boundary)
   is
      Result : Budgets.Update_Result;
      Accepted : Boolean;
      Elapsed : Unsigned_64;
      Executing : constant Boolean := Event /= Dispatch;
   begin
      if CPU.Condition /= Healthy then return; end if;
      if not CPU.Started or else
         Budgets.Running (CPU.Ledger) /= Executing or else
         (R.Started and then Budgets.Running (R.Ledger) /= Executing) or else
         (not R.Started and Event /= Dispatch)
      then
         CPU.Condition := Execution_Error;
         return;
      end if;
      if not R.Started then
         R.Ledger := Budgets.Create
           (Process_Allowance_Us * CPU.Rate, Now, 4, Period_Us * CPU.Rate);
         R.Started := True;
      end if;
      if Now < Budgets.Last_Update (CPU.Ledger) or else
         Now < Budgets.Last_Update (R.Ledger)
      then
         CPU.Condition := Clock_Error;
         return;
      end if;
      if Executing then
         -- These timestamps must match while the reservation owns this CPU.
         -- Otherwise two different spans would be charged to the two ledgers.
         if Budgets.Last_Update (CPU.Ledger) /= Budgets.Last_Update (R.Ledger) then
            CPU.Condition := Execution_Error;
            return;
         end if;
         Elapsed := Unsigned_64 (Now - Budgets.Last_Update (R.Ledger));
         Add (R.Totals.Charged_Ticks, Elapsed, R.Totals.Saturated);
         Add (CPU.Totals.Charged_Ticks, Elapsed, CPU.Totals.Saturated);
      end if;
      Budgets.Account (CPU.Ledger, Now, Event /= Stop, Result);
      pragma Assert (Result = Budgets.Updated);
      Budgets.Account (R.Ledger, Now, Event /= Stop, Result);
      pragma Assert (Result = Budgets.Updated);
      case Event is
         when Dispatch =>
            Add (R.Totals.Dispatches, 1, R.Totals.Saturated);
            -- All-or-neither claims under the adapter's existing process lock.
            -- Denial is recorded but never prevents or changes real execution.
            if Budgets.Start_Allowed (CPU.Ledger) and then Budgets.Start_Allowed (R.Ledger) then
               Budgets.Claim_Dispatch (CPU.Ledger, Accepted);
               pragma Assert (Accepted);
               Budgets.Claim_Dispatch (R.Ledger, Accepted);
               pragma Assert (Accepted);
            else
               Add (R.Totals.Denied, 1, R.Totals.Saturated);
            end if;
         when Continue_Execution =>
            Add (R.Totals.Checkpoints, 1, R.Totals.Saturated);
         when Stop => null;
      end case;
   end Observe;
end Scheduling_Shadow;
