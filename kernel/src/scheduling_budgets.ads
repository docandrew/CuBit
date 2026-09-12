-- CPU-time ledger for a future admitted, expedited scheduling lane.
-- Pure policy only: no process IDs, authority lookup, clock reads or dispatch.
package Scheduling_Budgets with Pure, SPARK_Mode is
   type Time_Units is range 0 .. 2 ** 60;
   -- Units are chosen once per ledger: microseconds in hosted scenarios,
   -- exact TSC ticks in native observation. Never round individual handoffs.
   Period : constant Time_Units := 2_000;
   subtype Period_Length is Time_Units range 2 .. Time_Units'Last;
   subtype Allowance is Time_Units range 1 .. Time_Units'Last / 2;
   type Dispatch_Count is range 0 .. 32;
   subtype Dispatch_Allowance is Dispatch_Count range 1 .. Dispatch_Count'Last;
   type Update_Result is (Updated, Clock_Reversed);
   type State is private;

   function Window (S : State) return Period_Length;

   function Limit (S : State) return Allowance;
   function Remaining (S : State) return Time_Units;
   function Last_Update (S : State) return Time_Units;
   function Running (S : State) return Boolean;
   function Overrun (S : State) return Boolean;
   function Eligible (S : State) return Boolean;
   function Dispatch_Limit (S : State) return Dispatch_Allowance;
   function Dispatches_Left (S : State) return Dispatch_Count;
   function Start_Allowed (S : State) return Boolean;

   -- Privileged lifecycle operation, not a wakeup operation. Native admission
   -- must not let an app recreate its budget. A shared per-CPU ledger bounds
   -- aggregate expedited time even when individual reservations are replaced.
   function Create
     (Budget : Allowance; Now : Time_Units;
      Dispatches : Dispatch_Allowance := 8;
      Interval : Period_Length := Period) return State
     with Pre => Budget <= Interval / 2,
       Post => Window (Create'Result) = Interval and then
       Limit (Create'Result) = Budget and then
       Remaining (Create'Result) = Budget and then
       Last_Update (Create'Result) = Now and then
       not Running (Create'Result) and then not Overrun (Create'Result) and then
       Dispatch_Limit (Create'Result) = Dispatches and then
       Dispatches_Left (Create'Result) = Dispatches;

   -- Separate from elapsed execution: a zero-duration wake/IPC storm must not
   -- cause unlimited expedited context switches. Claim once per expedited
   -- dispatch, from BOTH CPU and reservation ledgers, under the adapter lock.
   -- Continuing the same execution does not consume another dispatch credit.
   procedure Claim_Dispatch (S : in out State; Accepted : out Boolean)
     with Post =>
       Window (S) = Window (S'Old) and then
       Limit (S) = Limit (S'Old) and then
       Remaining (S) = Remaining (S'Old) and then
       Last_Update (S) = Last_Update (S'Old) and then
       Running (S) = Running (S'Old) and then Overrun (S) = Overrun (S'Old) and then
       Dispatch_Limit (S) = Dispatch_Limit (S'Old) and then
       Accepted = Start_Allowed (S'Old) and then
       Dispatches_Left (S) =
         (if Accepted then Dispatches_Left (S'Old) - 1
          else Dispatches_Left (S'Old));

   -- Charge the PREVIOUS expedited-execution state through Now, then change it.
   -- Sleeping, waking, dispatch and handoff never replenish a ledger.
   -- Only crossing a CPU-clock-aligned period boundary does. Call at EVERY
   -- execution transition, including direct IPC, and before selecting work.
   -- An overrun is sticky and disables this ledger's expedited eligibility;
   -- it does not kill a process or deny its ordinary scheduling eligibility.
   procedure Account
     (S : in out State; Now : Time_Units; Execute : Boolean;
     Result : out Update_Result)
     with Post =>
       Window (S) = Window (S'Old) and then
       Limit (S) = Limit (S'Old) and then
       Dispatch_Limit (S) = Dispatch_Limit (S'Old) and then
       (if Now < Last_Update (S'Old) then
          Result = Clock_Reversed and S = S'Old
        else
          Result = Updated and then Last_Update (S) = Now and then
          Running (S) = Execute and then
          Dispatches_Left (S) =
            (if Now / Window (S) = Last_Update (S'Old) / Window (S)
             then Dispatches_Left (S'Old) else Dispatch_Limit (S)) and then
          (if Overrun (S'Old) then Overrun (S)) and then
          (if Now / Window (S) = Last_Update (S'Old) / Window (S) then
             Remaining (S) =
               (if Running (S'Old) then
                  (if Now - Last_Update (S'Old) >= Remaining (S'Old)
                   then 0 else Remaining (S'Old) - (Now - Last_Update (S'Old)))
                else Remaining (S'Old)) and then
             Overrun (S) =
               (Overrun (S'Old) or
                (Running (S'Old) and Now - Last_Update (S'Old) > Remaining (S'Old)))
           else
             Remaining (S) =
               (if not Running (S'Old) then Limit (S)
                elsif Now mod Window (S) >= Limit (S) then 0
                else Limit (S) - Now mod Window (S)) and then
             Overrun (S) =
               (Overrun (S'Old) or
                (Running (S'Old) and
                 (Window (S) - Last_Update (S'Old) mod Window (S) > Remaining (S'Old)
                  or Now / Window (S) - Last_Update (S'Old) / Window (S) > 1
                  or Now mod Window (S) > Limit (S))))));

   -- Proof example: splitting execution at a same-period handoff cannot mint
   -- time or hide an overrun. No runtime code for this procedure.
   procedure Prove_Split_Charge
     (Original : State; Middle, Finish : Time_Units)
     with Ghost,
       Pre => Running (Original) and then
         Last_Update (Original) <= Middle and then Middle <= Finish and then
         Last_Update (Original) / Window (Original) = Finish / Window (Original);

private
   type State is record
      Interval : Period_Length := Period;
      Ceiling : Allowance := Allowance'First;
      Left : Time_Units := Allowance'First;
      Timestamp : Time_Units := 0;
      Executing : Boolean := False;
      Missed_Stop : Boolean := False;
      Dispatch_Ceiling : Dispatch_Allowance := 8;
      Dispatch_Remaining : Dispatch_Count := 8;
   end record
     with Type_Invariant => State.Ceiling <= State.Interval / 2 and then
       State.Left <= State.Ceiling and then
       State.Dispatch_Remaining <= State.Dispatch_Ceiling;

   function Limit (S : State) return Allowance is (S.Ceiling);
   function Window (S : State) return Period_Length is (S.Interval);
   function Remaining (S : State) return Time_Units is (S.Left);
   function Last_Update (S : State) return Time_Units is (S.Timestamp);
   function Running (S : State) return Boolean is (S.Executing);
   function Overrun (S : State) return Boolean is (S.Missed_Stop);
   function Eligible (S : State) return Boolean is
     (not S.Missed_Stop and S.Left > 0);
   function Dispatch_Limit (S : State) return Dispatch_Allowance is (S.Dispatch_Ceiling);
   function Dispatches_Left (S : State) return Dispatch_Count is (S.Dispatch_Remaining);
   function Start_Allowed (S : State) return Boolean is
     (Eligible (S) and S.Dispatch_Remaining > 0);
end Scheduling_Budgets;
