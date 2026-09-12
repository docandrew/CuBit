-- CPU-time ledger for a future admitted, expedited scheduling lane.
-- Pure policy only: no process IDs, authority lookup, clock reads or dispatch.
package Scheduling_Budgets with Pure, SPARK_Mode is
   type Microseconds is range 0 .. 2 ** 60;
   Period : constant Microseconds := 2_000;
   -- Experimental common period; at most half reserved for expedited work.
   subtype Allowance is Microseconds range 1 .. Period / 2;
   type Dispatch_Count is range 0 .. 32;
   subtype Dispatch_Allowance is Dispatch_Count range 1 .. Dispatch_Count'Last;
   type Update_Result is (Updated, Clock_Reversed);
   type State is private;

   function Limit (S : State) return Allowance;
   function Remaining (S : State) return Microseconds;
   function Last_Update (S : State) return Microseconds;
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
     (Budget : Allowance; Now : Microseconds;
      Dispatches : Dispatch_Allowance := 8) return State
     with Post => Limit (Create'Result) = Budget and then
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
     (S : in out State; Now : Microseconds; Execute : Boolean;
      Result : out Update_Result)
     with Post =>
       Limit (S) = Limit (S'Old) and then
       Dispatch_Limit (S) = Dispatch_Limit (S'Old) and then
       (if Now < Last_Update (S'Old) then
          Result = Clock_Reversed and S = S'Old
        else
          Result = Updated and then Last_Update (S) = Now and then
          Running (S) = Execute and then
          Dispatches_Left (S) =
            (if Now / Period = Last_Update (S'Old) / Period
             then Dispatches_Left (S'Old) else Dispatch_Limit (S)) and then
          (if Overrun (S'Old) then Overrun (S)) and then
          (if Now / Period = Last_Update (S'Old) / Period then
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
                elsif Now mod Period >= Limit (S) then 0
                else Limit (S) - Now mod Period) and then
             Overrun (S) =
               (Overrun (S'Old) or
                (Running (S'Old) and
                 (Period - Last_Update (S'Old) mod Period > Remaining (S'Old)
                  or Now / Period - Last_Update (S'Old) / Period > 1
                  or Now mod Period > Limit (S))))));

   -- Proof example: splitting execution at a same-period handoff cannot mint
   -- time or hide an overrun. No runtime code for this procedure.
   procedure Prove_Split_Charge
     (Original : State; Middle, Finish : Microseconds)
     with Ghost,
       Pre => Running (Original) and then
         Last_Update (Original) <= Middle and then Middle <= Finish and then
         Last_Update (Original) / Period = Finish / Period;

private
   type State is record
      Ceiling : Allowance := Allowance'First;
      Left : Microseconds := Allowance'First;
      Timestamp : Microseconds := 0;
      Executing : Boolean := False;
      Missed_Stop : Boolean := False;
      Dispatch_Ceiling : Dispatch_Allowance := 8;
      Dispatch_Remaining : Dispatch_Count := 8;
   end record
     with Type_Invariant => State.Left <= State.Ceiling and then
       State.Dispatch_Remaining <= State.Dispatch_Ceiling;

   function Limit (S : State) return Allowance is (S.Ceiling);
   function Remaining (S : State) return Microseconds is (S.Left);
   function Last_Update (S : State) return Microseconds is (S.Timestamp);
   function Running (S : State) return Boolean is (S.Executing);
   function Overrun (S : State) return Boolean is (S.Missed_Stop);
   function Eligible (S : State) return Boolean is
     (not S.Missed_Stop and S.Left > 0);
   function Dispatch_Limit (S : State) return Dispatch_Allowance is (S.Dispatch_Ceiling);
   function Dispatches_Left (S : State) return Dispatch_Count is (S.Dispatch_Remaining);
   function Start_Allowed (S : State) return Boolean is
     (Eligible (S) and S.Dispatch_Remaining > 0);
end Scheduling_Budgets;
