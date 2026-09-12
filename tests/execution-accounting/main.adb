with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Execution_Accounting;

procedure Main is
   type PID is range 0 .. 3;
   package A is new Execution_Accounting (PID, 0);
   use A;
   S, Other_CPU : Clock_State;
   Accounts : array (PID) of Totals;
   C : Charge;
   Now : Unsigned_64 := 100;
   Current : PID := 0;

   procedure Move (Target : PID; At_Tick : Unsigned_64) is
   begin
      Transition (S, Current, Target, At_Tick, C);
      pragma Assert (C.Accepted and C.Charged_Owner = Current);
      Add_Time (Accounts (Current), C.Ticks);
      if Target /= Current and Target /= 0 then
         Dispatch (Accounts (Target), (if Current = 0 then Scheduled else Direct_IPC));
      end if;
      Current := Target;
   end Move;
begin
   Move (1, Now);
   -- Repeated A->B->A direct handoffs without returning to scheduler.
   for I in 1 .. 100_000 loop
      Now := Now + 3; Move (2, Now);
      Now := Now + 5; Move (1, Now);
      -- An observation splits an interval, but creates no dispatch.
      Now := Now + 1; Move (1, Now);
   end loop;
   Move (0, Now);
   pragma Assert (Accounts (1).Residency_Ticks = 400_000);
   pragma Assert (Accounts (2).Residency_Ticks = 500_000);
   pragma Assert (Accounts (1).Scheduled_Dispatches = 1);
   pragma Assert (Accounts (1).Direct_Dispatches = 100_000);
   pragma Assert (Accounts (2).Direct_Dispatches = 100_000);
   pragma Assert (Accounts (0).Residency_Ticks = 0);
   Now := Now + 17; Move (3, Now);
   pragma Assert (Accounts (0).Residency_Ticks = 17);
   Now := Now + 11; Move (0, Now);
   pragma Assert (Accounts (3).Residency_Ticks = 11);
   -- Reap/reuse AFTER the final interval is charged. No inheritance of time.
   Accounts (3) := (others => <>);
   Move (3, Now);
   Now := Now + 7; Move (0, Now);
   pragma Assert (Accounts (3).Residency_Ticks = 7);
   pragma Assert (Accounts (3).Scheduled_Dispatches = 1);

   Transition (Other_CPU, 0, 2, 4, C);
   Transition (Other_CPU, 2, 0, 9, C);
   pragma Assert (C.Accepted and C.Ticks = 5 and Timestamp (S) = Now);
   Transition (Other_CPU, 0, 1, 8, C);
   pragma Assert (not C.Accepted and Status (Other_CPU) = Clock_Reversed);
   Transition (Other_CPU, 0, 1, 20, C);
   pragma Assert (not C.Accepted and C.Ticks = 0);
   Transition (S, 1, 2, Now, C);
   pragma Assert (not C.Accepted and Status (S) = Owner_Mismatch);
   declare
      T : Totals;
   begin
      Add_Time (T, Unsigned_64'Last);
      pragma Assert (not T.Saturated);
      Add_Time (T, 1);
      pragma Assert (T.Saturated and T.Residency_Ticks = Unsigned_64'Last);
      T := (Scheduled_Dispatches => Unsigned_64'Last, others => <>);
      Dispatch (T, Scheduled);
      pragma Assert (T.Saturated and T.Scheduled_Dispatches = Unsigned_64'Last);
      T := (Direct_Dispatches => Unsigned_64'Last, others => <>);
      Dispatch (T, Direct_IPC);
      pragma Assert (T.Saturated and T.Direct_Dispatches = Unsigned_64'Last);
   end;
   Put_Line ("EXECUTION-ACCOUNTING: PASS 100000 IPC chains, exact attribution, checkpoints, retirement/reuse, independent CPUs, fault and overflow reporting");
end Main;
