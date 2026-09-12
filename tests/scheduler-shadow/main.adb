with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Scheduling_Shadow; use Scheduling_Shadow;
with Scheduling_Budgets;

procedure Main is
   use type Budgets.Time_Units;
   use type Budgets.Dispatch_Count;
   CPU, Other : CPU_State;
   A, B, C : Reservation;
   Now : Budgets.Time_Units := 0;
   S : Snapshot;
begin
   Initialize (CPU, 3_500, 0);
   Initialize (Other, 3_500, 0);
   -- Submicrosecond A -> B -> A direct handoffs: no per-handoff flooring.
   for I in 1 .. 10_000 loop
      Observe (CPU, A, Now, Dispatch);
      Now := Now + 3;
      Observe (CPU, A, Now, Stop);
      Observe (CPU, B, Now, Dispatch);
      Now := Now + 5;
      Observe (CPU, B, Now, Stop);
   end loop;
   pragma Assert (Status (CPU) = Healthy);
   pragma Assert (Inspect (A).Totals.Charged_Ticks = 30_000);
   pragma Assert (Inspect (B).Totals.Charged_Ticks = 50_000);
   pragma Assert (CPU_Ticks (CPU) = 80_000 and CPU_Ticks (Other) = 0);
   pragma Assert (Inspect (A).Totals.Dispatches = 10_000);
   pragma Assert (Inspect (A).Totals.Denied = 9_996);
   pragma Assert (Inspect (B).Totals.Denied = 9_996);
   -- A new PID lifetime cannot replenish the shared CPU dispatch budget.
   A := Empty_Reservation;
   Observe (CPU, A, Now, Dispatch);
   pragma Assert (Inspect (A).Totals.Dispatches = 1 and Inspect (A).Totals.Denied = 1);
   pragma Assert (Inspect (A).Credits = 4);
   Observe (CPU, A, Now + 1, Continue_Execution);
   Observe (CPU, A, Now + 2, Stop);
   pragma Assert (Inspect (A).Totals.Charged_Ticks = 2);
   pragma Assert (Inspect (A).Totals.Dispatches = 1 and Inspect (A).Totals.Checkpoints = 1);
   pragma Assert (CPU_Ticks (CPU) = 80_002);
   -- A fresh CPU is independent; multiple whole windows cannot hide overrun.
   Observe (Other, C, 0, Dispatch);
   Observe (Other, C, 21_000_001, Stop);
   pragma Assert (Status (Other) = Healthy and Inspect (C).Overrun);
   pragma Assert (CPU_Ticks (Other) = 21_000_001);
   -- Sleep/idle is not charged, and overrun remains sticky on later dispatch.
   Observe (Other, C, 35_000_000, Dispatch);
   pragma Assert (Inspect (C).Overrun and Inspect (C).Totals.Denied = 1);
   Observe (Other, C, 35_000_001, Stop);
   pragma Assert (CPU_Ticks (Other) = 21_000_002);
   S := Inspect (C);
   Observe (Other, C, 1, Dispatch);
   pragma Assert (Status (Other) = Clock_Error and Inspect (C) = S);
   Observe (Other, C, 40_000_000, Dispatch);
   pragma Assert (Status (Other) = Clock_Error and Inspect (C) = S);
   Initialize (Other, 1, 0);
   C := Empty_Reservation;
   Observe (Other, C, 0, Stop);
   pragma Assert (Status (Other) = Execution_Error);
   -- Exact exhaustion is distinct from overrun; each tick is accounted once.
   Initialize (Other, 1, 0);
   C := Empty_Reservation;
   Observe (Other, C, 0, Dispatch);
   Observe (Other, C, 199, Continue_Execution);
   Observe (Other, C, 200, Stop);
   pragma Assert (not Inspect (C).Overrun and Inspect (C).Remaining = 0);
   pragma Assert (CPU_Ticks (Other) = 200 and Inspect (C).Totals.Charged_Ticks = 200);
   -- Largest supported scaling/timestamp: subtraction stays exact at the edge.
   Initialize (Other, Tick_Rate'Last, Budgets.Time_Units'Last - 1);
   C := Empty_Reservation;
   Observe (Other, C, Budgets.Time_Units'Last - 1, Dispatch);
   Observe (Other, C, Budgets.Time_Units'Last, Stop);
   pragma Assert (Status (Other) = Healthy and CPU_Ticks (Other) = 1);
   Invalidate (Other);
   S := Inspect (C);
   Observe (Other, C, Budgets.Time_Units'Last, Dispatch);
   pragma Assert (Status (Other) = Invalid_Clock and Inspect (C) = S);
   Put_Line ("SHADOW-CHECK: PASS exact ticks, 20000 handoffs, checkpoints, idle, PID reuse, CPU isolation, sticky faults");
end Main;
