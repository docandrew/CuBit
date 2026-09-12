with Interfaces; use Interfaces;
with Scheduling_Budgets;

-- Observation of actual demand, NOT admission or a scheduling decision.
-- Every observed non-idle interval is charged even after budget exhaustion.
package Scheduling_Shadow with Pure, SPARK_Mode is
   package Budgets renames Scheduling_Budgets;
   use type Budgets.Time_Units;
   Period_Us : constant := 2_000;
   Process_Allowance_Us : constant := 200;
   CPU_Allowance_Us : constant := 1_000;
   subtype Tick_Rate is Budgets.Time_Units range 1 .. Budgets.Time_Units'Last / Period_Us;
   type Health is (Healthy, Clock_Error, Execution_Error, Invalid_Clock);
   type CPU_State is private;
   type Reservation is private;
   Empty_Reservation : constant Reservation;
   type Boundary is (Dispatch, Continue_Execution, Stop);
   type Counters is record
      Charged_Ticks : Unsigned_64 := 0;
      Dispatches : Unsigned_64 := 0;
      Denied : Unsigned_64 := 0;
      Checkpoints : Unsigned_64 := 0;
      Saturated : Boolean := False;
   end record;
   type Snapshot is record
      Totals : Counters;
      Remaining : Budgets.Time_Units;
      Credits : Budgets.Dispatch_Count;
      Overrun : Boolean;
   end record;
   function Initialized (CPU : CPU_State) return Boolean;
   function Status (CPU : CPU_State) return Health;
   function CPU_Ticks (CPU : CPU_State) return Unsigned_64;
   function Inspect (R : Reservation) return Snapshot;
   procedure Initialize (CPU : out CPU_State; Rate : Tick_Rate; Now : Budgets.Time_Units);
   procedure Invalidate (CPU : in out CPU_State);
   -- Caller serializes CPU and reservation together. Dispatch is paired with
   -- Stop before another reservation can run. Checkpoints never claim credit.
   -- Bad sequencing/clock state is sticky telemetry failure, never a trap.
   procedure Observe
     (CPU : in out CPU_State; R : in out Reservation;
      Now : Budgets.Time_Units; Event : Boundary);
private
   type CPU_State is record
      Ledger : Budgets.State;
      Rate : Tick_Rate := 1;
      Started : Boolean := False;
      Condition : Health := Healthy;
      Totals : Counters;
   end record;
   type Reservation is record
      Ledger : Budgets.State;
      Started : Boolean := False;
      Totals : Counters;
   end record;
   Empty_Reservation : constant Reservation := (others => <>);
   function Initialized (CPU : CPU_State) return Boolean is (CPU.Started);
   function Status (CPU : CPU_State) return Health is (CPU.Condition);
   function CPU_Ticks (CPU : CPU_State) return Unsigned_64 is (CPU.Totals.Charged_Ticks);
   function Inspect (R : Reservation) return Snapshot is
     ((Totals => R.Totals, Remaining => Budgets.Remaining (R.Ledger),
       Credits => Budgets.Dispatches_Left (R.Ledger), Overrun => Budgets.Overrun (R.Ledger)));
end Scheduling_Shadow;
