with Interfaces; use Interfaces;

-- Pure boundary accounting. The native adapter supplies ordered timestamps
-- and serializes transitions with the process-lifetime/dispatch lock.
generic
   type Owner_ID is range <>;
   Scheduler_Owner : Owner_ID;
package Execution_Accounting with Pure, SPARK_Mode is
   type Health is (Healthy, Clock_Reversed, Owner_Mismatch);
   type Clock_State is private;
   function Owner (S : Clock_State) return Owner_ID;
   function Timestamp (S : Clock_State) return Unsigned_64;
   function Initialized (S : Clock_State) return Boolean;
   function Status (S : Clock_State) return Health;

   type Charge is record
      Charged_Owner : Owner_ID := Scheduler_Owner;
      Ticks : Unsigned_64 := 0;
      Accepted : Boolean := False;
   end record;

   -- The old owner receives the entire interval. A checkpoint with identical
   -- owners splits an interval without rounding or changing dispatch state.
   -- Bad clocks/ownership disable accounting, NOT execution or scheduling.
   procedure Transition
     (S : in out Clock_State; Expected, Next_Owner : Owner_ID;
      Now : Unsigned_64; C : out Charge)
     with Post =>
       C.Charged_Owner = Owner (S'Old) and then
       (if Status (S'Old) /= Healthy then
          S = S'Old and not C.Accepted and C.Ticks = 0
        elsif Expected /= Owner (S'Old) then
          Status (S) = Owner_Mismatch and not C.Accepted and C.Ticks = 0
        elsif Initialized (S'Old) and Now < Timestamp (S'Old) then
          Status (S) = Clock_Reversed and not C.Accepted and C.Ticks = 0
        else
          C.Accepted and then Status (S) = Healthy and then Initialized (S)
          and then Owner (S) = Next_Owner and then Timestamp (S) = Now
          and then C.Ticks =
            (if Initialized (S'Old) then Now - Timestamp (S'Old) else 0));

   type Dispatch_Kind is (Scheduled, Direct_IPC);
   type Totals is record
      Residency_Ticks : Unsigned_64 := 0;
      Scheduled_Dispatches : Unsigned_64 := 0;
      Direct_Dispatches : Unsigned_64 := 0;
      Saturated : Boolean := False;
   end record;

   function Saturating_Add (Left, Right : Unsigned_64) return Unsigned_64 is
     (if Right > Unsigned_64'Last - Left then Unsigned_64'Last else Left + Right);

   procedure Add_Time (T : in out Totals; Ticks : Unsigned_64)
     with Post =>
       T.Residency_Ticks = Saturating_Add (T'Old.Residency_Ticks, Ticks) and then
       T.Saturated = (T'Old.Saturated or
         Ticks > Unsigned_64'Last - T'Old.Residency_Ticks) and then
       T.Scheduled_Dispatches = T'Old.Scheduled_Dispatches and then
       T.Direct_Dispatches = T'Old.Direct_Dispatches;

   procedure Dispatch (T : in out Totals; Kind : Dispatch_Kind)
     with Post =>
       T.Residency_Ticks = T'Old.Residency_Ticks and then
       T.Scheduled_Dispatches =
         (if Kind = Scheduled then Saturating_Add (T'Old.Scheduled_Dispatches, 1)
          else T'Old.Scheduled_Dispatches) and then
       T.Direct_Dispatches =
         (if Kind = Direct_IPC then Saturating_Add (T'Old.Direct_Dispatches, 1)
          else T'Old.Direct_Dispatches) and then
       T.Saturated = (T'Old.Saturated or
         (if Kind = Scheduled then T'Old.Scheduled_Dispatches = Unsigned_64'Last
          else T'Old.Direct_Dispatches = Unsigned_64'Last));

private
   type Clock_State is record
      Current : Owner_ID := Scheduler_Owner;
      Last : Unsigned_64 := 0;
      Started : Boolean := False;
      Condition : Health := Healthy;
   end record;
   function Owner (S : Clock_State) return Owner_ID is (S.Current);
   function Timestamp (S : Clock_State) return Unsigned_64 is (S.Last);
   function Initialized (S : Clock_State) return Boolean is (S.Started);
   function Status (S : Clock_State) return Health is (S.Condition);
end Execution_Accounting;
