with Interfaces; use Interfaces;

-- Ordinary execution credit, not a reservation or application authority.
package Scheduling_Turns with SPARK_Mode is
   type State is private;
   Empty : constant State;
   type Event_Kind is
     (Fresh_Dispatch, Resumed_Dispatch, Higher_Preemption,
      Quantum_Rotation, Wake_Rotation, Relinquishment, Timer_Opportunity);
   type Counters is array (Event_Kind) of Unsigned_64;
   procedure Count (Totals : in out Counters; Event : Event_Kind);
   function Remaining (S : State) return Unsigned_64;
   function Fresh (Ticks : Unsigned_64) return State
     with Post => Remaining (Fresh'Result) = Ticks;
   procedure Charge (S : in out State; Ticks : Unsigned_64)
     with Post => Remaining (S) =
       (if Ticks >= Remaining (S'Old) then 0
        else Remaining (S'Old) - Ticks);
   procedure Move (Source : in out State; Target : out State)
     with Post => Remaining (Source) = 0 and
       Remaining (Target) = Remaining (Source'Old);
   procedure Prove_Split (Initial, First, Second : Unsigned_64)
     with Ghost, Pre => First <= Unsigned_64'Last - Second;
private
   type State is record
      Credit : Unsigned_64 := 0;
   end record;
   Empty : constant State := (Credit => 0);
end Scheduling_Turns;
