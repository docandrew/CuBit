-- Interrupt opportunities are not elapsed time. Calibrate the reference TSC
-- before enabling the fast LAPIC timer, which a hypervisor may rate-limit.
package Scheduler_Timing with Pure, SPARK_Mode is
   Ticks_Per_Millisecond : constant := 4;
   Tick_Microseconds : constant := 1_000 / Ticks_Per_Millisecond;
   Quantum_Microseconds : constant := 1_500;
   Wakeup_Microseconds : constant := 100;
   OneShot_Ticks_Per_Millisecond : constant := 1_000 / Wakeup_Microseconds;
   -- A monotonic clock is ordered, not modular arithmetic. Native timestamps
   -- outside this supported epoch are rejected by the adapter.
   type Tick_Count is range 0 .. 2 ** 63 - 1;
   subtype Tick_Rate is Tick_Count range 1 .. Tick_Count'Last;
   type Clock_State is private;
   function Timestamp (S : Clock_State) return Tick_Count;
   function Rate (S : Clock_State) return Tick_Rate;
   function Start (Now : Tick_Count; Ticks_Per_Millisecond : Tick_Rate)
     return Clock_State with Post =>
       Timestamp (Start'Result) = Now and
       Rate (Start'Result) = Ticks_Per_Millisecond;
   procedure Advance (S : in out Clock_State; Now : Tick_Count;
                      Elapsed : out Tick_Count; Valid : out Boolean)
     with Post => Rate (S) = Rate (S'Old) and then
       (if Now < Timestamp (S'Old) then
          not Valid and S = S'Old and Elapsed = 0
        else Valid and then
          Elapsed = (Now - Timestamp (S'Old)) / Rate (S) and then
          Timestamp (S) = Now - (Now - Timestamp (S'Old)) mod Rate (S));
   procedure Prove_Split (Initial, Middle, Finish : Tick_Count; Ticks : Tick_Rate)
     with Ghost, Pre => Initial <= Middle and Middle <= Finish;
private
   type Clock_State is record
      Last : Tick_Count := 0;
      Period : Tick_Rate := 1;
   end record;
   function Timestamp (S : Clock_State) return Tick_Count is (S.Last);
   function Rate (S : Clock_State) return Tick_Rate is (S.Period);
end Scheduler_Timing;
