-- Hardware-independent clock and quantum dividers. The periodic scheduling
-- opportunity is CPU-owned: IPC handoffs, yields and sleeps cannot reset it.
package Scheduler_Timing with Pure, SPARK_Mode is
   Ticks_Per_Millisecond : constant := 2;
   Ticks_Per_Quantum : constant := 3;
   Tick_Microseconds : constant := 1_000 / Ticks_Per_Millisecond;
   Quantum_Microseconds : constant := Tick_Microseconds * Ticks_Per_Quantum;
   type Millisecond_Phase is (First_Half, Second_Half);
   type Quantum_Phase is (First_Third, Second_Third, Last_Third);
   type Tick_Phase is record
      Clock : Millisecond_Phase := First_Half;
      Scheduling : Quantum_Phase := First_Third;
   end record;
   procedure Advance
     (Phase : in out Tick_Phase; Millisecond, Quantum : out Boolean)
     with Post =>
       Millisecond = (Phase'Old.Clock = Second_Half) and then
       Quantum = (Phase'Old.Scheduling = Last_Third) and then
       Phase.Clock =
         (if Phase'Old.Clock = Second_Half then First_Half
          else Millisecond_Phase'Succ (Phase'Old.Clock)) and then
       Phase.Scheduling =
         (if Phase'Old.Scheduling = Last_Third then First_Third
          else Quantum_Phase'Succ (Phase'Old.Scheduling));
end Scheduler_Timing;
