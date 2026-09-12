with Ada.Text_IO;
with Scheduler_Timing; use Scheduler_Timing;
procedure Main is
   Clocks : array (1 .. 6) of Tick_Phase := [others => <>];
   Counts, Quanta : array (1 .. 6) of Natural := [others => 0];
   Elapsed, Quantum : Boolean;
begin
   pragma Assert
     (Millisecond_Phase'Pos (Second_Half) + 1 = Ticks_Per_Millisecond);
   pragma Assert (Quantum_Phase'Pos (Last_Third) + 1 = Ticks_Per_Quantum);
   pragma Assert (Tick_Microseconds * Ticks_Per_Millisecond = 1_000);
   pragma Assert (Quantum_Microseconds = 1_500);
   -- Independent CPU clocks, including different starting phases. Dispatch,
   -- IPC and sleep are deliberately not inputs to this CPU-owned divider.
   for CPU in Clocks'Range loop
      Clocks (CPU) :=
        (Clock => Millisecond_Phase'Val ((CPU - 1) mod Ticks_Per_Millisecond),
         Scheduling => Quantum_Phase'Val ((CPU - 1) mod Ticks_Per_Quantum));
   end loop;
   for Tick in 1 .. 100_000 loop
      for CPU in Clocks'Range loop
         Advance (Clocks (CPU), Elapsed, Quantum);
         if Elapsed then Counts (CPU) := Counts (CPU) + 1; end if;
         if Quantum then Quanta (CPU) := Quanta (CPU) + 1; end if;
         pragma Assert (Counts (CPU) =
           (Tick + (CPU - 1) mod Ticks_Per_Millisecond) / Ticks_Per_Millisecond);
         pragma Assert (Quanta (CPU) =
           (Tick + (CPU - 1) mod Ticks_Per_Quantum) / Ticks_Per_Quantum);
         pragma Assert (Millisecond_Phase'Pos (Clocks (CPU).Clock) =
           (Tick + CPU - 1) mod Ticks_Per_Millisecond);
         pragma Assert (Quantum_Phase'Pos (Clocks (CPU).Scheduling) =
           (Tick + CPU - 1) mod Ticks_Per_Quantum);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SCHEDULER-TIMING: PASS 600000 ticks, all phase combinations, millisecond and quantum conservation");
end Main;
