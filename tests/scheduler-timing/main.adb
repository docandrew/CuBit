with Ada.Text_IO;
with Scheduler_Timing; use Scheduler_Timing;
procedure Main is
   Clocks : array (1 .. 10) of Clock_State;
   Counts : array (Clocks'Range) of Tick_Count := [others => 0];
   Elapsed : Tick_Count;
   Valid : Boolean;
   S, Before : Clock_State;
begin
   pragma Assert (Quantum_Microseconds = 1_500);
   for CPU in Clocks'Range loop
      Clocks (CPU) := Start (Tick_Count (CPU - 1), 1000);
   end loop;
   -- Deliberately irregular interrupts, including coalesced/missed ticks.
   for Tick in Tick_Count range 1 .. 100_000 loop
      for CPU in Clocks'Range loop
         Advance (Clocks (CPU), Tick * 7919, Elapsed, Valid);
         Counts (CPU) := Counts (CPU) + Elapsed;
         pragma Assert (Valid);
         pragma Assert (Counts (CPU) = (Tick * 7919 - Tick_Count (CPU - 1)) / 1000);
      end loop;
   end loop;
   for Middle in Tick_Count range 0 .. 100 loop
      for Finish in Middle .. 100 loop
         Prove_Split (0, Middle, Finish, 17);
      end loop;
   end loop;
   S := Start (10, 1000);
   Before := S;
   Advance (S, 9, Elapsed, Valid);
   pragma Assert (not Valid and Elapsed = 0 and S = Before);
   Advance (S, 1_000_000_010, Elapsed, Valid);
   pragma Assert (Valid and Elapsed = 1_000_000);
   S := Start (0, 1);
   Advance (S, Tick_Count'Last, Elapsed, Valid);
   pragma Assert (Valid and Elapsed = Tick_Count'Last);
   Ada.Text_IO.Put_Line ("SCHEDULER-TIMING: PASS 1000000 irregular samples, split conservation, reversal and long pauses");
end Main;
