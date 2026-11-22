with Ada.Text_IO;
with Interfaces; use Interfaces;
with Scheduling_Turns; use Scheduling_Turns;
procedure Main is
   CPU, Low, Medium : State;
begin
   for Initial in Unsigned_64 range 0 .. 100 loop
      for First in Unsigned_64 range 0 .. 100 loop
         for Second in Unsigned_64 range 0 .. 100 loop
            Prove_Split (Initial, First, Second);
         end loop;
      end loop;
   end loop;
   CPU := Fresh (1_500);
   Charge (CPU, 200);
   Move (CPU, Low); -- higher priority interrupted the ordinary turn
   pragma Assert (Remaining (CPU) = 0 and Remaining (Low) = 1_300);
   CPU := Fresh (1_500);
   Charge (CPU, 100);
   Move (CPU, Medium); -- nested higher-priority interruption
   CPU := Fresh (1_500);
   Charge (CPU, 400);
   CPU := Empty; -- high priority blocks, unused time is not banked
   Move (Medium, CPU);
   pragma Assert (Remaining (CPU) = 1_400 and Remaining (Medium) = 0);
   Charge (CPU, 1_400);
   Move (Low, CPU);
   pragma Assert (Remaining (CPU) = 1_300 and Remaining (Low) = 0);
   -- A direct IPC chain changes owners, not the turn; no Fresh here.
   for Handoff in 1 .. 13 loop
      Charge (CPU, 100);
      pragma Assert (Remaining (CPU) = Unsigned_64 (13 - Handoff) * 100);
   end loop;
   Charge (CPU, Unsigned_64'Last);
   pragma Assert (Remaining (CPU) = 0);
   CPU := Fresh (Unsigned_64'Last);
   Charge (CPU, Unsigned_64'Last - 1);
   pragma Assert (Remaining (CPU) = 1);
   Charge (CPU, 2);
   pragma Assert (Remaining (CPU) = 0);
   Ada.Text_IO.Put_Line ("PASS: 1,030,301 split charges, nested preemption, IPC no-refill, exhaustion");
end Main;
