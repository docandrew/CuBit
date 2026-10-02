--  Hosted tests of Virtual_Deadlines (docs/scheduler.md).
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Virtual_Deadlines; use Virtual_Deadlines;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   Slice  : constant Slice_Length := 3_000;
   Margin : constant Ticks := 100;
begin
   --  Refill and saturation.
   Check (Refill (1_000, Slice) = 4_000, "refill adds the slice");
   Check (Refill (Deadline'Last - 1, Slice) = Deadline'Last, "refill saturates");


   --  Minimum dispatch charge.
   Check (Dispatch_Charge (5, 40) = 40, "tiny run pays the minimum");
   Check (Dispatch_Charge (400, 40) = 400, "long run pays its time");

   --  Preemption margin, and idle.
   Check (Preempts (1_000, 1_200, Margin), "earlier by more than the margin");
   Check (not Preempts (1_150, 1_200, Margin), "near-equal does not preempt");
   Check (not Preempts (1_200, 1_000, Margin), "later does not preempt");
   Check (Preempts (Deadline'Last, Idle_Key, Margin), "anything preempts idle");
   Check (not Preempts (0, 50, Margin), "no underflow below the margin");

   declare
      All_CPUs : constant CPU_Flags (0 .. 3) := [others => True];
      Running  : CPU_Keys (0 .. 3) := [5_000, 6_000, Idle_Key, 7_000];
      Queued   : CPU_Keys (0 .. 3) := [Idle_Key, Idle_Key, Idle_Key, Idle_Key];
   begin
      --  Home preempted: stays home.
      Check (Place (0, 1_000, False, Running, Queued, All_CPUs, Margin) = 0,
             "wakee preempting home stays home");
      --  Would not preempt home: goes to the idle CPU.
      Check (Place (0, 5_050, False, Running, Queued, All_CPUs, Margin) = 2,
             "wakee that would wait goes to an idle CPU");
      --  Pinned stays home regardless.
      Check (Place (0, 5_050, True, Running, Queued, All_CPUs, Margin) = 0,
             "pinned stays home");
      --  An idle CPU outside the domain is not used.
      Check (Place (0, 5_050, False, Running, Queued,
                    [True, True, False, True], Margin) = 3,
             "no idle CPU allowed: preempts the latest key");
      --  Home preemptable but an earlier thread is queued there.
      Queued (0) := 900;
      Check (Place (0, 1_000, False, Running, Queued, All_CPUs, Margin) = 2,
             "earlier work queued at home: idle CPU instead");
      --  Nothing idle, nothing preemptable: home.
      Running (2) := 1_000;
      Check (Place (0, 8_000, False, Running, Queued, All_CPUs, Margin) = 0,
             "nothing better: queue at home");
      --  Nearest idle after home wraps around.
      Running := [Idle_Key, 6_000, 6_000, 6_000];
      Queued := [Idle_Key, Idle_Key, Idle_Key, Idle_Key];
      Check (Place (2, 9_000, False, Running, Queued, All_CPUs, Margin) = 0,
             "idle search wraps past the last CPU");
   end;

   declare
      Heads : constant CPU_Keys (0 .. 3) := [5_000, 4_950, 1_000, Idle_Key];
      Every : constant CPU_Flags (0 .. 3) := [others => True];
      No_Work : constant CPU_Keys (0 .. 3) := [others => Idle_Key];
   begin
      Check (Choose (0, Heads, Every, Margin) = 2,
             "take a much earlier remote head");
      Check (Choose (0, Heads, [True, True, False, True], Margin) = 0,
             "a remote head within the margin stays local");
      Check (Choose (3, Heads, Every, Margin) = 2,
             "an idle CPU takes the earliest head");
      Check (Choose (3, No_Work, Every, Margin) = 3,
             "nothing anywhere: own list");
   end;

   if Failures = 0 then
      Put_Line ("scheduler-deadlines: all tests passed");
   else
      Put_Line ("scheduler-deadlines:" & Failures'Image & " failures");
   end if;
end Main;
