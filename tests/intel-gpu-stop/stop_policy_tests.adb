--  intel-gpu's Stop policy (PWR-003), Linux-hosted: the decisions its main
--  loop takes each turn after Service_Kernel_Events, over a scripted clock
--  and queue state. The loop itself needs an Intel GPU; this checks what it
--  decides, and gnatprove (run.sh --prove) proves the contracts.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Stop_Policy; use Intel_GPU_Stop_Policy;

procedure Stop_Policy_Tests is
   Checks, Failed : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   S : State;
begin
   --  Recognition: only the kernel's control label, length 2, kind Stop.
   Check (Is_Stop (16#010E#, 2, 1), "the kernel's Stop is a Stop");
   Check (not Is_Stop (16#010E#, 2, 2), "Interrupt is not a Stop");
   Check (not Is_Stop (16#010E#, 2, 3), "Reload is not a Stop");
   Check (not Is_Stop (16#010E#, 3, 1), "a malformed length is not a Stop");
   Check (not Is_Stop (16#010C#, 3, 1), "a grant revoke is not a Stop");
   Check (not Is_Stop (0, 0, 1), "a doorbell-shaped entry is not a Stop");

   --  No Stop: the loop runs, busy or idle, at any time.
   Check (Decide (S, True, 0) = Run, "idle, no Stop: run");
   Check (Decide (S, False, Unsigned_64'Last) = Run, "busy, no Stop: run");

   --  A Stop with the GPU idle: end at once.
   Note_Stop (S, 1_000);
   Check (S.Requested and then S.Since = 1_000, "the Stop is noted with its time");
   Check (Decide (S, True, 1_000) = End_Now, "idle after a Stop: end at once");

   --  Busy: drain until the bound, then end anyway.
   Check (Decide (S, False, 1_000) = Drain, "busy at the Stop: drain");
   Check (Decide (S, False, 1_000 + Drain_Ms - 1) = Drain, "busy just inside the bound: drain");
   Check (Decide (S, False, 1_000 + Drain_Ms) = End_Now, "busy at the bound: end");
   Check (Decide (S, True, 1_050) = End_Now, "work finished during the drain: end");

   --  A second Stop does not move the bound.
   Note_Stop (S, 5_000);
   Check (S.Since = 1_000, "a second Stop keeps the first one's time");

   --  A clock earlier than the Stop's: end rather than wait on it.
   Check (Decide (S, False, 999) = End_Now, "a clock that went back: end");

   --  Every turn after a Stop ends within Drain_Ms whatever the queues do.
   for T in Unsigned_64 range 1_000 .. 1_000 + 2 * Drain_Ms loop
      Check ((if T - 1_000 >= Drain_Ms then Decide (S, False, T) = End_Now
              else Decide (S, False, T) = Drain),
             "drain bound at" & Unsigned_64'Image (T));
   end loop;

   Put_Line ("intel-gpu stop policy:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failed) & " failed");
   if Failed > 0 then
      raise Program_Error;
   end if;
end Stop_Policy_Tests;
