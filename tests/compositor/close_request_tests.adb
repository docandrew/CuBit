with Ada.Text_IO; use Ada.Text_IO;
with Compositor_Input_Queue;
with Compositor_Close_Request;
procedure Close_Request_Tests is
   package IQ renames Compositor_Input_Queue;
   package C renames Compositor_Close_Request;
   use type IQ.Word;
   use type IQ.Selection;
   Pending, Next : IQ.Word;
   Q : IQ.Queue;
   Result : IQ.Outcome;
   Choice : IQ.Selection;
   Saved : IQ.Word;
begin
   for Round in 1 .. 1_000 loop
      Pending := 0;
      Next := IQ.Word (Round) * 1_000;
      Q := [others => (others => <>)];
      IQ.Push (Q, Next, (True, 0, 1, 17, 1, 0),
               (True, 0, 9, 17, 0, 0), 3, Result);
      C.Request (Pending, Next);
      Saved := Pending;
      Choice := IQ.Oldest_After (Q, 0);
      pragma Assert (Choice /= -1);
      pragma Assert (not C.Select_Close (Pending, 0, Q (Choice).Serial));
      -- Repeated title-bar clicks do not allocate more serials or requests.
      for Click in 1 .. 1_000 loop C.Request (Pending, Next); end loop;
      pragma Assert (Pending = Saved and Next = Saved + 1);
      -- Force several real queue overflows; close is still ordered before
      -- the replacement resynchronization and later ordinary reports.
      for Input in 1 .. 1_000 loop
         IQ.Push (Q, Next, (True, 0, 1, 17, 1, 0),
                  (True, 0, 9, 17, 0, 0), 3, Result);
      end loop;
      Choice := IQ.Oldest_After (Q, 0);
      pragma Assert (Choice /= -1);
      pragma Assert (C.Select_Close (Pending, 0, Q (Choice).Serial));
      C.Acknowledge (Pending, Saved - 1);
      pragma Assert (Pending = Saved);
      C.Acknowledge (Pending, Saved);
      pragma Assert (Pending = 0);
      C.Request (Pending, Next);
      pragma Assert (Pending > Saved);
   end loop;
   for Exhausted in 1 .. 2 loop
      Pending := 0;
      Next := (if Exhausted = 1 then 0 else IQ.Word'Last);
      C.Request (Pending, Next);
      pragma Assert (Pending = 0);
   end loop;
   Pending := 0; Next := IQ.Word'Last - 1;
   C.Request (Pending, Next);
   pragma Assert (Pending = IQ.Word'Last - 1 and Next = IQ.Word'Last);
   C.Acknowledge (Pending, IQ.Word'Last);
   pragma Assert (Pending = 0);
   pragma Assert (not C.Select_Close (0, 0, 0));
   Put_Line ("PASS close latch: 1000 cycles, repeated clicks, real input overflow, acknowledgment and exhaustion");
end Close_Request_Tests;
