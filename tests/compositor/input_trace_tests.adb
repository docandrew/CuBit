with Ada.Text_IO;
with Compositor_Input_Trace; use Compositor_Input_Trace;
procedure Input_Trace_Tests is
   use type Tick;
   S : State;
begin
   pragma Assert (Increment (Natural'Last) = Natural'Last);
   pragma Assert (Increment (Natural'Last - 1) = Natural'Last);
   for Cycle in 1 .. 1000 loop
      for I in 1 .. 80 loop
         Add (S, (Tick (Cycle), Tick (I), Tick (I mod 10 + 1), Tick (I)));
      end loop;
      pragma Assert (Count (S) = 64 and Lost (S) = 16 and Invalid (S) = 0);
      for I in Record_Index loop
         pragma Assert (Item (S, I) =
           (Tick (Cycle), Tick (I), Tick (I mod 10 + 1), Tick (I)));
      end loop;
      Add (S, (0, 1, 1, 0));
      Add (S, (1, 0, 1, 0));
      Add (S, (1, 1, 0, 0));
      Add (S, (1, 1, 11, 0));
      Add (S, (1, 1, 1, Tick'Last));
      pragma Assert (Count (S) = 64 and Lost (S) = 16 and Invalid (S) = 5);
      Reset (S);
      pragma Assert (Count (S) = 0 and Lost (S) = 0 and Invalid (S) = 0);
      Add (S, (1, Tick'Last - 1, 9, 0));
      pragma Assert (Count (S) = 1 and Item (S, 1).Dequeued = 0);
      Reset (S);
   end loop;
   Ada.Text_IO.Put_Line ("INPUT-TRACE: PASS 1000 bounded batches, exact records, invalid identities/kinds/clocks, reset and saturation");
end Input_Trace_Tests;
