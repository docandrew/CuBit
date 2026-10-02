with Ada.Text_IO;
with Compositor_Source_Trace; use Compositor_Source_Trace;
procedure Source_Trace_Tests is
   use type Tick;
   S : State;
begin
   pragma Assert (Increment (Natural'Last) = Natural'Last);
   pragma Assert (Increment (Natural'Last - 1) = Natural'Last);
   for Cycle in 1 .. 1000 loop
      for I in 1 .. 80 loop
         -- Unknown and full-width watermarks remain diagnostic payloads.
         Add (S, (1, Tick (Cycle), Tick (I),
           (if I mod 2 = 0 then 0 else Tick'Last), Tick (I)));
      end loop;
      pragma Assert (Count (S) = 64 and Lost (S) = 16 and Invalid (S) = 0);
      for I in Record_Index loop
         pragma Assert (Item (S, I) =
           (1, Tick (Cycle), Tick (I),
            (if I mod 2 = 0 then 0 else Tick'Last), Tick (I)));
      end loop;
      Add (S, (0, 1, 1, 0, 0));
      Add (S, (1, 0, 1, 0, 0));
      Add (S, (1, 1, 0, 0, 0));
      Add (S, (1, 1, 1, 0, Tick'Last));
      pragma Assert (Count (S) = 64 and Lost (S) = 16 and Invalid (S) = 4);
      Reset (S);
      pragma Assert (Count (S) = 0 and Lost (S) = 0 and Invalid (S) = 0);
      Add (S, (1, 1, 1, 0, 0));
      pragma Assert (Count (S) = 1 and Item (S,1).Accepted = 0);
      Reset (S);
   end loop;
   Ada.Text_IO.Put_Line ("SOURCE-TRACE: PASS 1000 bounded batches, unknown/full-width input, invalid records, reset and saturation");
end Source_Trace_Tests;
