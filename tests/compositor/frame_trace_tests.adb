with Ada.Text_IO;
with Compositor_Frame_Trace; use Compositor_Frame_Trace;
procedure Frame_Trace_Tests is
   S : State;
begin
   for Cycle in 1 .. 100 loop
      for I in 1 .. 80 loop
         Add (S, (I mod 2, 1, Tick (I), Tick (I), Tick (I + 5)));
      end loop;
      pragma Assert (Count (S) = 64 and Lost (S) = 16);
      for I in Record_Index loop
         pragma Assert (Item (S, I) = (I mod 2, 1, Tick (I), Tick (I), Tick (I + 5)));
      end loop;
      Add (S, (0, 0, 1, 0, 1));
      Add (S, (0, 1, 0, 0, 1));
      Add (S, (0, 1, 1, 2, 1));
      Add (S, (0, 1, 1, 0, Tick'Last));
      pragma Assert (Count (S) = 64 and Lost (S) = 16 and Invalid (S) = 4);
      Reset (S);
      pragma Assert (Count (S) = 0 and Lost (S) = 0 and Invalid (S) = 0);
   end loop;
   Ada.Text_IO.Put_Line ("FRAME-TRACE: PASS 100 saturation/reset cycles and invalid clock/identity cases");
end Frame_Trace_Tests;
