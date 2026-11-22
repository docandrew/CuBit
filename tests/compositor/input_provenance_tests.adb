with Ada.Text_IO;
with Client_Input_Provenance; use Client_Input_Provenance;
procedure Input_Provenance_Tests is
   use type Serial;
   S : State;
   OK : Boolean;
   Stamp : Serial;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "provenance mismatch"; end if;
   end Check;
begin
   End_Paint (S, True, Stamp); Check (Stamp = 0);
   Begin_Paint (S, OK); Check (OK);
   End_Paint (S, True, Stamp); Check (Stamp = 0);
   Finish_Event (S, 1, OK); Check (not OK);
   Begin_Event (S, 0, OK); Check (not OK);
   for I in Serial range 1 .. 10_000 loop
      Begin_Event (S, I, OK); Check (OK);
      Begin_Event (S, I + 1, OK); Check (not OK);
      Finish_Event (S, I + 1, OK); Check (not OK);
      -- A paint nested inside the handler cannot claim its unfinished input.
      Begin_Paint (S, OK); Check (OK and Frozen (S) = I - 1);
      Begin_Paint (S, OK); Check (not OK);
      Finish_Event (S, I, OK); Check (OK and Handled (S) = I);
      Check (Frozen (S) = I - 1);
      Begin_Event (S, I + 1, OK); Check (not OK);
      End_Paint (S, True, Stamp); Check (Stamp = I - 1);
      Check (Last_Published (S) = I - 1);
      Begin_Paint (S, OK); Check (OK and Frozen (S) = I);
      End_Paint (S, False, Stamp); Check (Stamp = 0);
      Check (Last_Published (S) = I - 1 and Handled (S) = I);
      Begin_Paint (S, OK); Check (OK);
      End_Paint (S, True, Stamp); Check (Stamp = I);
      Begin_Event (S, I, OK); Check (not OK);
      Finish_Event (S, I, OK); Check (not OK);
   end loop;
   Begin_Event (S, Serial'Last, OK); Check (OK);
   Finish_Event (S, Serial'Last, OK); Check (OK);
   Begin_Event (S, 0, OK); Check (not OK);
   Begin_Event (S, 1, OK); Check (not OK);
   Begin_Paint (S, OK); Check (OK);
   End_Paint (S, True, Stamp); Check (Stamp = Serial'Last);
   Ada.Text_IO.Put_Line ("PASS provenance: 10000 interleaved paint/event/retry cycles, stale and exhausted serials");
end Input_Provenance_Tests;
