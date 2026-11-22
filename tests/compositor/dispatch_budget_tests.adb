with Ada.Text_IO;
with Compositor_Dispatch_Budget; use Compositor_Dispatch_Budget;
procedure Dispatch_Budget_Tests is
   use type Tick;
   S : Input_Batch;
   R : Request_Batch;
   C : Completion_Batch;
begin
   for Cycle in 1 .. 10_000 loop
      C := New_Completions (100);
      for I in 1 .. Completion_Limit loop
         pragma Assert (Can_Complete (C, 100));
         Charge_Completion (C);
      end loop;
      pragma Assert (not Can_Complete (C, 100));
      C := New_Completions (100); Charge_Completion (C);
      pragma Assert (Can_Complete (C, 599) and not Can_Complete (C, 600));
      pragma Assert (not Can_Complete (C, 99) and not Can_Complete (C, Unavailable));
      C := New_Completions (Unavailable);
      pragma Assert (Can_Complete (C, Unavailable)); Charge_Completion (C);
      pragma Assert (not Can_Complete (C, Unavailable));
      C := New_Completions (Tick'Last - 300); Charge_Completion (C);
      pragma Assert (Can_Complete (C, Tick'Last - 1) and not Can_Complete (C, 0));
      -- Even a frozen clock cannot allow an input flood to starve requests
      -- or paint. Reopening the second phase cannot reset the turn's count.
      S := New_Input;
      pragma Assert (Phases (S) = 0 and not Can_Input (S, 100));
      Begin_Input_Phase (S, 100);
      for I in 1 .. 64 loop
         pragma Assert (Can_Input (S, 100));
         Charge_Input (S);
      end loop;
      Begin_Input_Phase (S, 1_000);
      pragma Assert (Phases (S) = 2 and not Can_Input (S, 1_000) and Events (S) = 64);

      S := New_Input;
      Begin_Input_Phase (S, 100);
      Charge_Input (S);
      pragma Assert (Can_Input (S, 599) and not Can_Input (S, 600));
      pragma Assert (not Can_Input (S, 99) and not Can_Input (S, Unavailable));
      -- Fresh arrivals after request dispatch get a second opportunity even
      -- if the first input phase exhausted its elapsed-time allowance.
      Begin_Input_Phase (S, 2_000);
      pragma Assert (Events (S) = 1 and Can_Input (S, 2_000));
      Charge_Input (S);
      pragma Assert (Can_Input (S, 2_499) and not Can_Input (S, 2_500));

      S := New_Input;
      Begin_Input_Phase (S, Unavailable);
      pragma Assert (Can_Input (S, Unavailable));
      Charge_Input (S);
      pragma Assert (not Can_Input (S, Unavailable));
      Begin_Input_Phase (S, Tick'Last - 300);
      Charge_Input (S);
      pragma Assert (Can_Input (S, Tick'Last - 1));
      pragma Assert (not Can_Input (S, 0));

      for Pending in Boolean loop
         R := New_Requests (100);
         for I in 1 .. (if Pending then 32 else 96) loop
            pragma Assert (Can_Request (R, 100, Pending));
            Charge_Request (R);
         end loop;
         pragma Assert (not Can_Request (R, 100, Pending));
      end loop;
      R := New_Requests (100);
      Charge_Request (R);
      pragma Assert (Can_Request (R, 1_099, False));
      pragma Assert (not Can_Request (R, 1_100, False));
      pragma Assert (not Can_Request (R, 99, False));
      pragma Assert (not Can_Request (R, Unavailable, False));
      R := New_Requests (Unavailable);
      pragma Assert (Can_Request (R, Unavailable, True));
      Charge_Request (R);
      pragma Assert (not Can_Request (R, Unavailable, True));
      -- Work can become paintable during the request phase. Apply the lower
      -- cap immediately without resetting the count or delaying that frame.
      R := New_Requests (0);
      for I in 1 .. 40 loop Charge_Request (R); end loop;
      pragma Assert (Can_Request (R, 0, False) and not Can_Request (R, 0, True));
   end loop;
   Ada.Text_IO.Put_Line ("dispatch-budget: PASS 10000 flood, two-phase, time-boundary, clock-fault and new-frame cycles");
end Dispatch_Budget_Tests;
