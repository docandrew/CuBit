with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Timeline;
-- Hosted coverage of the proved timeline rules (GPU-001 step 1): torn-read
-- detection, acceptance classes, and the deferred completion waiter.
procedure Timeline_Tests is
   package T renames Intel_GPU_Timeline;
   use type T.Value;
   use type T.Microseconds;
   use type T.Observation;
   use type T.Outcome;
   R : T.Read_Result;
   W : T.Waiter;
   Started : Boolean;
   Result : T.Outcome;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Check;
begin
   -- High, low, high: a changed high half is a torn read, never a value.
   R := T.Combine (0, 7, 0);
   Check (R.Stable and R.Observed = 7);
   R := T.Combine (1, 0, 1);
   Check (R.Stable and R.Observed = 2 ** 32);
   R := T.Combine (0, 16#FFFF_FFFF#, 1);
   Check (not R.Stable);
   R := T.Combine (T.Half'Last, T.Half'Last, T.Half'Last);
   Check (R.Stable and R.Observed = T.Value'Last);
   -- Acceptance around a published target.
   for Observed in T.Value range 0 .. 12 loop
      Check (T.Classify (5, 9, Observed) =
        (if Observed < 5 then T.Regressed elsif Observed = 5 then T.Unchanged
         elsif Observed < 9 then T.Advanced elsif Observed = 9 then T.Reached
         else T.Beyond_Published));
      Check (T.Accepted (T.Classify (5, 9, Observed)) = (Observed in 5 .. 9));
   end loop;
   Check (T.Classify (T.Value'Last - 1, T.Value'Last, T.Value'Last) = T.Reached);
   -- Start: explicit, positive budget; target is the successor; the
   -- deadline never reaches the clock's unavailable value.
   T.Start (W, 4, 5, 1_000, 0, Started); Check (not Started and not T.Waiting (W));
   T.Start (W, 4, 6, 1_000, 10, Started); Check (not Started);
   T.Start (W, T.Value'Last, 0, 1_000, 10, Started); Check (not Started);
   T.Start (W, 4, 5, T.Clock_Unavailable, 10, Started); Check (not Started);
   T.Start (W, 4, 5, T.Clock_Unavailable - 10, 10, Started); Check (not Started);
   T.Start (W, 4, 5, T.Clock_Unavailable - 11, 10, Started); Check (Started);
   T.Abandon (W); Check (not T.Waiting (W));
   T.Start (W, 4, 5, 1_000, 500, Started);
   Check (Started and T.Waiting (W) and T.Deadline (W) = 1_500 and T.Target (W) = 5);
   T.Start (W, 5, 6, 1_000, 500, Started); Check (not Started and T.Target (W) = 5);
   -- Pending while unchanged, then complete exactly at the target.
   T.Step (W, True, 4, True, 1_100, Result); Check (Result = T.Pending);
   T.Step (W, True, 4, True, 1_499, Result); Check (Result = T.Pending);
   T.Step (W, True, 5, True, 1_499, Result);
   Check (Result = T.Complete and not T.Waiting (W) and T.Completed (W) = 5);
   T.Step (W, True, 5, True, 1_600, Result); Check (Result = T.Not_Waiting);
   -- Closed gate: reached but held until the gate opens or time runs out.
   T.Start (W, 5, 6, 2_000, 100, Started); Check (Started);
   T.Step (W, True, 6, False, 2_050, Result); Check (Result = T.Pending);
   T.Step (W, True, 6, True, 2_060, Result); Check (Result = T.Complete);
   T.Start (W, 6, 7, 3_000, 100, Started);
   T.Step (W, True, 7, False, 3_100, Result);
   Check (Result = T.Deadline_Expired and T.Completed (W) = 6);
   -- A reached timeline is authoritative even when seen after the deadline.
   T.Start (W, 6, 7, 4_000, 100, Started);
   T.Step (W, True, 7, True, 9_999, Result); Check (Result = T.Complete);
   -- Faults: regression (the old transient zero), beyond published, read
   -- failure, clock backwards, clock unavailable. Each ends the wait.
   T.Start (W, 7, 8, 5_000, 100, Started);
   T.Step (W, True, 0, True, 5_001, Result);
   Check (Result = T.Timeline_Fault and not T.Waiting (W) and T.Completed (W) = 7);
   T.Start (W, 7, 8, 5_000, 100, Started);
   T.Step (W, True, 9, True, 5_001, Result); Check (Result = T.Timeline_Fault);
   T.Start (W, 7, 8, 5_000, 100, Started);
   T.Step (W, False, 8, True, 5_001, Result); Check (Result = T.Read_Fault);
   T.Start (W, 7, 8, 5_000, 100, Started);
   T.Step (W, True, 7, True, 5_010, Result); Check (Result = T.Pending);
   T.Step (W, True, 7, True, 5_009, Result); Check (Result = T.Clock_Fault);
   T.Start (W, 7, 8, 5_000, 100, Started);
   T.Step (W, True, 8, True, T.Clock_Unavailable, Result); Check (Result = T.Clock_Fault);
   -- Many deferred completions in a row.
   for Job in T.Value range 8 .. 10_000 loop
      T.Start (W, Job, Job + 1, T.Microseconds (Job) * 10, 1_000, Started);
      Check (Started);
      T.Step (W, True, Job, True, T.Microseconds (Job) * 10 + 1, Result);
      Check (Result = T.Pending);
      T.Step (W, True, Job + 1, True, T.Microseconds (Job) * 10 + 2, Result);
      Check (Result = T.Complete and T.Completed (W) = Job + 1);
   end loop;
   Ada.Text_IO.Put_Line ("Timeline PASS:" & Natural'Image (Checks) &
     " checks: torn reads, acceptance, explicit deadlines, gate, single terminal outcome");
end Timeline_Tests;
