with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Release_Metrics;
with CuBit.Metric_Protocol;
with Metric_Store;
procedure Release_Metrics_Store_Tests is
   package M renames Compositor_Release_Metrics;
   package R renames M.Records;
   package P renames CuBit.Metric_Protocol;
   use type P.Status;
   Store : Metric_Store.Store;
   Sequence : R.Batch_Sequence := 1;
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next : Metric_Store.Series_Cursor;
   procedure Ingest
     (Value : R.Metric_Record; Pid : Unsigned_64 := 42;
      Tag : Unsigned_64 := P.Publisher_Tag (1); Expected_Accepted : Boolean := True) is
      Page : R.Page_Words := [others => 0];
      Outcome : Metric_Store.Ingest_Outcome;
   begin
      R.Put_Slot (Page, 0, R.Encode_Header ((1, Sequence, 0, R.Monotonic_Microseconds)));
      R.Put_Slot (Page, 1, R.Encode (Value));
      -- Hosted service-policy boundary: PID/tag supplied by the fixture,
      -- no claim to kernel authentication or actual asynchronous IPC.
      Metric_Store.Ingest (Store, Pid, Tag, Page, R.Batch_Bytes (1), Outcome);
      pragma Assert (Outcome.Result = P.OK and
        Outcome.Accepted = (if Expected_Accepted then 1 else 0) and
        Outcome.Rejected = (if Expected_Accepted then 0 else 1));
      Sequence := Sequence + 1;
   end Ingest;
begin
   Ingest (M.Declaration (0)); Ingest (M.Declaration (1));
   Ingest (M.Prepare ((0, 7, 11, 100, 104)).Value);
   -- Reopened output: a new session, same metric series, fresh global frame.
   Ingest (M.Prepare ((0, 8, 12, 200, 208)).Value);
   Ingest (M.Prepare ((1, 9, 13, 300, 316)).Value);
   Metric_Store.Fill_Summaries (Store, 0, Rows, Written, Next);
   pragma Assert (Written = 2 and Next = Metric_Store.Series_Slots);
   for I in 0 .. 1 loop
      pragma Assert (Rows (I) (P.Row_Source) = 42 and
                     Rows (I) (P.Row_Publisher_Tag) = P.Publisher_Tag (1) and
                     Rows (I) (P.Row_Key) = Unsigned_64 (M.Key (I)) and
                     Rows (I) (P.Row_Kind) = R.Record_Kind'Enum_Rep (R.Span) and
                     Rows (I) (P.Row_Unit) = R.Unit'Enum_Rep (R.Microseconds));
   end loop;
   pragma Assert (Rows (0) (P.Row_Count_Word) = 2 and Rows (0) (P.Row_Total) = 12 and
                  Rows (0) (P.Row_Minimum) = 4 and Rows (0) (P.Row_Maximum) = 8);
   pragma Assert (Rows (1) (P.Row_Count_Word) = 1 and Rows (1) (P.Row_Total) = 16 and
                  Rows (1) (P.Row_Minimum) = 16 and Rows (1) (P.Row_Maximum) = 16);
   -- Fill all source slots, expire them, then force eviction of this source.
   for I in 2 .. Metric_Store.Maximum_Sources loop
      Ingest (M.Declaration (0), Unsigned_64 (41 + I), P.Publisher_Tag (Unsigned_64 (I)));
   end loop;
   Metric_Store.Advance_Time (Store, Metric_Store.Source_Lease_Ms + 1);
   Ingest (M.Declaration (0), 100, P.Publisher_Tag (100));
   Ingest (M.Prepare ((0, 8, 14, 400, 420)).Value, Expected_Accepted => False);
   -- Repeating declarations restores the source without changing authority.
   Ingest (M.Declaration (0)); Ingest (M.Declaration (1));
   Ingest (M.Prepare ((0, 8, 15, 500, 520)).Value);
   declare Cursor : Metric_Store.Series_Cursor := 0; Found : Boolean := False;
   begin
      loop
         Metric_Store.Fill_Summaries (Store, Cursor, Rows, Written, Next);
         for I in 0 .. Written - 1 loop
            if Rows (I) (P.Row_Source) = 42 and Rows (I) (P.Row_Key) = 1 then
               pragma Assert (Rows (I) (P.Row_Count_Word) = 1 and Rows (I) (P.Row_Total) = 20);
               Found := True;
            end if;
         end loop;
         exit when Next = Metric_Store.Series_Slots;
         pragma Assert (Next > Cursor); Cursor := Next;
      end loop;
      pragma Assert (Found);
   end;
   Ada.Text_IO.Put_Line ("RELEASE-METRICS-STORE: PASS actual metric store declarations, output isolation, reopen, lease eviction/redeclaration and duration summaries");
end Release_Metrics_Store_Tests;
