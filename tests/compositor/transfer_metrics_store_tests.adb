with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Work_Metrics;
with CuBit.Metric_Protocol;
with Metric_Store;
procedure Transfer_Metrics_Store_Tests is
   package M renames Compositor_Work_Metrics;
   package R renames M.R;
   package P renames CuBit.Metric_Protocol;
   use type P.Status;
   Store : Metric_Store.Store;
   Sequence : R.Batch_Sequence := 1;
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next : Metric_Store.Series_Cursor;
   procedure Ingest (Value : R.Metric_Record; Accepted : Boolean := True) is
      Page : R.Page_Words := [others => 0];
      Outcome : Metric_Store.Ingest_Outcome;
   begin
      R.Put_Slot (Page, 0, R.Encode_Header ((1, Sequence, 0, R.Monotonic_Microseconds)));
      R.Put_Slot (Page, 1, R.Encode (Value));
      Metric_Store.Ingest (Store, 42, P.Publisher_Tag (1), Page, R.Batch_Bytes (1), Outcome);
      pragma Assert (Outcome.Result = P.OK and Outcome.Accepted = (if Accepted then 1 else 0)
         and Outcome.Rejected = (if Accepted then 0 else 1));
      Sequence := Sequence + 1;
   end Ingest;
begin
   -- Data without metadata must not be silently interpreted as pixel counts.
   Ingest (M.Prepare (M.GPU_Readback_Bytes, 4096, 100).Value, False);
   for Kind in M.Work_Kind loop Ingest (M.Declaration (Kind)); end loop;
   Ingest (M.Prepare (M.Scene_Pixels, 64, 101).Value);
   Ingest (M.Prepare (M.Repair_Pixels, 128, 101).Value);
   Ingest (M.Prepare (M.GPU_Readback_Bytes, 4096, 101).Value);
   Ingest (M.Prepare (M.CPU_Copy_Bytes, 1024, 101).Value);
   -- Every newly submitted page may redeclare; metadata must not reset totals.
   for Kind in M.Work_Kind loop Ingest (M.Declaration (Kind)); end loop;
   Ingest (M.Prepare (M.GPU_Readback_Bytes, 8192, 102).Value);
   Ingest (M.Prepare (M.CPU_Copy_Bytes, 3072, 102).Value);
   Metric_Store.Fill_Summaries (Store, 0, Rows, Written, Next);
   pragma Assert (Written = 4 and Next = Metric_Store.Series_Slots);
   for I in 0 .. 3 loop
      pragma Assert (Rows (I) (P.Row_Source) = 42 and
        Rows (I) (P.Row_Publisher_Tag) = P.Publisher_Tag (1) and
        Rows (I) (P.Row_Key) = Unsigned_64 (7 + I) and
        Rows (I) (P.Row_Kind) = R.Record_Kind'Enum_Rep (R.Counter) and
        Rows (I) (P.Row_Unit) = (if I < 2 then R.Unit'Enum_Rep (R.Count) else R.Unit'Enum_Rep (R.Bytes)));
   end loop;
   pragma Assert (Rows (0) (P.Row_Total) = 64 and Rows (1) (P.Row_Total) = 128);
   pragma Assert (Rows (2) (P.Row_Count_Word) = 2 and Rows (2) (P.Row_Total) = 12288);
   pragma Assert (Rows (3) (P.Row_Count_Word) = 2 and Rows (3) (P.Row_Total) = 4096);
   Ada.Text_IO.Put_Line ("TRANSFER-METRICS-STORE: PASS byte units, separate keys, delta totals, undeclared rejection and redeclaration");
end Transfer_Metrics_Store_Tests;
