with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Observatory_Metric_Summaries;
with Metric_Store;
with Observatory_Metric_Queries;
procedure Main is
   package Q renames Observatory_Metric_Queries;
   Reply, Bad : Q.Reply;
   package V renames Observatory_Metric_Summaries;
   package R renames V.R;
   package P renames V.P;
   use type P.Status;
   use type R.Record_Kind;
   S : Metric_Store.Store;
   Page : R.Page_Words := [others => 0];
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next : Metric_Store.Series_Cursor;
   Outcome : Metric_Store.Ingest_Outcome;
   Changed, Good : P.Summary_Row;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "check" & Checks'Image; end if;
   end Check;
   procedure Reject (Index : P.Row_Word_Index; Value : Unsigned_64) is
   begin
      Changed := Good; Changed (Index) := Value;
      Check (not V.Decode (Changed).Success);
   end Reject;
begin
   -- Real collector encoding, including a percentile bucket above the maximum.
   R.Put_Slot (Page, 0, R.Encode_Header
     ((Records => 2, Sequence => 1, Producer_Dropped => 7, others => <>)));
   R.Put_Slot (Page, 1, R.Encode
     ((Kind => R.Describe, Key => 1, Declared => R.Latency,
       Measure => R.Microseconds, Name => R.To_Name ("desktop.release"))));
   R.Put_Slot (Page, 2, R.Encode
     ((Kind => R.Latency, Key => 1, Time_Us => Unsigned_64'Last,
       Value => 9, Correlation => Unsigned_64'Last)));
   Metric_Store.Ingest (S, Unsigned_64'Last, P.Publisher_Tag (P.Issuance'Last),
                       Page, R.Batch_Bytes (2), Outcome);
   Check (Outcome.Result = P.OK and Outcome.Accepted = 2);
   Metric_Store.Fill_Summaries (S, 0, Rows, Written, Next);
   Check (Written = 1);
   Good := Rows (0);
   declare
      D : constant V.Decoded := V.Decode (Good);
   begin
      Check (D.Success);
      Check (V.Declaration (D.Value).Value.Kind = R.Describe);
      for I in P.Row_Word_Index loop
         Check (V.Word (D.Value, I) = Good (I));
      end loop;
      Check (V.Word (D.Value, P.Row_Source) = Unsigned_64'Last);
      Check (V.Word (D.Value, P.Row_Last_Time) = Unsigned_64'Last);
      Check (V.Word (D.Value, P.Row_Source_Producer_Dropped) = 7);
      Check (V.Word (D.Value, P.Row_P999) > V.Word (D.Value, P.Row_Maximum));
   end;
   Reject (P.Row_Source, 0);
   Reject (P.Row_Publisher_Tag, P.Observer_Tag (1));
   Reject (P.Row_Publisher_Tag, P.Publisher_Tag_Base);
   Reject (P.Row_Key, 0); Reject (P.Row_Key, 33);
   Reject (P.Row_Kind, 1); Reject (P.Row_Kind, 6);
   Reject (P.Row_Unit, 0); Reject (P.Row_Unit, 5);
   Reject (P.Row_Flags, 4);
   Reject (P.Row_Minimum, 11);
   Reject (P.Row_P90, 0);
   Reject (P.Row_Count_Word, 0);
   Reject (P.Row_First_Name, Character'Pos ('!'));
   for I in 21 .. 23 loop Reject (I, 1); end loop;
   for I in 28 .. 31 loop Reject (I, 1); end loop;
   Changed := Good;
   Changed (P.Row_Flags) := P.Flag_Total_Saturated or P.Flag_Histogram_Saturated;
   Changed (P.Row_Total) := Unsigned_64'Last;
   Check (V.Decode (Changed).Success);
   -- Declaration before first sample is valid; do not invent an observation.
   Changed := Good; Changed (P.Row_Count_Word) := 0;
   for I in P.Row_Minimum .. P.Row_P999 loop Changed (I) := 0; end loop;
   Check (V.Decode (Changed).Success);
   -- Unknown bytes anywhere after the NUL terminator must be rejected.
   Changed := Good; Changed (P.Row_First_Name + 3) := 1;
   Check (not V.Decode (Changed).Success);
   Reply := (Kernel_Valid => True, Label => P.Status'Enum_Rep (P.OK),
             Length => P.Message_Words,
             Payload => [Unsigned_64 (Written), Unsigned_64 (Next),
                         Metric_Store.Series_Slots, 0], others => <>);
   Check (Q.Valid_Page (Reply, 0, Rows));
   Bad := Reply; Bad.Kernel_Valid := False; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Kernel_Status := 1; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Label := 0; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Length := 3; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Flags := 1; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Reserved := 1; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (3) := 1; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (0) := 17; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (2) := 513; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (1) := 0; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (1) := 513; Check (not Q.Admitted (Bad, 0));
   Bad := Reply; Bad.Payload (1) := 5; Check (not Q.Admitted (Bad, 0));
   Rows (0) (P.Row_Flags) := 4;
   Check (not Q.Valid_Page (Reply, 0, Rows));
   Rows (0) := Good;
   Rows (1) := [others => Unsigned_64'Last];
   Check (Q.Valid_Page (Reply, 0, Rows)); -- unused rows are not observations
   -- Every legal ordinal: empty at end is terminal, never a continuation.
   for C in Q.Cursor loop
      Bad := Reply; Bad.Payload := [0, C, C, 0];
      Check (Q.Valid_Page (Bad, C, Rows));
      if C < Q.Maximum_Series then
         Bad.Payload (2) := C + 1;
         Check (not Q.Admitted (Bad, C));
      end if;
      if C <= Q.Maximum_Series - P.Rows_Per_Page then
         Bad.Payload := [P.Rows_Per_Page, C + P.Rows_Per_Page,
                         Q.Maximum_Series, 0];
         Check (Q.Admitted (Bad, C));
         Bad.Payload (1) := C + P.Rows_Per_Page - 1;
         Check (not Q.Admitted (Bad, C));
      end if;
   end loop;
   -- Walk actual sparse collector pages, including its empty terminal page.
   declare
      Other : Metric_Store.Store;
      Position : Metric_Store.Series_Cursor := 0;
      Queries : Natural := 0;
   begin
      Page := [others => 0];
      R.Put_Slot (Page, 0, R.Encode_Header
        ((Records => 32, Sequence => 1, others => <>)));
      for K in R.Metric_Key loop
         R.Put_Slot (Page, K, R.Encode
           ((Kind => R.Describe, Key => K, Declared => R.Counter,
             Measure => R.Count, Name => R.To_Name ("counter"))));
      end loop;
      Metric_Store.Ingest (Other, 42, P.Publisher_Tag (2), Page,
                          R.Batch_Bytes (32), Outcome);
      Check (Outcome.Result = P.OK and Outcome.Accepted = 32);
      loop
         Metric_Store.Fill_Summaries (Other, Position, Rows, Written, Next);
         Reply.Payload := [Unsigned_64 (Written), Unsigned_64 (Next),
                           Metric_Store.Series_Slots, 0];
         Check (Q.Valid_Page (Reply, Unsigned_64 (Position), Rows));
         Queries := Queries + 1;
         exit when Next = Metric_Store.Series_Slots;
         Check (Next > Position);
         Position := Next;
      end loop;
      Check (Queries = 3);
   end;
   Put_Line ("PASS observatory summaries:" & Checks'Image & " checks");
end Main;
