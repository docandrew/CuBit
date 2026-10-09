with Interfaces; use Interfaces;
with Ada.Text_IO;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with Metric_Store;
procedure Raw_Check is
 package R renames CuBit.Metric_Records;
 package P renames CuBit.Metric_Protocol;
 use type R.Metric_Record, P.Status;
 S : Metric_Store.Store;
 Page : R.Page_Words := (others => 0);
 Outcome : Metric_Store.Ingest_Outcome;
 Event : Metric_Store.Raw.Event;
 Next, Gap, Before : Unsigned_64;
 Present, Valid : Boolean;
 Count : Natural := 0;
 procedure Check (V : Boolean) is
 begin
  Count := Count + 1;
  if not V then Ada.Text_IO.Put_Line ("FAIL check" & Count'Image); raise Program_Error; end if;
 end Check;
 procedure Send (Tag, Seq, Drops : Unsigned_64; N : R.Batch_Record_Count) is
 begin
  R.Put_Slot (Page, 0, R.Encode_Header ((N, Seq, Drops, R.Monotonic_Microseconds)));
  Metric_Store.Ingest (S, 77, Tag, Page, R.Batch_Bytes (N), Outcome);
 end Send;
 procedure Fetch (Cursor : Unsigned_64) is
 begin
  Metric_Store.Read_History (S, Cursor, Event, Next, Gap, Present, Valid);
 end Fetch;
 Declaration : constant R.Metric_Record :=
  (R.Describe, 1, R.Span, R.Microseconds, R.To_Name ("draw.span"));
 Span : constant R.Metric_Record := (R.Span, 1, 20, 30, 991);
begin
 R.Put_Slot (Page, 1, R.Encode (Declaration));
 R.Put_Slot (Page, 2, R.Encode (Span));
 R.Put_Slot (Page, 3, R.Encode ((R.Gauge, 1, 25, 100, 992)));
 Send (P.Publisher_Tag (1), 1, 0, 3);
 Check (Outcome.Accepted = 2 and Outcome.Rejected = 1 and Metric_Store.History_Next (S) = 3);
 Fetch (1); Check (Present and Valid and Event.Value = Declaration and Gap = 0);
 Fetch (2); Check (Present and Event.Value = Span and Event.Pid = 77 and Event.Publisher = P.Publisher_Tag (1));
 Before := Metric_Store.History_Next (S);
 Send (P.Publisher_Tag (1), 1, 0, 3);
 Check (Outcome.Result = P.Invalid_Request and Metric_Store.History_Next (S) = Before);
 R.Put_Slot (Page, 1, R.Encode (Span));
 Send (P.Publisher_Tag (1), 4, 7, 1);
 Fetch (3); Check (Present and Event.Batch = 4 and Event.Batch_Gaps = 2 and Event.Producer_Dropped = 7);
 R.Put_Slot (Page, 1, R.Encode (Declaration));
 R.Put_Slot (Page, 2, R.Encode (Span));
 Send (P.Publisher_Tag (2), 1, 0, 2);
 Fetch (4); Check (Present and Event.Pid = 77 and Event.Publisher = P.Publisher_Tag (2));
 Fetch (3); Check (Event.Publisher = P.Publisher_Tag (1));
 for I in Unsigned_64 range 1 .. 400 loop
  R.Put_Slot (Page, 1, R.Encode ((R.Span, 1, I, I + 2, I + 1000)));
  Send (P.Publisher_Tag (1), I + 4, 7, 1);
  Check (Outcome.Accepted = 1 and Outcome.Rejected = 0);
 end loop;
 Check (Metric_Store.History_Next (S) = 406);
 Fetch (1); Check (Present and Valid and Gap = 149 and Event.Sequence = 150);
 Fetch (405); Check (Present and Event.Value = (R.Span, 1, 400, 402, 1400) and Event.Batch = 404);
 Before := Metric_Store.History_Next (S);
 R.Put_Slot (Page, 1, (others => 0));
 Send (P.Publisher_Tag (1), 405, 7, 1);
 Check (Outcome.Accepted = 0 and Outcome.Rejected = 1 and Metric_Store.History_Next (S) = Before);
 Fetch (406); Check (Valid and not Present and Next = 406 and Gap = 0);
 Fetch (407); Check (not Valid and not Present and Next = 407 and Gap = 0);
 Check (Metric_Store.History_Dropped (S) = 0);
 Ada.Text_IO.Put_Line ("PASS raw-store checks:" & Count'Image);
end Raw_Check;
