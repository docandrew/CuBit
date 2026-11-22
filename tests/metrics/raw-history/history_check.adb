with Interfaces; use Interfaces;
with Ada.Text_IO;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with Metric_History;
procedure History_Check is
 package R renames CuBit.Metric_Records;
 use type R.Record_Kind;
 package P renames CuBit.Metric_Protocol;
 package H is new Metric_History (Capacity => 3, Sequence_Limit => 1001);
 S : H.State;
 E : H.Event;
 Next, Gap : Unsigned_64;
 OK, Present, Valid : Boolean;
 procedure Require (Condition : Boolean) is
 begin
  if not Condition then raise Program_Error; end if;
 end Require;
begin
 H.Read (S, 1, E, Next, Gap, Present, Valid);
 Require (Valid and not Present and Next = 1 and Gap = 0);
 for I in Unsigned_64 range 1 .. 1000 loop
  H.Append (S, 7, P.Publisher_Tag (P.Issuance (1 + I mod 2)), I, I / 7, I / 11,
    (R.Span, 1, I * 10, I * 10 + 4, I + 90), OK);
  Require (OK and H.Following (S) = I + 1);
  for J in H.First (S) .. I loop
   H.Read (S, J, E, Next, Gap, Present, Valid);
   Require (Valid and Present and Next = J + 1 and Gap = 0);
   Require (E.Pid = 7 and E.Publisher = P.Publisher_Tag (P.Issuance (1 + J mod 2)) and E.Batch = J);
   Require (E.Producer_Dropped = J / 7 and E.Batch_Gaps = J / 11);
   Require (E.Value.Kind = R.Span and E.Value.Start_Us = J * 10
    and E.Value.End_Us = J * 10 + 4 and E.Value.Span_Correlation = J + 90);
  end loop;
  H.Read (S, 1, E, Next, Gap, Present, Valid);
  Require (Valid and Present and E.Sequence = H.First (S)
    and Gap = H.First (S) - 1);
 end loop;
 Require (H.Exhausted (S));
 H.Append (S, 8, P.Publisher_Tag (2), 1, 0, 0, (R.Gauge, 1, 1, 1, 1), OK);
 Require (not OK and H.Following (S) = 1001);
 H.Read (S, 1000, E, Next, Gap, Present, Valid);
 Require (Present and E.Pid = 7 and E.Publisher = P.Publisher_Tag (1));
 H.Read (S, 0, E, Next, Gap, Present, Valid);
 Require (not Valid and not Present and Next = 0 and Gap = 0);
 H.Read (S, 1002, E, Next, Gap, Present, Valid);
 Require (not Valid and not Present and Next = 1002 and Gap = 0);
 H.Read (S, 1001, E, Next, Gap, Present, Valid);
 Require (Valid and not Present and Next = 1001 and Gap = 0);
 Ada.Text_IO.Put_Line ("PASS: wrap, slow-reader gaps, preserved identities/spans, invalid cursors, exhaustion");
end History_Check;
