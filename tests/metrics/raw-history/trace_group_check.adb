with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Metric_Records;
with CuBit.Metric_Batches;
with CuBit.Metric_Protocol;
with Metric_Store;
procedure Trace_Group_Check is
   package R renames CuBit.Metric_Records;
   package B renames CuBit.Metric_Batches;
   package P renames CuBit.Metric_Protocol;
   use type R.Metric_Record, R.Page_Words, B.Page_Pair, P.Status;
   Values : R.Trace_Group;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   Plain : constant R.Metric_Record := (R.Counter, 1, 0, 1, 0);
   Accepted, Sealed : Boolean;
   Page : B.Page_Id;
   Bytes : Unsigned_64;
   Data : R.Slot_Words;
   S : Metric_Store.Store;
   Outcome : Metric_Store.Ingest_Outcome;
   E : Metric_Store.Raw.Event;
   Next, Gap : Unsigned_64;
   Available, Valid : Boolean;
   Batch : R.Page_Words := (others => 0);
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Summary_Next : Metric_Store.Series_Cursor;
begin
   for I in R.Trace_Part loop
      Values (I) := (R.Trace, 1, Unsigned_64'Last, I,
                     (Unsigned_64 (I), Unsigned_64'Last, 0, 16#8000_0000_0000_0000#));
      Check (R.Decode (R.Encode (Values (I))).Success and then
             R.Decode (R.Encode (Values (I))).Value = Values (I));
   end loop;
   Data := R.Encode (Values (0));
   Data (2) := 0; Check (not R.Decode (Data).Success);
   Data (2) := 1; Data (3) := 4; Check (not R.Decode (Data).Success);
   Data (3) := Unsigned_64'Last; Check (not R.Decode (Data).Success);
   Check (R.Valid_Group (Values));
   for Used in 0 .. R.Maximum_Records loop
      declare
         State : B.Builder;
         Pages, Before : B.Page_Pair := (others => (others => 0));
      begin
         for I in 1 .. Used loop
            B.Append (State, Pages, Plain, Accepted); Check (Accepted);
         end loop;
         Before := Pages;
         B.Append_Group (State, Pages, Values, Accepted);
         Check (Accepted = (Used <= R.Maximum_Records - 4));
         if Accepted then
            Check (B.Used (State, B.Filling (State)) = Used + 4);
            Check (B.Dropped (State) = 0);
            for I in R.Trace_Part loop
               Check (R.Decode (R.Slot (Pages (B.Filling (State)), Used + I + 1)).Value = Values (I));
            end loop;
            for W in 0 .. Used * 8 + 7 loop
               Check (Pages (B.Filling (State)) (W) = Before (B.Filling (State)) (W));
            end loop;
         else
            Check (Pages = Before and B.Used (State, B.Filling (State)) = Used);
            Check (B.Dropped (State) = 4);
         end if;
      end;
   end loop;
   declare
      State : B.Builder;
      Pages, Held : B.Page_Pair := (others => (others => 0));
   begin
      for I in 1 .. 2 loop
         B.Append_Group (State, Pages, Values, Accepted); Check (Accepted);
         B.Seal (State, Pages, Sealed, Page, Bytes);
         Check (Sealed and Bytes = R.Batch_Bytes (4));
      end loop;
      Held := Pages;
      for I in 1 .. 1000 loop
         B.Append_Group (State, Pages, Values, Accepted);
         Check (not Accepted and Pages = Held and B.Dropped (State) = Unsigned_64 (I) * 4);
      end loop;
      B.Complete (State, 1);
      B.Append_Group (State, Pages, Values, Accepted);
      Check (Accepted and Pages (2) = Held (2) and B.Dropped (State) = 4000);
   end;
   --  Trace schema key 1 must not alter existing counter series key 1.
   R.Put_Slot (Batch, 0, R.Encode_Header ((6, 1, 0, R.Monotonic_Microseconds)));
   R.Put_Slot (Batch, 1, R.Encode ((R.Describe, 1, R.Counter, R.Count, R.To_Name ("same.key"))));
   R.Put_Slot (Batch, 2, R.Encode (Plain));
   for I in R.Trace_Part loop R.Put_Slot (Batch, I + 3, R.Encode (Values (I))); end loop;
   Metric_Store.Ingest (S, 77, P.Publisher_Tag (1), Batch, R.Batch_Bytes (6), Outcome);
   Check (Outcome.Result = P.OK and Outcome.Accepted = 6 and Outcome.Rejected = 0);
   for I in R.Trace_Part loop
      Metric_Store.Read_History (S, Unsigned_64 (I + 3), E, Next, Gap, Available, Valid);
      Check (Valid and Available and Gap = 0 and E.Value = Values (I) and
             E.Pid = 77 and E.Publisher = P.Publisher_Tag (1) and E.Batch = 1);
   end loop;
   Metric_Store.Fill_Summaries (S, 0, Rows, Written, Summary_Next);
   Check (Written = 1 and Rows (0) (P.Row_Count_Word) = 1 and Rows (0) (P.Row_Total) = 1);
   Ada.Text_IO.Put_Line ("PASS trace-group checks" & Checks'Image);
end Trace_Group_Check;
