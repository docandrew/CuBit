pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with CuBit.Metric_Batches;
with CuBit.Log_Protocol;
with Metric_Store;
with Metric_Histograms;

--  Linux-hosted regression tests for the metric record codec, producer
--  batching, metrics.svc aggregation and authorization. Peer identities and
--  tags are supplied by this harness, not by kernel IPC.
procedure Main is
   package R renames CuBit.Metric_Records;
   package P renames CuBit.Metric_Protocol;
   package B renames CuBit.Metric_Batches;
   use type R.Record_Kind;
   use type R.Metric_Record;
   use type R.Slot_Words;
   use type R.Batch_Header;
   use type P.Status;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   Pid_A : constant Unsigned_64 := 40;
   Pid_B : constant Unsigned_64 := 41;
   Tag_A : constant Unsigned_64 := P.Publisher_Tag (1);
   Tag_B : constant Unsigned_64 := P.Publisher_Tag (2);
   Tag_A_Reused_Pid : constant Unsigned_64 := P.Publisher_Tag (3);

   function Describe
     (Key : R.Metric_Key; Kind : R.Metric_Kind; Measure : R.Unit;
      Name : String) return R.Metric_Record is
     (Kind => R.Describe, Key => Key, Declared => Kind, Measure => Measure,
      Name => R.To_Name (Name));
   function Sample
     (Kind : R.Metric_Kind; Key : R.Metric_Key; Time, Value : Unsigned_64;
      Correlation : Unsigned_64 := 0) return R.Metric_Record is
     (case Kind is
         when R.Counter => (Kind => R.Counter, Key => Key, Time_Us => Time,
                            Value => Value, Correlation => Correlation),
         when R.Gauge => (Kind => R.Gauge, Key => Key, Time_Us => Time,
                          Value => Value, Correlation => Correlation),
         when R.Latency => (Kind => R.Latency, Key => Key, Time_Us => Time,
                            Value => Value, Correlation => Correlation),
         when R.Span => (Kind => R.Span, Key => Key, Start_Us => Time,
                         End_Us => Time + Value,
                         Span_Correlation => Correlation));

   --  A tiny producer: builder + two pages; Flush seals and "submits" by
   --  handing a private copy of the page to the store, then completes.
   type Producer is record
      State : B.Builder;
      Pages : B.Page_Pair := [others => [others => 0]];
      Pid, Tag : Unsigned_64 := 0;
   end record;

   Store : Metric_Store.Store;

   procedure Emit (Item : in out Producer; Value : R.Metric_Record) is
      Accepted : Boolean;
   begin
      B.Append (Item.State, Item.Pages, Value, Accepted);
      Check (Accepted, "append accepted");
   end Emit;

   procedure Flush
     (Item : in out Producer; Outcome : out Metric_Store.Ingest_Outcome) is
      Sealed : Boolean;
      Page : B.Page_Id;
      Bytes : Unsigned_64;
   begin
      B.Seal (Item.State, Item.Pages, Sealed, Page, Bytes);
      Check (Sealed, "seal");
      Metric_Store.Ingest
        (Store, Item.Pid, Item.Tag, Item.Pages (Page), Bytes, Outcome);
      B.Complete (Item.State, Page);
   end Flush;

   type Row_Array is array (Positive range <>) of P.Summary_Row;
   Collected : Row_Array (1 .. Metric_Store.Series_Slots);
   Collected_Count : Natural := 0;

   procedure Collect_All is
      Page : P.Summary_Page;
      Written : P.Row_Count;
      Cursor : Metric_Store.Series_Cursor := 0;
      Next : Metric_Store.Series_Cursor;
      Calls : Natural := 0;
   begin
      Collected_Count := 0;
      loop
         Metric_Store.Fill_Summaries (Store, Cursor, Page, Written, Next);
         Calls := Calls + 1;
         for I in 0 .. Written - 1 loop
            Collected_Count := Collected_Count + 1;
            Collected (Collected_Count) := Page (I);
         end loop;
         exit when Next = Metric_Store.Series_Slots;
         Check (Written = P.Rows_Per_Page, "partial page only at end");
         Cursor := Next;
      end loop;
      Check (Calls <= Metric_Store.Series_Slots / P.Rows_Per_Page + 1,
             "bounded paging");
   end Collect_All;

   function Find (Pid, Tag : Unsigned_64; Key : R.Metric_Key) return Natural
   is
   begin
      for I in 1 .. Collected_Count loop
         if Collected (I) (P.Row_Source) = Pid and then
           Collected (I) (P.Row_Publisher_Tag) = Tag and then
           Collected (I) (P.Row_Key) = Unsigned_64 (Key)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Name_Of (Row : P.Summary_Row) return String is
      Text : String (1 .. R.Maximum_Name_Bytes);
      Length : Natural := 0;
   begin
      for W in 0 .. R.Name_Words - 1 loop
         for Byte in 0 .. R.Bytes_Per_Word - 1 loop
            declare
               Value : constant Unsigned_64 :=
                 Shift_Right (Row (P.Row_First_Name + W), 8 * Byte) and 16#FF#;
            begin
               if Value /= 0 then
                  Length := Length + 1;
                  Text (Length) := Character'Val (Value);
               end if;
            end;
         end loop;
      end loop;
      return Text (1 .. Length);
   end Name_Of;

   procedure Test_Codec is
      Values : constant array (Positive range <>) of R.Metric_Record :=
        [Describe (1, R.Latency, R.Microseconds, "frame.latency"),
         Describe (32, R.Span, R.Nanoseconds, "a-b_c.9"),
         Sample (R.Counter, 2, 10, 7, 99),
         Sample (R.Gauge, 3, 11, Unsigned_64'Last),
         Sample (R.Latency, 4, 12, 0),
         Sample (R.Span, 5, 100, 50, 7)];
      Words : R.Slot_Words;
      Decoded : R.Decoded_Record;
   begin
      for V of Values loop
         Words := R.Encode (V);
         Decoded := R.Decode (Words);
         Check (Decoded.Success and then Decoded.Value = V, "round trip");
      end loop;
      Check (R.To_Name ("").Length = 0, "empty name rejected");
      Check (R.To_Name ("Upper").Length = 0, "uppercase rejected");
      Check (R.To_Name ("sp ace").Length = 0, "space rejected");
      Check (R.To_Name ([1 .. 33 => 'a']).Length = 0, "overlong rejected");
      Check (R.To_Name ([1 .. 32 => 'z']).Length = 32, "32 bytes allowed");

      Words := R.Encode (Values (3));
      Words (0) := 9;
      Check (not R.Decode (Words).Success, "unknown kind");
      Words := R.Encode (Values (3));
      Words (1) := 0;
      Check (not R.Decode (Words).Success, "key zero");
      Words (1) := 33;
      Check (not R.Decode (Words).Success, "key 33");
      Words := R.Encode (Values (3));
      Words (7) := 1;
      Check (not R.Decode (Words).Success, "reserved word");
      Words := R.Encode (Values (1));
      Words (2) := R.Record_Kind'Enum_Rep (R.Describe);
      Check (not R.Decode (Words).Success, "describe as declared kind");
      Words := R.Encode (Values (1));
      Words (3) := 0;
      Check (not R.Decode (Words).Success, "unit zero");
      Words := R.Encode (Values (1));
      --  "frame.la" + zero byte + nonzero byte: embedded terminator.
      Words (5) := Words (5) and 16#FFFF_FFFF_FFFF_FF00#;
      Check (not R.Decode (Words).Success, "embedded zero in name");
      Words := R.Encode (Values (1));
      Words (4) := (Words (4) and 16#FFFF_FFFF_FFFF_FF00#) or
        Character'Pos ('F');
      Check (not R.Decode (Words).Success, "uppercase in wire name");
      Words := R.Encode (Values (6));
      Words (2) := Words (3) + 1;
      Check (not R.Decode (Words).Success, "reversed span");

      --  Canonical encoding: every accepted slot re-encodes identically.
      declare
         Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
         function Next return Unsigned_64 is
         begin
            Seed := Seed xor Shift_Left (Seed, 13);
            Seed := Seed xor Shift_Right (Seed, 7);
            Seed := Seed xor Shift_Left (Seed, 17);
            return Seed;
         end Next;
         Accepted : Natural := 0;
      begin
         for Trial in 1 .. 200_000 loop
            for W in R.Slot_Word_Index loop
               Words (W) := Next;
            end loop;
            --  Bias toward plausible slots so every branch is exercised.
            Words (0) := Words (0) mod 7;
            Words (1) := Words (1) mod 35;
            if Trial mod 2 = 0 then
               Words (5) := 0;
               Words (6) := 0;
               Words (7) := 0;
               Words (2) := Words (2) mod 8;
               Words (3) := Words (3) mod 6;
            end if;
            if Trial mod 4 = 0 then
               for W in 4 .. 7 loop
                  Words (W) := (Words (W) and 16#0707_0707_0707_0707#) or
                    16#6060_6060_6060_6060#;
               end loop;
            end if;
            Decoded := R.Decode (Words);
            if Decoded.Success then
               Accepted := Accepted + 1;
               Check (R.Encode (Decoded.Value) = Words, "canonical slot");
            end if;
         end loop;
         Check (Accepted > 1_000, "fuzz accepted enough slots");
      end;
   end Test_Codec;

   procedure Test_Header is
      Header : constant R.Batch_Header :=
        (Records => 3, Sequence => 7, Producer_Dropped => 2,
         Clock => R.Monotonic_Microseconds);
      Words : R.Slot_Words := R.Encode_Header (Header);
      Decoded : R.Decoded_Header := R.Decode_Header (Words, 4 * R.Slot_Bytes);
   begin
      Check (Decoded.Success and then Decoded.Value = Header, "header trip");
      Check (not R.Decode_Header (Words, 3 * R.Slot_Bytes).Success,
             "length mismatch");
      Words (0) := 0;
      Check (not R.Decode_Header (Words, 4 * R.Slot_Bytes).Success, "magic");
      Words := R.Encode_Header (Header);
      Words (1) := 64;
      Check (not R.Decode_Header (Words, 65 * R.Slot_Bytes).Success, "count");
      Words (1) := 0;
      Check (not R.Decode_Header (Words, R.Slot_Bytes).Success, "count 0");
      Words := R.Encode_Header (Header);
      Words (2) := 0;
      Check (not R.Decode_Header (Words, 4 * R.Slot_Bytes).Success, "seq 0");
      Words (2) := Unsigned_64'Last;
      Check (not R.Decode_Header (Words, 4 * R.Slot_Bytes).Success, "seq max");
      Words := R.Encode_Header (Header);
      Words (4) := 2;
      Check (not R.Decode_Header (Words, 4 * R.Slot_Bytes).Success, "clock");
      Words := R.Encode_Header (Header);
      Words (6) := 1;
      Decoded := R.Decode_Header (Words, 4 * R.Slot_Bytes);
      Check (not Decoded.Success, "header reserved");
   end Test_Header;

   procedure Test_Batches is
      Item : Producer;
      Accepted, Sealed : Boolean;
      Page, Second : B.Page_Id;
      Bytes : Unsigned_64;
      Header : R.Decoded_Header;
      use type B.Page_Id;
   begin
      for I in 1 .. R.Maximum_Records loop
         B.Append (Item.State, Item.Pages,
                   Sample (R.Latency, 1, Unsigned_64 (I), 5), Accepted);
         Check (Accepted, "fill page");
      end loop;
      B.Append (Item.State, Item.Pages, Sample (R.Latency, 1, 0, 5), Accepted);
      Check (not Accepted and B.Dropped (Item.State) = 1, "full page drops");
      B.Seal (Item.State, Item.Pages, Sealed, Page, Bytes);
      Check (Sealed and Bytes = R.Page_Bytes, "seal full page");
      Header := R.Decode_Header (R.Slot (Item.Pages (Page), 0), Bytes);
      Check (Header.Success and then Header.Value.Sequence = 1 and then
             Header.Value.Producer_Dropped = 1, "header carries drops");
      B.Append (Item.State, Item.Pages, Sample (R.Gauge, 2, 0, 5), Accepted);
      Check (Accepted and B.Filling (Item.State) /= Page, "second page fills");
      B.Seal (Item.State, Item.Pages, Sealed, Second, Bytes);
      Check (Sealed and Second /= Page and Bytes = 2 * R.Slot_Bytes,
             "second seal");
      B.Append (Item.State, Item.Pages, Sample (R.Gauge, 2, 0, 5), Accepted);
      Check (not Accepted and B.Dropped (Item.State) = 2,
             "both in flight drops without waiting");
      B.Seal (Item.State, Item.Pages, Sealed, Page, Bytes);
      Check (not Sealed, "nothing to seal while both in flight");
      B.Complete (Item.State, Second);
      B.Append (Item.State, Item.Pages, Sample (R.Gauge, 2, 0, 5), Accepted);
      Check (Accepted and B.Filling (Item.State) = Second,
             "completed page refills");
      B.Seal (Item.State, Item.Pages, Sealed, Page, Bytes);
      Header := R.Decode_Header (R.Slot (Item.Pages (Page), 0), Bytes);
      Check (Header.Success and then Header.Value.Sequence = 3,
             "sequence increments");
   end Test_Batches;

   procedure Test_Store is
      A : Producer := (Pid => Pid_A, Tag => Tag_A, others => <>);
      Bp : Producer := (Pid => Pid_B, Tag => Tag_B, others => <>);
      Reused : Producer := (Pid => Pid_A, Tag => Tag_A_Reused_Pid,
                            others => <>);
      Outcome : Metric_Store.Ingest_Outcome;
      Row : Natural;
      Junk : R.Page_Words := [others => 0];
   begin
      --  Malformed batch allocates nothing.
      Metric_Store.Ingest (Store, Pid_A, Tag_A, Junk, R.Page_Bytes, Outcome);
      Check (Outcome.Result = P.Invalid_Request, "malformed batch");
      Collect_All;
      Check (Collected_Count = 0, "no rows after malformed batch");

      Emit (A, Describe (1, R.Latency, R.Microseconds, "frame.latency"));
      Emit (A, Describe (2, R.Counter, R.Count, "frames"));
      Emit (A, Describe (3, R.Gauge, R.Bytes, "queue.bytes"));
      Emit (A, Describe (4, R.Span, R.Microseconds, "input.to.present"));
      Flush (A, Outcome);
      Check (Outcome.Result = P.OK and Outcome.Accepted = 4, "declare");
      for V in 1 .. 1000 loop
         Emit (A, Sample (R.Latency, 1, Unsigned_64 (V), Unsigned_64 (V)));
         if V mod 60 = 0 then
            Flush (A, Outcome);
            Check (Outcome.Result = P.OK and Outcome.Rejected = 0, "batch");
         end if;
      end loop;
      Emit (A, Sample (R.Counter, 2, 1, 240));
      Emit (A, Sample (R.Counter, 2, 2, 240));
      Emit (A, Sample (R.Gauge, 3, 3, 4096));
      Emit (A, Sample (R.Gauge, 3, 4, 1024));
      Emit (A, Sample (R.Span, 4, 1_000, 2_500, 77));
      --  Rejections: wrong kind, undeclared key, conflicting redeclare,
      --  while an identical redeclare is accepted.
      Emit (A, Sample (R.Gauge, 1, 5, 1));
      Emit (A, Sample (R.Latency, 9, 5, 1));
      Emit (A, Describe (2, R.Counter, R.Count, "other"));
      Emit (A, Describe (2, R.Counter, R.Count, "frames"));
      Flush (A, Outcome);
      Check (Outcome.Result = P.OK and Outcome.Accepted = 46 and
             Outcome.Rejected = 3, "mixed batch");

      Collect_All;
      Row := Find (Pid_A, Tag_A, 1);
      Check (Row /= 0, "latency row");
      if Row /= 0 then
         declare
            S : P.Summary_Row renames Collected (Row);
         begin
            Check (S (P.Row_Count_Word) = 1000, "latency count");
            Check (S (P.Row_Minimum) = 1 and S (P.Row_Maximum) = 1000,
                   "min/max");
            Check (S (P.Row_P50) = 512, "p50 bucket bound");
            Check (S (P.Row_P90) = 1024 and S (P.Row_P99) = 1024 and
                   S (P.Row_P999) = 1024, "tail bucket bounds");
            Check (S (P.Row_Total) = 500_500, "latency sum");
            Check (S (P.Row_Series_Rejected) = 1, "wrong-kind counted");
            Check (S (P.Row_Source_Rejected) = 1, "undeclared counted");
            Check (S (P.Row_Kind) = R.Record_Kind'Enum_Rep (R.Latency) and
                   S (P.Row_Unit) = R.Unit'Enum_Rep (R.Microseconds),
                   "kind/unit");
            Check (Name_Of (S) = "frame.latency", "name");
            Check (S (P.Row_Source_Batch_Gaps) = 0, "no gaps yet");
         end;
      end if;
      Row := Find (Pid_A, Tag_A, 2);
      Check (Row /= 0 and then Collected (Row) (P.Row_Total) = 480 and then
             Collected (Row) (P.Row_Series_Rejected) = 1, "counter total");
      Row := Find (Pid_A, Tag_A, 3);
      Check (Row /= 0 and then Collected (Row) (P.Row_Total) = 1024 and then
             Collected (Row) (P.Row_Maximum) = 4096, "gauge latest/max");
      Row := Find (Pid_A, Tag_A, 4);
      Check (Row /= 0 and then Collected (Row) (P.Row_Minimum) = 2500 and then
             Collected (Row) (P.Row_Last_Time) = 3500, "span duration");

      --  Isolation: B and a reused PID with a new tag are separate sources.
      Emit (Bp, Describe (1, R.Counter, R.Count, "packets"));
      Emit (Bp, Sample (R.Counter, 1, 1, 5));
      Flush (Bp, Outcome);
      Emit (Reused, Describe (1, R.Gauge, R.Count, "reused"));
      Flush (Reused, Outcome);
      Collect_All;
      Row := Find (Pid_A, Tag_A, 1);
      Check (Row /= 0 and then Collected (Row) (P.Row_Count_Word) = 1000 and
             then Name_Of (Collected (Row)) = "frame.latency",
             "A unchanged by B");
      Row := Find (Pid_B, Tag_B, 1);
      Check (Row /= 0 and then Name_Of (Collected (Row)) = "packets",
             "B row");
      Row := Find (Pid_A, Tag_A_Reused_Pid, 1);
      Check (Row /= 0 and then Name_Of (Collected (Row)) = "reused" and then
             Collected (Row) (P.Row_Count_Word) = 0, "PID reuse separate");

      --  Sequence gaps and replay.
      declare
         Sealed : Boolean;
         Page : B.Page_Id;
         Bytes : Unsigned_64;
         Saved : R.Page_Words;
         Saved_Bytes : Unsigned_64;
         Before : Unsigned_64;
      begin
         Emit (Bp, Sample (R.Counter, 1, 2, 1));
         B.Seal (Bp.State, Bp.Pages, Sealed, Page, Bytes);
         Saved := Bp.Pages (Page);
         Saved_Bytes := Bytes;
         B.Complete (Bp.State, Page);   --  "lost" in transit
         Emit (Bp, Sample (R.Counter, 1, 3, 1));
         Flush (Bp, Outcome);
         Collect_All;
         Row := Find (Pid_B, Tag_B, 1);
         Check (Row /= 0 and then
                Collected (Row) (P.Row_Source_Batch_Gaps) = 1, "gap counted");
         Before := Collected (Row) (P.Row_Total);
         Metric_Store.Ingest (Store, Pid_B, Tag_B, Saved, Saved_Bytes,
                              Outcome);
         Check (Outcome.Result = P.Invalid_Request and Outcome.Accepted = 0,
                "late replay rejected");
         Collect_All;
         Row := Find (Pid_B, Tag_B, 1);
         Check (Row /= 0 and then Collected (Row) (P.Row_Total) = Before,
                "replay changed nothing");
         --  Same batch from another identity counts only for that identity.
         Metric_Store.Ingest (Store, Pid_B, Tag_A, Saved, Saved_Bytes,
                              Outcome);
         Check (Outcome.Result = P.OK and Outcome.Rejected = 1,
                "foreign identity cannot write B's series");
      end;
   end Test_Store;

   procedure Test_Capacity is
      Outcome : Metric_Store.Ingest_Outcome;
      Item : Producer;
      Base : constant Natural := 100;
   begin
      --  Fill remaining source slots (4 used: A, B, reused, foreign).
      for I in 1 .. Metric_Store.Maximum_Sources - 4 loop
         Item := (Pid => Unsigned_64 (Base + I),
                  Tag => P.Publisher_Tag (Unsigned_64 (Base + I)),
                  others => <>);
         for K in R.Metric_Key loop
            Emit (Item, Describe (K, R.Counter, R.Count, "k"));
         end loop;
         Flush (Item, Outcome);
         Check (Outcome.Result = P.OK, "fill sources");
      end loop;
      Item := (Pid => 999, Tag => P.Publisher_Tag (999), others => <>);
      Emit (Item, Describe (1, R.Counter, R.Count, "late"));
      Flush (Item, Outcome);
      Check (Outcome.Result = P.Exhausted and Outcome.Accepted = 0,
             "source table exhausted");
      Collect_All;
      Check (Collected_Count > P.Rows_Per_Page * 2, "multi-page summaries");
      Check (Find (999, P.Publisher_Tag (999), 1) = 0, "no row when full");
      --  After the lease every source is idle; the LRU slot is reused.
      Metric_Store.Advance_Time (Store, Metric_Store.Source_Lease_Ms);
      Item := (Pid => 999, Tag => P.Publisher_Tag (999), others => <>);
      Emit (Item, Describe (1, R.Counter, R.Count, "late"));
      Flush (Item, Outcome);
      Check (Outcome.Result = P.OK, "evicts idle source after lease");
      Collect_All;
      Check (Find (999, P.Publisher_Tag (999), 1) /= 0, "new source row");
   end Test_Capacity;

   procedure Test_Authority is
      Log_Observer : constant Unsigned_64 :=
        CuBit.Log_Protocol.Observer_Authority_Tag;
   begin
      Check (P.May_Invoke (P.Publisher_Tag (5), P.Publish_Batch),
             "publisher publishes");
      Check (not P.May_Invoke (P.Publisher_Tag (5), P.Query_Summaries),
             "publisher cannot observe");
      Check (P.May_Invoke (P.Observer_Tag (5), P.Query_Summaries),
             "observer queries");
      Check (not P.May_Invoke (P.Observer_Tag (5), P.Publish_Batch),
             "observer cannot publish");
      Check (not P.May_Invoke (0, P.Publish_Batch) and
             not P.May_Invoke (0, P.Query_Summaries),
             "generic endpoint (tag 0) denied");
      Check (not P.May_Invoke (P.Publisher_Tag_Base, P.Publish_Batch) and
             not P.May_Invoke (P.Observer_Tag_Base, P.Query_Summaries),
             "zero issuance denied");
      Check (not P.May_Invoke (Log_Observer, P.Query_Summaries),
             "log observer is not metrics observer");
   end Test_Authority;

   procedure Test_Histogram is
      H : Metric_Histograms.Histogram := Metric_Histograms.Empty;
   begin
      Check (Metric_Histograms.Quantile_Upper (H, 500) = 0, "empty");
      Metric_Histograms.Add (H, 0);
      Metric_Histograms.Add (H, Unsigned_64'Last);
      Check (Metric_Histograms.Minimum (H) = 0 and
             Metric_Histograms.Maximum (H) = Unsigned_64'Last, "extremes");
      Check (Metric_Histograms.Quantile_Upper (H, 500) = 0 and
             Metric_Histograms.Quantile_Upper (H, 1000) = Unsigned_64'Last,
             "extreme quantiles");
      for I in Metric_Histograms.Bucket_Index'First + 1 ..
        Metric_Histograms.Bucket_Index'Last
      loop
         Check (Metric_Histograms.Upper_Bound (I) >=
                Metric_Histograms.Upper_Bound (I - 1), "monotone bounds");
      end loop;
   end Test_Histogram;
begin
   Test_Codec;
   Test_Header;
   Test_Batches;
   Test_Store;
   Test_Capacity;
   Test_Authority;
   Test_Histogram;
   Put_Line ("metrics:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      raise Program_Error with "metrics tests failed";
   end if;
end Main;
