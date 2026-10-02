pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with CuBit.Metrics;
with CCL_Manifest_Bindings;

--  Native acceptance check for metrics.svc: publish typed latency, counter
--  and span metrics through the batched asynchronous client, query the
--  aggregated summaries through the separate observer authority, and check
--  that publisher authority cannot observe and malformed batches are refused.
procedure Main is
   package R renames CuBit.Metric_Records;
   package P renames CuBit.Metric_Protocol;
   use type P.Status;

   Publisher_Slot : constant CapabilitySlot :=
     CapabilitySlot (CCL_Manifest_Bindings.Slot_metrics);
   Observer_Slot : constant CapabilitySlot :=
     CapabilitySlot (CCL_Manifest_Bindings.Slot_metrics_observer);
   Writer : CuBit.Metrics.Publisher (Publisher_Slot);
   Watcher : CuBit.Metrics.Observer (Observer_Slot);

   Latency_Key : constant R.Metric_Key := 1;
   Frames_Key : constant R.Metric_Key := 2;
   Span_Key : constant R.Metric_Key := 3;
   Samples : constant := 1_000;
   Frame_Increment : constant := 240;
   Span_Start_Us : constant := 5_000;
   Span_Length_Us : constant := 2_500;
   Wait_Ms : constant Unsigned_64 := 2_000;
   Expected_P50 : constant Unsigned_64 := 512;
   Expected_P99 : constant Unsigned_64 := 1_024;

   Token : Unsigned_64 := 1;
   Ignore : Unsigned_64;
   Accepted_Records : Unsigned_64 := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL metrics: " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop
            Ignore := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      end if;
   end Check;

   --  A real producer forwards completions from its event loop; this test
   --  waits for each one to observe the outcome.
   procedure Flush_And_Wait is
      Submitted, Handled : Boolean;
      Completion : CompletionEntry;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + Wait_Ms;
      Activity : Activity_Result;
   begin
      CuBit.Metrics.Flush (Writer, Token, Submitted);
      Check (Submitted, "batch submitted");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            CuBit.Metrics.Complete (Writer, Completion, Handled);
            Check (Handled, "completion correlation");
            Check (Completion.msg.tag.label = P.Status'Enum_Rep (P.OK),
                   "batch accepted");
            Accepted_Records := Accepted_Records + Completion.msg.words (0);
            exit;
         end if;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "completion timeout");
         Activity := Wait_For_Activity_Until (Deadline);
         Check (Activity /= Unavailable, "completion wait available");
      end loop;
      Token := Token + 1;
   end Flush_And_Wait;

   procedure Put (Value : R.Metric_Record) is
      Accepted : Boolean;
   begin
      if not CuBit.Metrics.Has_Room (Writer) then
         Flush_And_Wait;
      end if;
      CuBit.Metrics.Put (Writer, Value, Accepted);
      Check (Accepted, "append");
   end Put;

   function Row_For (Rows : P.Summary_Page; Written : P.Row_Count;
                     Key : R.Metric_Key) return Integer is
   begin
      for I in 0 .. Written - 1 loop
         if Rows (I) (P.Row_Key) = Unsigned_64 (Key) then
            return I;
         end if;
      end loop;
      return -1;
   end Row_For;

   procedure Put_Decimal (Label : String; Value : Unsigned_64) is
      Digits_Text : String (1 .. 20);
      Remaining : Unsigned_64 := Value;
      First : Positive := Digits_Text'Last + 1;
   begin
      loop
         First := First - 1;
         Digits_Text (First) :=
           Character'Val (Character'Pos ('0') + Natural (Remaining mod 10));
         Remaining := Remaining / 10;
         exit when Remaining = 0;
      end loop;
      debugPrint (" " & Label & "=" & Digits_Text (First .. Digits_Text'Last));
   end Put_Decimal;

   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next : Unsigned_64;
   Result : P.Status;
   Row : Integer;
   Msg : Message;
   Tag : MessageTag;
begin
   Put ((Kind => R.Describe, Key => Latency_Key, Declared => R.Latency,
         Measure => R.Microseconds,
         Name => R.To_Name ("check.frame.latency")));
   Put ((Kind => R.Describe, Key => Frames_Key, Declared => R.Counter,
         Measure => R.Count, Name => R.To_Name ("check.frames")));
   Put ((Kind => R.Describe, Key => Span_Key, Declared => R.Span,
         Measure => R.Microseconds,
         Name => R.To_Name ("check.input.to.present")));
   for V in 1 .. Samples loop
      Put ((Kind => R.Latency, Key => Latency_Key, Time_Us => Unsigned_64 (V),
            Value => Unsigned_64 (V), Correlation => Unsigned_64 (V)));
   end loop;
   Put ((Kind => R.Counter, Key => Frames_Key, Time_Us => 1,
         Value => Frame_Increment, Correlation => 0));
   Put ((Kind => R.Span, Key => Span_Key, Start_Us => Span_Start_Us,
         End_Us => Span_Start_Us + Span_Length_Us, Span_Correlation => 7));
   Flush_And_Wait;
   Check (Accepted_Records = Samples + 5, "every record accepted");
   Check (CuBit.Metrics.Dropped (Writer) = 0, "no local drops");
   Check (CuBit.Metrics.Rejected (Writer) = 0, "no service rejections");

   CuBit.Metrics.Query (Watcher, 0, Rows, Written, Next, Result);
   Check (Result = P.OK, "observer query");
   Check (Written = 3, "three series visible");
   Row := Row_For (Rows, Written, Latency_Key);
   Check (Row >= 0, "latency row");
   Check (Rows (Row) (P.Row_Count_Word) = Samples and
          Rows (Row) (P.Row_Minimum) = 1 and
          Rows (Row) (P.Row_Maximum) = Samples, "latency count/min/max");
   Check (Rows (Row) (P.Row_P50) = Expected_P50 and
          Rows (Row) (P.Row_P99) = Expected_P99, "latency percentiles");
   debugPrint ("metrics: check.frame.latency");
   Put_Decimal ("count", Rows (Row) (P.Row_Count_Word));
   Put_Decimal ("p50_us<=", Rows (Row) (P.Row_P50));
   Put_Decimal ("p99_us<=", Rows (Row) (P.Row_P99));
   Put_Decimal ("max_us", Rows (Row) (P.Row_Maximum));
   debugPrint ("" & ASCII.LF);
   Row := Row_For (Rows, Written, Frames_Key);
   Check (Row >= 0 and then Rows (Row) (P.Row_Total) = Frame_Increment,
          "counter total");
   Row := Row_For (Rows, Written, Span_Key);
   Check (Row >= 0 and then Rows (Row) (P.Row_Maximum) = Span_Length_Us,
          "span duration");

   --  Publisher authority cannot observe (tags are kernel-stamped).
   Msg := NULL_MESSAGE;
   Msg.tag := (P.Operation'Enum_Rep (P.Query_Summaries), P.Message_Words,
               0, 0);
   Msg.authorityTag := P.Observer_Tag (1);
   Tag := capCall (Publisher_Slot, Msg);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Denied),
          "publisher cannot query");
   --  Malformed batch length is refused before any grant acquisition.
   Msg := NULL_MESSAGE;
   Msg.tag := (P.Operation'Enum_Rep (P.Publish_Batch), P.Message_Words,
               0, 0);
   Msg.words := [0, 1, R.Slot_Bytes + 1, 0];
   Tag := capCall (Publisher_Slot, Msg);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "malformed batch refused");
   --  Observer authority cannot publish.
   Msg.tag := (P.Operation'Enum_Rep (P.Publish_Batch), P.Message_Words,
               0, 0);
   Msg.words := [0, 1, 2 * R.Slot_Bytes, 0];
   Tag := capCall (Observer_Slot, Msg);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Denied),
          "observer cannot publish");

   debugPrint ("TEST: PASS metrics" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
