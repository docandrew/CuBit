pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with CuBit.Metrics;
with CuBit.Metric_Raw_Observer;
with CCL_Manifest_Bindings;
with Compositor_Trace_Metrics;
with Compositor_Trace_Wire;
with Compositor_Trace_Stream;

--  Native acceptance check for metrics.svc: publish typed latency, counter
--  and span metrics through the batched asynchronous client, query the
--  aggregated summaries through the separate observer authority, and check
--  that publisher authority cannot observe and malformed batches are refused.
procedure Main is
   package R renames CuBit.Metric_Records;
   package P renames CuBit.Metric_Protocol;
   use type P.Status;
   use type R.Slot_Words;

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
   Tag := capCall (Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Denied),
          "publisher cannot query");
   --  Malformed batch length is refused before any grant acquisition.
   Msg := NULL_MESSAGE;
   Msg.tag := (P.Operation'Enum_Rep (P.Publish_Batch), P.Message_Words,
               0, 0);
   Msg.words := [0, 1, R.Slot_Bytes + 1, 0];
   Tag := capCall (Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "malformed batch refused");
   --  Observer authority cannot publish.
   Msg.tag := (P.Operation'Enum_Rep (P.Publish_Batch), P.Message_Words,
               0, 0);
   Msg.words := [0, 1, 2 * R.Slot_Bytes, 0];
   Tag := capCall (Observer_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label /= 0 and then
          Msg.tag.label = P.Status'Enum_Rep (P.Denied),
          "observer cannot publish");

   declare
      Raw : CuBit.Metric_Raw_Observer.Observer (Observer_Slot);
      Page : P.Raw_Page;
      N : P.Raw_Row_Count;
      Cursor : Unsigned_64 := 1;
      Resume, Gap, Lost, Total : Unsigned_64 := 0;
      Encoded, Expected : R.Slot_Words;
      Sequence, V : Unsigned_64;
      Done : Boolean;
      Deadline : Unsigned_64;
   begin
      loop
         CuBit.Metric_Raw_Observer.Query (Raw, Cursor, Page, N, Resume, Gap, Lost, Result);
         Check (Result = P.OK and N = 32, "raw page");
         Check (Gap = (if Cursor = 1 then 749 else 0), "raw overwrite gap");
         Check (Lost = 0, "raw sequence not exhausted");
         for I in 0 .. N - 1 loop
            Sequence := Page (I) (0);
            Check (Page (I) (1) = syscall (SYSCALL_GETPID) and
              P.Is_Publisher (Page (I) (2)), "authenticated raw identity");
            Check (Page (I) (4) = 0 and Page (I) (5) = 0, "raw producer loss");
            for W in R.Slot_Word_Index loop Encoded (W) := Page (I) (8 + W); end loop;
            if Sequence <= 1003 then
               V := Sequence - 3;
               Expected := R.Encode ((R.Latency, Latency_Key, V, V, V));
            elsif Sequence = 1004 then
               Expected := R.Encode ((R.Counter, Frames_Key, 1, Frame_Increment, 0));
            else
               Check (Sequence = 1005, "last raw sequence");
               Expected := R.Encode ((R.Span, Span_Key, Span_Start_Us,
                 Span_Start_Us + Span_Length_Us, 7));
            end if;
            Check (Encoded = Expected, "exact raw payload");
         end loop;
         Total := Total + Unsigned_64 (N); Cursor := Resume;
         exit when Total = 256;
         Check (Total < 256, "bounded raw capture");
      end loop;
      Check (Cursor = 1006 and Is_Process (CuBit.Metric_Raw_Observer.Incarnation (Raw)),
        "raw cursor and incarnation");
      CuBit.Metric_Raw_Observer.Query (Raw, Cursor, Page, N, Resume, Gap, Lost, Result);
      Check (Result = P.OK and N = 0 and Gap = 0 and Resume = Cursor, "empty raw tail");
      Deadline := syscall (SYSCALL_GETTIME) + Wait_Ms;
      loop
         CuBit.Metric_Raw_Observer.Disconnect (Raw, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "raw grant retired");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Msg := NULL_MESSAGE;
      Msg.tag := (P.Operation'Enum_Rep (P.Query_Raw), P.Message_Words, 0, 0);
      Msg.authorityTag := P.Observer_Tag (1);
      Tag := capCall (Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
      Check (Tag.label = P.Status'Enum_Rep (P.Denied), "publisher cannot query raw");
      debugPrint ("TEST: PASS raw-metrics 256 exact records gap749 retirement" & ASCII.LF);
   end;

   declare
      package TM renames Compositor_Trace_Metrics;
      package TW renames Compositor_Trace_Wire;
      use type TW.Event;
      use type R.Record_Kind;
      Raw : CuBit.Metric_Raw_Observer.Observer (Observer_Slot);
      Raw_Page : P.Raw_Page;
      N : P.Raw_Row_Count;
      Cursor : Unsigned_64 := 1006;
      Resume, Gap, Lost, Total : Unsigned_64 := 0;
      package TS renames Compositor_Trace_Stream;
      Collector : TS.State;
      Captured : TS.Capture;
      Started : Boolean := False;
      Seen : Natural := 0;
      Done, Submitted, Handled, Accepted : Boolean;
      Deadline : Unsigned_64;
      Completion : CompletionEntry;
      Activity : Activity_Result;
      Number : Natural;
      function Expected_Event (Index : Natural) return TW.Event is
         V : constant Unsigned_64 := Unsigned_64 (Index);
         ID : constant Unsigned_64 := Unsigned_64'Last - V;
      begin
         case Index mod 5 is
            when 0 => return (TW.Input_Event, ID, (Unsigned_64'Last, V, 1, V));
            when 1 => return (TW.Source_Event, ID,
              (Unsigned_64'Last, Unsigned_64'Last - 1, V, V, V));
            when 2 => return (TW.Render_Event, ID,
              (TW.RT.Draw, 1, 3, Unsigned_64'Last, V,
               Unsigned_64'Last - 2, Unsigned_64'Last - 3, V, 0, 0, V));
            when 3 => return (TW.Render_Event, ID,
              (TW.RT.Submit, 1, 3, Unsigned_64'Last, V, 0, 0, 0, 77, V, V));
            when others => return (TW.Frame_Event, ID, (1, 77, V, V, V + 1));
         end case;
      end Expected_Event;
      procedure Put_Event (Index : Natural) is
      begin
         if not CuBit.Metrics.Has_Group_Room (Writer) then Flush_And_Wait; end if;
         CuBit.Metrics.Put_Group (Writer, TM.Fragment (Expected_Event (Index)), Accepted);
         Check (Accepted, "complete trace event accepted");
      end Put_Event;
   begin
      for I in 1 .. 100 loop Put_Event (I); end loop;
      Flush_And_Wait;
      --  Do not dispatch either completion yet: both SDK pages stay busy.
      for Page_Index in 0 .. 1 loop
         for I in 1 .. 15 loop
            CuBit.Metrics.Put_Group
              (Writer, TM.Fragment (Expected_Event (101 + Page_Index * 15 + I - 1)), Accepted);
            Check (Accepted, "fill retained trace page");
         end loop;
         CuBit.Metrics.Flush (Writer, Token, Submitted);
         Check (Submitted, "two trace pages submitted");
         Token := Token + 1;
      end loop;
      Check (not CuBit.Metrics.Has_Group_Room (Writer), "both trace pages held");
      for I in 1 .. 100 loop
         CuBit.Metrics.Put_Group (Writer, TM.Fragment (Expected_Event (999)), Accepted);
         Check (not Accepted, "overload refuses complete event");
      end loop;
      Check (CuBit.Metrics.Dropped (Writer) = 400, "four fragments counted per drop");
      Deadline := syscall (SYSCALL_GETTIME) + Wait_Ms;
      for I in 1 .. 2 loop
         loop
            if Poll_Completion (Completion'Address) = 1 then
               CuBit.Metrics.Complete (Writer, Completion, Handled);
               Check (Handled and Completion.status = COMPLETION_OK and
                 Completion.msg.tag.label = P.Status'Enum_Rep (P.OK) and
                 Completion.msg.words (0) = 60 and Completion.msg.words (1) = 0,
                 "held trace page accepted intact");
               Accepted_Records := Accepted_Records + Completion.msg.words (0);
               exit;
            end if;
            Check (syscall (SYSCALL_GETTIME) < Deadline, "held trace completion timeout");
            Activity := Wait_For_Activity_Until (Deadline);
            Check (Activity /= Unavailable, "held trace wait available");
         end loop;
      end loop;
      Put_Event (131); Flush_And_Wait;
      --  One ordinary metric shifts retained history to trace fragment 1.
      Put ((R.Counter, Frames_Key, 1, 0, 0)); Flush_And_Wait;
      Check (Accepted_Records = 1530 and CuBit.Metrics.Rejected (Writer) = 0,
        "all admitted fragments accepted");
      loop
         CuBit.Metric_Raw_Observer.Query
           (Raw, Cursor, Raw_Page, N, Resume, Gap, Lost, Result);
         Check (Result = P.OK and N = 32 and Lost = 0, "trace raw page");
         Check (Gap = (if Cursor = 1006 then 269 else 0), "trace overwrite gap");
         if not Started then
            TS.Start (Collector, To_Word (CuBit.Metric_Raw_Observer.Incarnation (Raw)), Cursor);
            Started := True;
         end if;
         for I in 0 .. N - 1 loop
            TS.Feed (Collector, To_Word (CuBit.Metric_Raw_Observer.Incarnation (Raw)),
                     Raw_Page (I), Captured);
            if Captured.Success then
               Number := 69 + Seen;
               Check (Captured.Value = Expected_Event (Number), "exact streaming native event");
               Check (Captured.Pid = syscall (SYSCALL_GETPID) and
                 P.Is_Publisher (Captured.Publisher), "stream publisher identity");
               Check (Captured.Producer_Dropped = (if Number = 131 then 400 else 0) and
                 Captured.Batch_Gaps = 0, "stream producer loss metadata");
               Seen := Seen + 1;
            end if;
         end loop;
         Total := Total + Unsigned_64 (N); Cursor := Resume;
         exit when Total = 256;
         Check (Total < 256, "bounded native trace capture");
      end loop;
      Check (Cursor = 1531 and TS.Cursor (Collector) = Cursor, "stream final cursor");
      Check (Seen = 63 and TS.Pending (Collector) = 0 and
        TS.Counts (Collector).Skipped_Rows = 269 and
        TS.Counts (Collector).Rejected_Rows = 3 and
        TS.Counts (Collector).Emitted_Events = 63 and
        TS.Counts (Collector).Abandoned_Events = 0, "partial history and page recovery");
      CuBit.Metrics.Query (Watcher, 0, Rows, Written, Next, Result);
      Row := Row_For (Rows, Written, Latency_Key);
      Check (Result = P.OK and then Written = 3 and then Row >= 0 and then
        Rows (Row) (P.Row_Count_Word) = Samples, "trace leaves metric summaries unchanged");
      Deadline := syscall (SYSCALL_GETTIME) + Wait_Ms;
      loop
         CuBit.Metric_Raw_Observer.Disconnect (Raw, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "trace reader grant retirement");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Deadline := syscall (SYSCALL_GETTIME) + Wait_Ms;
      loop
         CuBit.Metrics.Disconnect (Writer, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "trace publisher grant retirement");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      debugPrint ("TEST: PASS trace-stream 63 exact events gap269 orphans3 drops400 retirement" & ASCII.LF);
   end;

   debugPrint ("TEST: PASS metrics" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
