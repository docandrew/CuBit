with Compositor_Trace_Metrics;
package Compositor_Trace_Stream with Pure, SPARK_Mode is
   package M renames Compositor_Trace_Metrics;
   package W renames M.W;
   package R renames M.R;
   package P renames M.P;
   use type W.Word;
   subtype Partial_Count is Natural range 0 .. 3;
   type Statistics is record
      Skipped_Rows, Rejected_Rows, Abandoned_Events, Emitted_Events : W.Word := 0;
      Endpoint_Mismatches : W.Word := 0;
   end record;
   --  Fields other than Success are meaningful only for a complete event.
   --  A fixed record permits safe out-parameter use by any caller.
   type Capture is record
      Success : Boolean := False;
      Incarnation, Pid, Publisher, Batch, First_Sequence : W.Word := 0;
      Producer_Dropped, Batch_Gaps : W.Word := 0;
      Value : W.Event;
   end record;
   type State is private;
   function Owner (S : State) return W.Word;
   function Cursor (S : State) return W.Word;
   function Pending (S : State) return Partial_Count;
   function Counts (S : State) return Statistics;
   function Increment (Value : W.Word) return W.Word is
     (if Value = W.Word'Last then Value else Value + 1);
   --  A new capture has an explicit endpoint and requested history cursor.
   --  Archive old counters before Start: it intentionally starts a new capture.
   procedure Start (S : out State; Incarnation, Requested_Cursor : W.Word)
     with Pre => Incarnation /= 0 and then Requested_Cursor /= 0,
       Post => Owner (S) = Incarnation and then Cursor (S) = Requested_Cursor and then
         Pending (S) = 0 and then Counts (S) = (0, 0, 0, 0, 0);
   --  A caller feeds validated raw-query rows, in order. Defensive checks
   --  still reject malformed input. Incarnation comes from the raw observer,
   --  never the payload. Feed makes no calls, allocations or clock reads.
   --  No automatic endpoint switching: Start is required after reconnection.
   procedure Feed
     (S : in out State; Incarnation : W.Word; Row : P.Raw_Row;
      Result : out Capture)
     with Post => Owner (S) = Owner (S'Old) and then Cursor (S) >= Cursor (S'Old) and then
       Counts (S).Emitted_Events =
         (if Result.Success then Increment (Counts (S'Old).Emitted_Events)
          else Counts (S'Old).Emitted_Events) and then
       (if Result.Success then
          W.Valid (Result.Value) and then Result.Incarnation = Incarnation and then
          Incarnation /= 0 and then Incarnation = Owner (S) and then
          Result.Pid = Row (1) and then Result.Pid /= 0 and then
          Result.Publisher = Row (2) and then P.Is_Publisher (Result.Publisher) and then
          Result.Batch = Row (3) and then Result.Batch /= 0 and then
          Result.First_Sequence in 1 .. W.Word'Last - 3 and then
          Result.First_Sequence + 3 = Row (0) and then Pending (S) = 0);
   --  Use on capture end or a failed query; never keep half an event alive
   --  across a transport failure. Counters and cursor remain available.
   procedure Discard_Partial (S : in out State)
     with Post => Pending (S) = 0 and then Owner (S) = Owner (S'Old) and then
       Cursor (S) = Cursor (S'Old) and then
       Counts (S).Emitted_Events = Counts (S'Old).Emitted_Events and then
       Counts (S).Abandoned_Events =
         (if Pending (S'Old) = 0 then Counts (S'Old).Abandoned_Events
          else Increment (Counts (S'Old).Abandoned_Events));
private
   type State is record
      Endpoint : W.Word := 0;
      Next : W.Word := 1;
      Used : Partial_Count := 0;
      Rows : M.Group;
      Drops, Gaps : W.Word := 0;
      Stats : Statistics;
   end record;
   function Owner (S : State) return W.Word is (S.Endpoint);
   function Cursor (S : State) return W.Word is (S.Next);
   function Pending (S : State) return Partial_Count is (S.Used);
   function Counts (S : State) return Statistics is (S.Stats);
end Compositor_Trace_Stream;
