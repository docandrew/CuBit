with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with Log_Fanout;
with Log_Budgets;
with CuBit.Grant_References;
with CuBit.Authority_Policy; use CuBit.Authority_Policy;
with CuBit.Log_Records;
with CuBit.Log_Streams;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
procedure Main is
   Store : Log_Fanout.Broker;
   First, Second, Other, Replacement, Lost : Unsigned_64;
   Result : Status;
   Value : Event;
   Expected : Event :=
     (Source => 30, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
      Monotonic_Ms => 123, Data => CuBit.Log_Records.Empty_Record);
   Empty_Value : constant Event := (others => <>);
   procedure Read (Caller, Handle : Unsigned_64) is
   begin
      Log_Fanout.Read_Next
        (Store, Caller, Observer_Authority_Tag, Handle, Value, Lost, Result);
   end Read;
begin
   declare
      use CuBit.Grant_References;
      Ref : constant Reference := (slot => 48, generation => 10);
   begin
      pragma Assert (Retirement_Confirmed (Ref, 0));
      pragma Assert (not Retirement_Confirmed (Ref, 9));
      pragma Assert (not Retirement_Confirmed (Ref, 10));
      pragma Assert (Retirement_Confirmed (Ref, 11));
      pragma Assert (Retirement_Confirmed (Ref, Maximum_Generation));
      pragma Assert (not Retirement_Confirmed (Ref, Maximum_Generation + 1));
      pragma Assert (not Retirement_Confirmed (Ref, Unsigned_64'Last));
      Put_Line ("PASS: retirement distinguishes inactive, live/revoking, newer generations and query failures");
   end;
   declare
      Limits, Sustained : Log_Budgets.Limiter;
      Accepted : Boolean;
      Total : Natural := 0;
   begin
      pragma Assert (not Is_Publisher (Publisher_Tag_Base + 1));
      for Pool in Budget_Id loop
         for Serial in Publication_Issuance range 1 .. 100 loop
            pragma Assert (Is_Publisher (Publisher_Tag (Pool, Serial)));
            pragma Assert
              (Publication_Budget (Publisher_Tag (Pool, Serial)) = Pool);
         end loop;
         pragma Assert
           (Publication_Budget
              (Publisher_Tag (Pool, Publication_Issuance'Last)) = Pool);
      end loop;
      Log_Budgets.Advance_Time (Limits, 1000);
      --  Distinct launch/handle issuances still spend the same pool.
      for Serial in 1 .. Log_Budgets.Burst loop
         Log_Budgets.Admit
           (Limits, Publication_Budget
              (Publisher_Tag (Bootstrap_Budget, Unsigned_64 (Serial))), Accepted);
         pragma Assert (Accepted);
      end loop;
      Log_Budgets.Admit
        (Limits, Publication_Budget (Publisher_Tag (Bootstrap_Budget, 999)), Accepted);
      pragma Assert (not Accepted);
      pragma Assert (Log_Budgets.Rejected (Limits, Bootstrap_Budget) = 1);
      Log_Budgets.Admit (Limits, 2, Accepted);
      pragma Assert (Accepted);
      Log_Budgets.Advance_Time (Limits, 1099);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 0);
      Log_Budgets.Advance_Time (Limits, 1100);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 1);
      Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
      pragma Assert (Accepted);
      Log_Budgets.Advance_Time (Limits, 1050);
      Log_Budgets.Advance_Time (Limits, 1199);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 0);
      Log_Budgets.Advance_Time (Limits, Unsigned_64'Last);
      pragma Assert
        (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = Log_Budgets.Burst);
      for I in 1 .. Log_Budgets.Burst loop
         Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
         pragma Assert (Accepted);
      end loop;
      Log_Budgets.Advance_Time (Limits, 0);
      Log_Budgets.Advance_Time (Limits, Unsigned_64'Last);
      Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
      pragma Assert (not Accepted);
      --  Dense arrivals cannot replenish on each request or save credit past
      --  the burst cap. Each admitted attempt costs one, regardless of payload.
      for Tick in 0 .. 10_000 loop
         Log_Budgets.Advance_Time (Sustained, Unsigned_64 (Tick));
         Log_Budgets.Admit (Sustained, Bootstrap_Budget, Accepted);
         if Accepted then Total := Total + 1; end if;
         pragma Assert
           (Total <= Log_Budgets.Burst + Tick / Natural (Log_Budgets.Refill_Ms));
      end loop;
      pragma Assert (Total = Log_Budgets.Burst + 100);
      Put_Line ("PASS: shared producer budgets, copies, isolated pools, refill, backward/max time and sustained-rate bound");
   end;
   for Requested in Boolean loop
      for Installation in Boolean loop
         for Session in Boolean loop
            for Issuer in Boolean loop
               pragma Assert
                 ((Evaluate (Requested, Installation, Session, Issuer) = Approved) =
                  (Requested and Installation and Session and Issuer));
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (May_Invoke (Publisher_Authority_Tag, Publish));
   pragma Assert (not Is_Observer (Observer_Tag_Base));
   pragma Assert (not Is_Observer (Unsigned_64'Last));
   pragma Assert (Is_Observer (Observer_Tag_Base + Unsigned_64 (Unsigned_32'Last)));
   for Op in Subscribe .. Close loop
      pragma Assert (not May_Invoke (Publisher_Authority_Tag, Op));
      pragma Assert (May_Invoke (Observer_Authority_Tag, Op));
   end loop;
   pragma Assert (not May_Invoke (Observer_Authority_Tag, Publish));
   Log_Fanout.Publish (Store, Expected);
   Log_Fanout.Subscribe (Store, 30, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 0, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK and First /= 0);
   Log_Fanout.Subscribe (Store, 20, Observer_Authority_Tag, Second, Result);
   pragma Assert (Result = OK and Second /= First);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other = First);
   --  Recycled PID with a different launch-issued observer tag cannot inherit
   --  the previous instance's subscription, even knowing its handle.
   Log_Fanout.Read_Next
     (Store, 10, Observer_Authority_Tag + 1, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, 10, Observer_Authority_Tag + 1, First, Result);
   pragma Assert (Result = Denied);
   --  Knowledge of another subscriber's ID does not authorize use or close.
   Read (20, First);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, 20, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = Denied);
   --  Even the owner must invoke the observer authority, not its publisher one.
   Log_Fanout.Read_Next
     (Store, 10, Publisher_Authority_Tag, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Read (10, First); pragma Assert (Result = OK and Value = Expected);
   Read (20, Second); pragma Assert (Result = OK and Value = Expected);
   Read (10, First); pragma Assert (Result = Empty and Value = Empty_Value);
   --  One slow recipient does not advance another recipient's cursor.
   for I in 1 .. Log_Fanout.Capacity + 3 loop
      Expected.Monotonic_Ms := Unsigned_64 (I);
      Log_Fanout.Publish (Store, Expected);
      Read (10, First); pragma Assert (Result = OK and Value = Expected);
   end loop;
   Read (20, Second); pragma Assert (Result = Gap and Lost = 3 and Value = Empty_Value);
   for I in 4 .. Log_Fanout.Capacity + 3 loop
      Read (20, Second); pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
   end loop;
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Close (Store, 10, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First and Other /= Second);
   Read (10, First); pragma Assert (Result = Denied);
   Log_Fanout.Close (Store, 10, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied);
   -- The initial publication plus Capacity+3 later records discarded four
   -- entries from boot history, even though the active reader kept up.
   Read (10, Other); pragma Assert (Result = Gap and Lost = 4 and Value = Empty_Value);
   Read (10, Other); pragma Assert (Result = OK and Value.Monotonic_Ms = 4);
   Replacement := Other;
   --  Capacity rejection never returns a token or permits an unprivileged
   --  caller to distinguish capacity exhaustion from any other denial.
   for I in 3 .. Log_Fanout.Maximum_Subscribers loop
      Log_Fanout.Subscribe (Store, Unsigned_64 (I + 100), Observer_Authority_Tag, Other, Result);
      pragma Assert (Result = OK);
   end loop;
   Log_Fanout.Subscribe (Store, 99, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 99, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Exhausted and Other = 0);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms - 1);
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms);
   Read (10, Replacement); pragma Assert (Result = Denied);
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Subscribe (Store, 99, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First);
   Log_Fanout.Advance_Time (Store, Unsigned_64'Last);
   Read (99, Other); pragma Assert (Result = Denied);
   --  A reader's stream: events and gaps through a Datagram_Rings ring and
   --  back, across wrap-around; malformed entries are refused.
   declare
      package S renames CuBit.Log_Streams;
      package R renames CuBit.Channel_Rings;
      package D renames CuBit.Datagram_Rings;
      use type D.Put_Result;
      use type D.Take_Result;
      use type S.Entry_Kind;
      use type R.Count;
      Ring : R.Bytes (0 .. S.RING_BYTES - 1) := [others => 0];
      P : R.Producer := R.New_Producer (S.RING_BYTES);
      C : R.Consumer := R.New_Consumer (S.RING_BYTES);
      Entry_Bytes, Got : S.Entry_Buffer;
      Length : S.Entry_Length;
      Taken_Length : Natural;
      Truncated, Valid, Accepted : Boolean;
      Put : D.Put_Result;
      Take : D.Take_Result;
      Kind : S.Entry_Kind;
      Back : Event;
      Gap_Count : Unsigned_64;
      Sent : Event :=
        (Source => 41, Node => (High => 16#0123_4567_89AB_CDEF#, Low => 16#FEDC_BA98_7654_3210#),
         Publication_Tag => Publisher_Authority_Tag, Monotonic_Ms => 0,
         Data => CuBit.Log_Records.Make ("stream record", CuBit.Log_Records.Warning).Value);
   begin
      for Round in 1 .. 3_000 loop
         Sent.Monotonic_Ms := Unsigned_64 (Round);
         if Round mod 97 = 0 then
            S.Encode_Gap (Unsigned_64 (Round), Entry_Bytes, Length);
         else
            S.Encode_Event (Sent, Entry_Bytes, Length);
         end if;
         D.Put (P, Ring, Entry_Bytes (0 .. Length - 1), Put);
         pragma Assert (Put = D.Put);
         R.Accept_Produced (C, P.Produced, Accepted);
         pragma Assert (Accepted);
         D.Take (C, Ring, Got, Taken_Length, Truncated, Take);
         pragma Assert (Take = D.Taken and not Truncated and Taken_Length = Length);
         R.Accept_Consumed (P, C.Consumed, Accepted);
         pragma Assert (Accepted);
         S.Decode (Got, Taken_Length, Kind, Back, Gap_Count, Valid);
         pragma Assert (Valid);
         if Round mod 97 = 0 then
            pragma Assert (Kind = S.Gap_Entry and Gap_Count = Unsigned_64 (Round));
         else
            pragma Assert (Kind = S.Event_Entry and Back = Sent and Gap_Count = 0);
         end if;
      end loop;
      --  Wrapped many times over: 3000 entries of about 90 bytes in 64 KiB.
      pragma Assert (R.Distance (0, P.Produced) > R.Count (4 * S.RING_BYTES));
      S.Encode_Event (Sent, Entry_Bytes, Length);
      Entry_Bytes (0) := 7;
      S.Decode (Entry_Bytes, Length, Kind, Back, Gap_Count, Valid);
      pragma Assert (not Valid and Back = Empty_Value);
      S.Encode_Event (Sent, Entry_Bytes, Length);
      S.Decode (Entry_Bytes, Length - 1, Kind, Back, Gap_Count, Valid);
      pragma Assert (not Valid);
      S.Encode_Gap (0, Entry_Bytes, Length);
      S.Decode (Entry_Bytes, Length, Kind, Back, Gap_Count, Valid);
      pragma Assert (not Valid and Gap_Count = 0);
      Put_Line ("PASS: log streams carry events and gaps through a wrapping ring; malformed entries refused");
   end;
   Put_Line ("PASS: policy matrix, log fan-out gates, independent recipients, loss, launch tags, stale handles and lease expiration");
   --  Severity-filtered subscriptions (observability agent, 2026-10-01).
   declare
      package L renames CuBit.Log_Records;
      use type L.Severity;
      Filtered : Log_Fanout.Broker;
      Low, High : Unsigned_64;
      function At_Level (Level : L.Severity; Ms : Unsigned_64) return Event is
        (Source => 30, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
         Monotonic_Ms => Ms, Data => L.Make ("x", Level).Value);
   begin
      Log_Fanout.Publish (Filtered, At_Level (L.Debug, 1));
      Log_Fanout.Publish (Filtered, At_Level (L.Error, 2));
      --  Replay of retained history honours the filter.
      Log_Fanout.Subscribe (Filtered, 50, Observer_Authority_Tag, High,
                            Result, L.Warning);
      pragma Assert (Result = OK);
      Log_Fanout.Subscribe (Filtered, 51, Observer_Authority_Tag, Low,
                            Result);
      pragma Assert (Result = OK);
      Log_Fanout.Publish (Filtered, At_Level (L.Trace, 3));
      Log_Fanout.Publish (Filtered, At_Level (L.Critical, 4));
      Log_Fanout.Read_Next (Filtered, 50, Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = OK and Value.Monotonic_Ms = 2);
      Log_Fanout.Read_Next (Filtered, 50, Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = OK and Value.Monotonic_Ms = 4);
      Log_Fanout.Read_Next (Filtered, 50, Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      for Ms in Unsigned_64'(1) .. 4 loop
         Log_Fanout.Read_Next (Filtered, 51, Observer_Authority_Tag, Low,
                               Value, Lost, Result);
         pragma Assert (Result = OK and Value.Monotonic_Ms = Ms);
      end loop;
      --  Filtered records are not loss: a narrow observer flooded with
      --  low-severity records sees no gap.
      for I in 1 .. 2 * Log_Fanout.Capacity loop
         Log_Fanout.Publish (Filtered, At_Level (L.Debug, 5));
      end loop;
      Log_Fanout.Read_Next (Filtered, 50, Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      Log_Fanout.Read_Next (Filtered, 51, Observer_Authority_Tag, Low,
                            Value, Lost, Result);
      pragma Assert (Result = Gap and Lost = Unsigned_64 (Log_Fanout.Capacity));
      --  A retry keeps the queue and narrows later publications.
      Log_Fanout.Subscribe (Filtered, 51, Observer_Authority_Tag, Other,
                            Result, L.Critical);
      pragma Assert (Result = OK and Other = Low);
      Log_Fanout.Publish (Filtered, At_Level (L.Error, 6));
      for I in 1 .. Log_Fanout.Capacity loop
         Log_Fanout.Read_Next (Filtered, 51, Observer_Authority_Tag, Low,
                               Value, Lost, Result);
         pragma Assert (Result = OK and Value.Monotonic_Ms = 5);
      end loop;
      Log_Fanout.Read_Next (Filtered, 51, Observer_Authority_Tag, Low,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      Put_Line ("PASS: severity-filtered subscriptions, filtered replay, filtering is not loss");
   end;
   --  Source-filtered subscriptions: a viewer's "recent records of service X"
   --  replays only that publisher's history and then only its new records.
   declare
      package L renames CuBit.Log_Records;
      Sourced : Log_Fanout.Broker;
      Mine : Unsigned_64;
      type Times is array (Positive range <>) of Unsigned_64;
      From_70 : constant Times := [1, 3, 5];
      From_80 : constant Times := [2, 4];
      function From (Source, Ms : Unsigned_64) return Event is
        (Source => Source, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
         Monotonic_Ms => Ms, Data => L.Make ("y").Value);
   begin
      Log_Fanout.Publish (Sourced, From (70, 1));
      Log_Fanout.Publish (Sourced, From (80, 2));
      Log_Fanout.Publish (Sourced, From (70, 3));
      Log_Fanout.Subscribe (Sourced, 60, Observer_Authority_Tag, Mine, Result, Source => 70);
      pragma Assert (Result = OK);
      Log_Fanout.Publish (Sourced, From (80, 4));
      Log_Fanout.Publish (Sourced, From (70, 5));
      for Ms of From_70 loop
         Log_Fanout.Read_Next (Sourced, 60, Observer_Authority_Tag, Mine, Value, Lost, Result);
         pragma Assert (Result = OK and Value.Source = 70 and Value.Monotonic_Ms = Ms);
      end loop;
      Log_Fanout.Read_Next (Sourced, 60, Observer_Authority_Tag, Mine, Value, Lost, Result);
      pragma Assert (Result = Empty);
      --  Closing and subscribing again replays afresh (a new query).
      Log_Fanout.Close (Sourced, 60, Observer_Authority_Tag, Mine, Result);
      pragma Assert (Result = OK);
      Log_Fanout.Subscribe (Sourced, 60, Observer_Authority_Tag, Mine, Result, Source => 80);
      pragma Assert (Result = OK);
      for Ms of From_80 loop
         Log_Fanout.Read_Next (Sourced, 60, Observer_Authority_Tag, Mine, Value, Lost, Result);
         pragma Assert (Result = OK and Value.Source = 80 and Value.Monotonic_Ms = Ms);
      end loop;
      Put_Line ("PASS: source-filtered subscriptions and fresh replay per query");
   end;
end Main;
