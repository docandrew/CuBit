with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with Log_Fanout;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with Log_Budgets;
with CuBit.Grant_References;
with CuBit.Authority_Policy; use CuBit.Authority_Policy;
with CuBit.Log_Records;
with CuBit.Log_Streams;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
with CuBit.Log_Sources;
with Source_Identities;
with CuAlloc_Host;
pragma Unreferenced (CuAlloc_Host);
procedure Main is
   --  Brokers are several MiB each: on the heap, not the test's stack.
   type Broker_Access is access Log_Fanout.Broker;
   Store_Storage : constant Broker_Access := new Log_Fanout.Broker;
   Store : Log_Fanout.Broker renames Store_Storage.all;
   First, Second, Other, Replacement, Lost : Unsigned_64;
   Result : Status;
   Value : Event;
   Expected : Event :=
     (Source => 30, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
      Monotonic_Ms => 123, Data => CuBit.Log_Records.Empty_Record);
   Empty_Value : constant Event := (others => <>);
   procedure Read (Caller : Process_ID; Handle : Unsigned_64) is
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
      --  One credit per Refill_Ms: none just before it, one at it.
      Log_Budgets.Advance_Time (Limits, 1000 + Log_Budgets.Refill_Ms - 1);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 0);
      Log_Budgets.Advance_Time (Limits, 1000 + Log_Budgets.Refill_Ms);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 1);
      Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
      pragma Assert (Accepted);
      --  Time going backwards adds nothing.
      Log_Budgets.Advance_Time (Limits, 1000);
      Log_Budgets.Advance_Time (Limits, 1000 + 2 * Log_Budgets.Refill_Ms - 1);
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
      --  Arrivals (three per millisecond) outrun the refill, so the bucket
      --  empties and the sustained rate shows.
      for Tick in 0 .. 10_000 loop
         Log_Budgets.Advance_Time (Sustained, Unsigned_64 (Tick));
         for Arrival in 1 .. 3 loop
            Log_Budgets.Admit (Sustained, Bootstrap_Budget, Accepted);
            if Accepted then Total := Total + 1; end if;
         end loop;
         pragma Assert
           (Total <= Log_Budgets.Burst + Tick / Natural (Log_Budgets.Refill_Ms));
      end loop;
      pragma Assert (Total = Log_Budgets.Burst + Natural (10_000 / Log_Budgets.Refill_Ms));
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
   pragma Assert (May_Publish (Publisher_Authority_Tag));
   pragma Assert (not Is_Observer (Observer_Tag_Base));
   pragma Assert (not Is_Observer (Unsigned_64'Last));
   pragma Assert (Is_Observer (Observer_Tag_Base + Unsigned_64 (Unsigned_32'Last)));
   for Op in Subscribe .. Close loop
      pragma Assert (not May_Invoke (Publisher_Authority_Tag, Op));
      pragma Assert (May_Invoke (Observer_Authority_Tag, Op));
   end loop;
   pragma Assert (not May_Publish (Observer_Authority_Tag));
   Log_Fanout.Publish (Store, Expected);
   Log_Fanout.Subscribe (Store, From_Word (30), Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, From_Word (0), Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, From_Word (10), Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK and First /= 0);
   Log_Fanout.Subscribe (Store, From_Word (20), Observer_Authority_Tag, Second, Result);
   pragma Assert (Result = OK and Second /= First);
   Log_Fanout.Subscribe (Store, From_Word (10), Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other = First);
   --  Recycled PID with a different launch-issued observer tag cannot inherit
   --  the previous instance's subscription, even knowing its handle.
   Log_Fanout.Read_Next
     (Store, From_Word (10), Observer_Authority_Tag + 1, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, From_Word (10), Observer_Authority_Tag + 1, First, Result);
   pragma Assert (Result = Denied);
   --  Knowledge of another subscriber's ID does not authorize use or close.
   Read (From_Word (20), First);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, From_Word (20), Observer_Authority_Tag, First, Result);
   pragma Assert (Result = Denied);
   --  Even the owner must invoke the observer authority, not its publisher one.
   Log_Fanout.Read_Next
     (Store, From_Word (10), Publisher_Authority_Tag, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Read (From_Word (10), First); pragma Assert (Result = OK and Value = Expected);
   Read (From_Word (20), Second); pragma Assert (Result = OK and Value = Expected);
   Read (From_Word (10), First); pragma Assert (Result = Empty and Value = Empty_Value);
   --  One slow recipient does not advance another recipient's cursor.
   for I in 1 .. Log_Fanout.Capacity + 3 loop
      Expected.Monotonic_Ms := Unsigned_64 (I);
      Log_Fanout.Publish (Store, Expected);
      Read (From_Word (10), First); pragma Assert (Result = OK and Value = Expected);
   end loop;
   Read (From_Word (20), Second); pragma Assert (Result = Gap and Lost = 3 and Value = Empty_Value);
   for I in 4 .. Log_Fanout.Capacity + 3 loop
      Read (From_Word (20), Second); pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
   end loop;
   Read (From_Word (20), Second); pragma Assert (Result = Empty);
   Log_Fanout.Close (Store, From_Word (10), Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK);
   Log_Fanout.Subscribe (Store, From_Word (10), Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First and Other /= Second);
   Read (From_Word (10), First); pragma Assert (Result = Denied);
   Log_Fanout.Close (Store, From_Word (10), Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied);
   --  Source 30 published the initial record and Capacity+3 more: its
   --  history keeps its boot window (the initial record, then 1 .. 31) and
   --  its newest records, and a fresh subscription replays them in order
   --  with no gap for what was not retained (LOG-002).
   Read (From_Word (10), Other); pragma Assert (Result = OK and Value.Monotonic_Ms = 123);
   for I in 1 .. Log_Fanout.Boot_Records - 1 loop
      Read (From_Word (10), Other);
      pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
   end loop;
   for I in Log_Fanout.Capacity + 3 - Log_Fanout.Newest_Records + 1 .. Log_Fanout.Capacity + 3 loop
      Read (From_Word (10), Other);
      pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
   end loop;
   Read (From_Word (10), Other); pragma Assert (Result = Empty);
   Replacement := Other;
   --  Capacity rejection never returns a token or permits an unprivileged
   --  caller to distinguish capacity exhaustion from any other denial.
   for I in 3 .. Log_Fanout.Maximum_Subscribers loop
      Log_Fanout.Subscribe (Store, From_Word (Unsigned_64 (I + 100)), Observer_Authority_Tag, Other, Result);
      pragma Assert (Result = OK);
   end loop;
   Log_Fanout.Subscribe (Store, From_Word (99), Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, From_Word (99), Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Exhausted and Other = 0);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms - 1);
   Read (From_Word (20), Second); pragma Assert (Result = Empty);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms);
   Read (From_Word (10), Replacement); pragma Assert (Result = Denied);
   Read (From_Word (20), Second); pragma Assert (Result = Empty);
   Log_Fanout.Subscribe (Store, From_Word (99), Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First);
   Log_Fanout.Advance_Time (Store, Unsigned_64'Last);
   Read (From_Word (99), Other); pragma Assert (Result = Denied);
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
      Filtered_Storage : constant Broker_Access := new Log_Fanout.Broker;
      Filtered : Log_Fanout.Broker renames Filtered_Storage.all;
      Low, High : Unsigned_64;
      function At_Level (Level : L.Severity; Ms : Unsigned_64) return Event is
        (Source => 30, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
         Monotonic_Ms => Ms, Data => L.Make ("x", Level).Value);
   begin
      Log_Fanout.Publish (Filtered, At_Level (L.Debug, 1));
      Log_Fanout.Publish (Filtered, At_Level (L.Error, 2));
      --  Replay of retained history honours the filter.
      Log_Fanout.Subscribe (Filtered, From_Word (50), Observer_Authority_Tag, High,
                            Result, L.Warning);
      pragma Assert (Result = OK);
      Log_Fanout.Subscribe (Filtered, From_Word (51), Observer_Authority_Tag, Low,
                            Result);
      pragma Assert (Result = OK);
      Log_Fanout.Publish (Filtered, At_Level (L.Trace, 3));
      Log_Fanout.Publish (Filtered, At_Level (L.Critical, 4));
      Log_Fanout.Read_Next (Filtered, From_Word (50), Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = OK and Value.Monotonic_Ms = 2);
      Log_Fanout.Read_Next (Filtered, From_Word (50), Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = OK and Value.Monotonic_Ms = 4);
      Log_Fanout.Read_Next (Filtered, From_Word (50), Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      for Ms in Unsigned_64'(1) .. 4 loop
         Log_Fanout.Read_Next (Filtered, From_Word (51), Observer_Authority_Tag, Low,
                               Value, Lost, Result);
         pragma Assert (Result = OK and Value.Monotonic_Ms = Ms);
      end loop;
      --  Filtered records are not loss: a narrow observer flooded with
      --  low-severity records sees no gap.
      for I in 1 .. 2 * Log_Fanout.Capacity loop
         Log_Fanout.Publish (Filtered, At_Level (L.Debug, 5));
      end loop;
      Log_Fanout.Read_Next (Filtered, From_Word (50), Observer_Authority_Tag, High,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      Log_Fanout.Read_Next (Filtered, From_Word (51), Observer_Authority_Tag, Low,
                            Value, Lost, Result);
      pragma Assert (Result = Gap and Lost = Unsigned_64 (Log_Fanout.Capacity));
      --  A retry keeps the queue and narrows later publications.
      Log_Fanout.Subscribe (Filtered, From_Word (51), Observer_Authority_Tag, Other,
                            Result, L.Critical);
      pragma Assert (Result = OK and Other = Low);
      Log_Fanout.Publish (Filtered, At_Level (L.Error, 6));
      for I in 1 .. Log_Fanout.Capacity loop
         Log_Fanout.Read_Next (Filtered, From_Word (51), Observer_Authority_Tag, Low,
                               Value, Lost, Result);
         pragma Assert (Result = OK and Value.Monotonic_Ms = 5);
      end loop;
      Log_Fanout.Read_Next (Filtered, From_Word (51), Observer_Authority_Tag, Low,
                            Value, Lost, Result);
      pragma Assert (Result = Empty);
      Put_Line ("PASS: severity-filtered subscriptions, filtered replay, filtering is not loss");
   end;
   --  Source-filtered subscriptions: a viewer's "recent records of service X"
   --  replays only that publisher's history and then only its new records.
   declare
      package L renames CuBit.Log_Records;
      Sourced_Storage : constant Broker_Access := new Log_Fanout.Broker;
      Sourced : Log_Fanout.Broker renames Sourced_Storage.all;
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
      Log_Fanout.Subscribe (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Result, Source => 70);
      pragma Assert (Result = OK);
      Log_Fanout.Publish (Sourced, From (80, 4));
      Log_Fanout.Publish (Sourced, From (70, 5));
      for Ms of From_70 loop
         Log_Fanout.Read_Next (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Value, Lost, Result);
         pragma Assert (Result = OK and Value.Source = 70 and Value.Monotonic_Ms = Ms);
      end loop;
      Log_Fanout.Read_Next (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Value, Lost, Result);
      pragma Assert (Result = Empty);
      --  Closing and subscribing again replays afresh (a new query).
      Log_Fanout.Close (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Result);
      pragma Assert (Result = OK);
      Log_Fanout.Subscribe (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Result, Source => 80);
      pragma Assert (Result = OK);
      for Ms of From_80 loop
         Log_Fanout.Read_Next (Sourced, From_Word (60), Observer_Authority_Tag, Mine, Value, Lost, Result);
         pragma Assert (Result = OK and Value.Source = 80 and Value.Monotonic_Ms = Ms);
      end loop;
      Put_Line ("PASS: source-filtered subscriptions and fresh replay per query");
   end;
   --  Per-source retention (LOG-002): a flooding source cannot erase a quiet
   --  source's boot records; replay merges the histories in publish order,
   --  reads them incrementally (more than a subscriber queue holds) and
   --  reports as a gap only records given up while it was due them.
   declare
      package L renames CuBit.Log_Records;
      Retained_Storage : constant Broker_Access := new Log_Fanout.Broker;
      Kept : Log_Fanout.Broker renames Retained_Storage.all;
      Quiet : constant Unsigned_64 := 200;
      Noisy : constant Unsigned_64 := 201;
      Quiet_Records : constant := 5;
      Flood_Records : constant := 5_000;
      Wide_Sources : constant := 20;
      Wide_Per_Source : constant := Log_Fanout.Boot_Records + Log_Fanout.Newest_Records;
      Published : Unsigned_64 := 0;
      Viewer, Narrow : Unsigned_64;
      Previous : Unsigned_64;
      Seen : Natural;
      Found : Boolean;
      procedure Put (Source : Unsigned_64) is
      begin
         Published := Published + 1;
         Log_Fanout.Publish
           (Kept, (Source => Source, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
                   Monotonic_Ms => Published, Data => L.Make ("z").Value));
      end Put;
      procedure Next (Caller : Unsigned_64; Handle : Unsigned_64) is
      begin
         Log_Fanout.Read_Next (Kept, From_Word (Caller), Observer_Authority_Tag, Handle,
                               Value, Lost, Result);
      end Next;
   begin
      for I in 1 .. Quiet_Records loop Put (Quiet); end loop;
      for I in 1 .. Flood_Records loop Put (Noisy); end loop;
      --  The quiet source alone: every boot record, no gap.
      Log_Fanout.Subscribe (Kept, From_Word (300), Observer_Authority_Tag, Narrow, Result, Source => Quiet);
      pragma Assert (Result = OK);
      for I in 1 .. Quiet_Records loop
         Next (300, Narrow);
         pragma Assert (Result = OK and Value.Source = Quiet and Value.Monotonic_Ms = Unsigned_64 (I));
      end loop;
      Next (300, Narrow); pragma Assert (Result = Empty);
      --  Everything: the quiet boot records, the flood's boot window and its
      --  newest, in publish order and with no gap.
      Log_Fanout.Subscribe (Kept, From_Word (301), Observer_Authority_Tag, Viewer, Result);
      pragma Assert (Result = OK);
      Previous := 0;
      Seen := 0;
      loop
         Next (301, Viewer);
         exit when Result = Empty;
         pragma Assert (Result = OK and Value.Monotonic_Ms > Previous);
         Previous := Value.Monotonic_Ms;
         Seen := Seen + 1;
      end loop;
      pragma Assert (Seen = Quiet_Records + Log_Fanout.Boot_Records + Log_Fanout.Newest_Records);
      pragma Assert (Previous = Published);
      --  The persistent log reads the same histories in publish order; what
      --  the flood's newest ring gave up before it was taken is its loss.
      Log_Fanout.Take_Persistent (Kept, Value, Found, Lost);
      pragma Assert (Found and Lost = 0 and Value.Monotonic_Ms = 1);
      for I in 2 .. Quiet_Records + Log_Fanout.Boot_Records loop
         Log_Fanout.Take_Persistent (Kept, Value, Found, Lost);
         pragma Assert (Found and Lost = 0 and Value.Monotonic_Ms = Unsigned_64 (I));
      end loop;
      Log_Fanout.Take_Persistent (Kept, Value, Found, Lost);
      pragma Assert
        (Found and Lost = Unsigned_64 (Flood_Records - Log_Fanout.Boot_Records - Log_Fanout.Newest_Records)
         and Value.Monotonic_Ms = Published - Log_Fanout.Newest_Records + 1);
      loop
         Log_Fanout.Take_Persistent (Kept, Value, Found, Lost);
         pragma Assert (Lost = 0);
         exit when not Found;
      end loop;
      Log_Fanout.Close (Kept, From_Word (300), Observer_Authority_Tag, Narrow, Result);
      Log_Fanout.Close (Kept, From_Word (301), Observer_Authority_Tag, Viewer, Result);
      --  A replay far larger than a subscriber queue, read incrementally.
      for S in 1 .. Wide_Sources loop
         for I in 1 .. Wide_Per_Source loop
            Put (Unsigned_64 (1_000 + S));
         end loop;
      end loop;
      pragma Assert (Wide_Sources * Wide_Per_Source > Log_Fanout.Capacity);
      Log_Fanout.Subscribe (Kept, From_Word (302), Observer_Authority_Tag, Viewer, Result);
      pragma Assert (Result = OK);
      Previous := 0;
      Seen := 0;
      loop
         Next (302, Viewer);
         exit when Result = Empty;
         pragma Assert (Result = OK and Value.Monotonic_Ms > Previous);
         Previous := Value.Monotonic_Ms;
         Seen := Seen + 1;
      end loop;
      pragma Assert
        (Seen = Quiet_Records + Wide_Per_Source + Wide_Sources * Wide_Per_Source);
      Log_Fanout.Close (Kept, From_Word (302), Observer_Authority_Tag, Viewer, Result);
      --  Records given up while a replay was due them are its gap: the flood's
      --  newest ring turns over completely before the reader gets to it.
      Log_Fanout.Subscribe (Kept, From_Word (303), Observer_Authority_Tag, Viewer, Result, Source => Noisy);
      pragma Assert (Result = OK);
      Next (303, Viewer); pragma Assert (Result = OK);
      for I in 1 .. Log_Fanout.Newest_Records loop Put (Noisy); end loop;
      Next (303, Viewer);
      pragma Assert (Result = Gap and Lost = Unsigned_64 (Log_Fanout.Newest_Records));
      Seen := 1;
      loop
         Next (303, Viewer);
         exit when Result = Empty;
         pragma Assert (Result = OK);
         Seen := Seen + 1;
      end loop;
      --  The rest of the boot window, then the newest ring that replaced the
      --  given-up records, now in the queue as live records.
      pragma Assert (Seen = Log_Fanout.Boot_Records + Log_Fanout.Newest_Records);
      Log_Fanout.Close (Kept, From_Word (303), Observer_Authority_Tag, Viewer, Result);
      --  Every slot taken: a new source takes an ended source's slot, never a
      --  live quiet one's.
      for S in Wide_Sources + 1 .. Log_Fanout.Source_Slots - 2 loop
         Put (Unsigned_64 (1_000 + S));
      end loop;
      Log_Fanout.Retire (Kept, 1_001);
      Put (5_000);
      Log_Fanout.Subscribe (Kept, From_Word (304), Observer_Authority_Tag, Narrow, Result, Source => 1_001);
      Next (304, Narrow); pragma Assert (Result = Empty);
      Log_Fanout.Close (Kept, From_Word (304), Observer_Authority_Tag, Narrow, Result);
      Log_Fanout.Subscribe (Kept, From_Word (304), Observer_Authority_Tag, Narrow, Result, Source => Quiet);
      for I in 1 .. Quiet_Records loop
         Next (304, Narrow);
         pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
      end loop;
      Log_Fanout.Close (Kept, From_Word (304), Observer_Authority_Tag, Narrow, Result);
      Log_Fanout.Subscribe (Kept, From_Word (304), Observer_Authority_Tag, Narrow, Result, Source => 5_000);
      Next (304, Narrow); pragma Assert (Result = OK and Value.Source = 5_000);
      Put_Line ("PASS: per-source retention, ordered incremental replay, persistence order, replay loss, slot reuse");
   end;
   --  LOG-003: who a source was, kept past its exit while its records are.
   declare
      package LS renames CuBit.Log_Sources;
      use type LS.Entry_Kind;
      use type LS.Source_Identity;
      Who : constant LS.Source_Identity := LS.Make (7_001, "com.cubit.acpi", "/boot/acpi.svc");
      Bare : constant LS.Source_Identity := LS.Make (7_002, "com.cubit.wifi", "");
      Long_Identity : constant String (1 .. 100) := [others => 'a'];
      Bytes, Broken : LS.Entry_Buffer;
      Back : LS.Source_Entry;
      Accepted : Boolean;
      Table : Source_Identities.Table;
      Kept_Storage : constant Broker_Access := new Log_Fanout.Broker;
      Kept : Log_Fanout.Broker renames Kept_Storage.all;
      Exit_Ms : constant := 1_000;
   begin
      pragma Assert (LS.Valid (Who) and then LS.Named (Who));
      pragma Assert (LS.Name_Of (Who) = "acpi.svc");
      pragma Assert (LS.Identity_Of (Who) = "com.cubit.acpi");
      pragma Assert (LS.Label (Who) = "acpi");
      pragma Assert (LS.Label (Bare) = "wifi");
      pragma Assert (LS.Identity_Of (LS.Make (7_003, "bad id" & ASCII.LF, "x")) = "badid");
      pragma Assert (LS.Make (7_004, Long_Identity, "").Identity_Used = LS.Identity_Bytes);
      pragma Assert (not LS.Named (LS.Make (7_005, "", "")));
      pragma Assert (LS.Answers_To (Who, "acpi") and then LS.Answers_To (Who, "acpi.svc"));
      pragma Assert (LS.Answers_To (Who, "com.cubit.acpi") and then LS.Answers_To (Who, "cubit.acpi"));
      pragma Assert (not LS.Answers_To (Who, "acp") and then not LS.Answers_To (Who, "i.acpi"));
      pragma Assert (not LS.Answers_To (Who, "") and then not LS.Answers_To (Who, "svc"));
      pragma Assert (LS.Is_Registrar (LS.Registrar_Tag (LS.Process_Manager_Issuance)));
      pragma Assert (not LS.Is_Registrar (LS.Registrar_Tag_Base));
      pragma Assert (not LS.Is_Registrar (Publisher_Authority_Tag));
      pragma Assert (not Is_Publisher (LS.Registrar_Tag (LS.Device_Manager_Issuance)));
      pragma Assert (not Is_Observer (LS.Registrar_Tag (LS.Device_Manager_Issuance)));
      Put_Line ("PASS: source identities: names from launchers, labels, matching, registrar tags");

      LS.Encode ((Kind => LS.Exited, Who => Who, Seen_Ms => 42), Bytes);
      LS.Decode (Bytes, Back, Accepted);
      pragma Assert (Accepted and then Back.Kind = LS.Exited and then Back.Who = Who
                     and then Back.Seen_Ms = 42);
      --  Malformed: unknown kind, overlong identity, a space in it, reserved
      --  header or tail bytes, no process.
      Broken := Bytes; Broken (0) := 3;
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      Broken := Bytes; Broken (1) := LS.Identity_Bytes + 1;
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      Broken := Bytes; Broken (24) := Character'Pos (' ');
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      Broken := Bytes; Broken (3) := 1;
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      Broken := Bytes; Broken (LS.Entry_Bytes - 1) := 1;
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      Broken := Bytes; Broken (8 .. 15) := [others => 0];
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      --  A start must say who; an exit need not.
      LS.Encode ((Kind => LS.Started, Who => LS.Make (7_005, "", ""), Seen_Ms => 0), Broken);
      LS.Decode (Broken, Back, Accepted); pragma Assert (not Accepted);
      LS.Encode ((Kind => LS.Exited, Who => LS.Make (7_005, "", ""), Seen_Ms => 0), Broken);
      LS.Decode (Broken, Back, Accepted); pragma Assert (Accepted and then Back.Who.Source = 7_005);
      Put_Line ("PASS: source notices round-trip and malformed ones are refused");

      Source_Identities.Started (Table, Who, 100);
      Source_Identities.Started (Table, Bare, 200);
      pragma Assert (Source_Identities.Count (Table) = 2);
      pragma Assert (Source_Identities.Newest (Table, 1).Who = Bare);
      pragma Assert (Source_Identities.Newest (Table, 2).Kind = LS.Started);
      pragma Assert (Source_Identities.Newest (Table, 2).Seen_Ms = 100);
      --  acpi logged before it exited; wifi never did.
      Log_Fanout.Publish
        (Kept, (Source => 7_001, Node => This_Node, Publication_Tag => Publisher_Authority_Tag,
                Monotonic_Ms => 1, Data => CuBit.Log_Records.Empty_Record));
      pragma Assert (Log_Fanout.Holds_History (Kept, 7_001));
      pragma Assert (not Log_Fanout.Holds_History (Kept, 7_002));
      Source_Identities.Exited (Table, 7_001, Exit_Ms);
      Source_Identities.Exited (Table, 7_002, Exit_Ms);
      Source_Identities.Exited (Table, 9_999, Exit_Ms);
      Source_Identities.Prune (Table, Kept, Exit_Ms + Source_Identities.Exit_Grace_Ms - 1);
      pragma Assert (Source_Identities.Count (Table) = 2);
      Source_Identities.Prune (Table, Kept, Exit_Ms + Source_Identities.Exit_Grace_Ms);
      pragma Assert (Source_Identities.Count (Table) = 1);
      pragma Assert (Source_Identities.Newest (Table, 1).Who = Who);
      pragma Assert (Source_Identities.Newest (Table, 1).Kind = LS.Exited);
      --  Grows with the processes started; a repeated start replaces.
      for I in 1 .. 100 loop
         Source_Identities.Started
           (Table, LS.Make (Unsigned_64 (8_000 + I), "com.cubit.many", "many.app"), Unsigned_64 (I));
      end loop;
      pragma Assert (Source_Identities.Count (Table) = 101);
      pragma Assert (Source_Identities.Newest (Table, 1).Who.Source = 8_100);
      pragma Assert (Source_Identities.Newest (Table, 101).Who = Who);
      Source_Identities.Started (Table, LS.Make (8_050, "com.cubit.again", "again.app"), 500);
      pragma Assert (Source_Identities.Count (Table) = 101);
      pragma Assert (Source_Identities.Newest (Table, 1).Who.Source = 8_050);
      pragma Assert (Source_Identities.Lost (Table) = 0);
      Put_Line ("PASS: source identities outlive their process while its records are kept (LOG-003)");
   end;
end Main;
