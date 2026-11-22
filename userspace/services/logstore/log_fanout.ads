pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Process_IDs; use CuBit.Process_IDs;
private with Heap_Arrays;
package Log_Fanout with SPARK_Mode is
   --  Single-owner broker; logstore serializes access in its service loop.
   --  Caller and Authority_Tag must be authenticated by a trusted adapter.
   --  Publish is an internal trusted operation, not an IPC authorization gate.
   --  Each subscriber keeps an independent queue of a fixed protocol size.
   --
   --  Retained history is kept per publishing source (LOG-002), so one
   --  chatty source cannot erase another's, and grows on the process heap
   --  (LOG-005, Heap_Arrays) up to the ceilings logstore's logs.retention
   --  setting names: each source keeps its first records (the boot window,
   --  never displaced by its later records) and as many of its newest as
   --  its own ceiling and the total ceiling allow. When a source is over
   --  its ceiling, or every source together is at the total, the oldest
   --  record after a boot window goes first (the largest history's, for
   --  the total), and every record given up is counted: in its source's
   --  tally, and in the gap before the next record of that source that is
   --  retained. Every record carries a publish-order number, so a new
   --  subscription's replay and the persistent log take the per-source
   --  histories merged in publish order. Replay is read incrementally from
   --  the histories, not copied into the subscriber's queue, so it cannot
   --  overflow that queue.
   --  Records a subscriber's queue holds beyond its stream ring: live
   --  records published after it subscribed.
   Capacity : constant := CuBit.Log_Protocol.Observer_Queue_Records;
   Maximum_Subscribers : constant := 8;
   --  Publisher channels logstore holds at once (Publisher_Rings).
   Publishing_Sources : constant := 64;
   --  Sources with retained history at once: every publisher channel, and
   --  logstore's own records. A new source beyond that takes the slot of an
   --  ended source first, then of one the persistent log already has, then
   --  the least recently published (Take_Reclaimed reports it).
   Source_Slots : constant := Publishing_Sources + 1;

   --  The retention ceilings, in records (one record is one history slot:
   --  the decoded record, at most about 0.9 KiB). The least of each is what
   --  logstore keeps until it has read the plan's logs.retention (records
   --  arrive before Config answers), so a setting can only raise them; the
   --  greatest are representation bounds, never policy. The setting's CCL
   --  type (CCL.Interfaces.Logs.RETENTION_TYPE_SOURCE) spells the same
   --  ranges.
   Least_Per_Source : constant := 1_024;
   Greatest_Per_Source : constant := 1_048_576;
   Least_Boot : constant := 64;
   Greatest_Boot : constant := 65_536;
   Least_Total : constant := 8_192;
   Greatest_Total : constant := 16_777_216;
   subtype Per_Source_Ceiling is Positive range Least_Per_Source .. Greatest_Per_Source;
   subtype Boot_Ceiling is Positive range Least_Boot .. Greatest_Boot;
   subtype Total_Ceiling is Positive range Least_Total .. Greatest_Total;
   type Ceilings is record
      --  Records one source keeps, its boot window included.
      Per_Source : Per_Source_Ceiling := Least_Per_Source;
      --  A source's first records, never given up for its later ones.
      Boot : Boot_Ceiling := Least_Boot;
      --  Records every source together keeps.
      Total : Total_Ceiling := Least_Total;
   end record;
   Until_Configured : constant Ceilings :=
     (Per_Source => Least_Per_Source, Boot => Least_Boot, Total => Least_Total);
   --  A boot window smaller than a source's ceiling, and a source's ceiling
   --  within the total.
   function Consistent (Limits : Ceilings) return Boolean is
     (Limits.Boot < Limits.Per_Source and then Limits.Per_Source <= Limits.Total);
   --  Whether Limits are at least Current, ceiling by ceiling.
   function Raises (Limits, Current : Ceilings) return Boolean is
     (Limits.Per_Source >= Current.Per_Source and then Limits.Boot >= Current.Boot
      and then Limits.Total >= Current.Total);

   type Broker is limited private
   with Default_Initial_Condition => Accounted (Broker);
   --  The counting invariant (proved): for every source with a history,
   --  every record it published is either retained or counted as not
   --  retained, Published = Retained + Not_Retained, and its history fits
   --  its block.
   function Accounted (Item : Broker) return Boolean;

   function Limits (Item : Broker) return Ceilings;
   --  The plan's ceilings (logs.retention), once read. They only rise:
   --  what is already retained stays.
   procedure Configure (Item : in out Broker; Limits : Ceilings)
   with Pre => Accounted (Item) and then Consistent (Limits)
               and then Raises (Limits, Log_Fanout.Limits (Item)),
        Post => Accounted (Item) and then Log_Fanout.Limits (Item) = Limits;

   Subscription_Lease_Ms : constant Unsigned_64 := 30_000;
   --  Service supplies trusted monotonic time before handling each message.
   --  Expiration frees dead/idle subscribers without polling the process table.
   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64)
   with Pre => Accounted (Item), Post => Accounted (Item);
   procedure Publish
     (Item : in out Broker; Value : CuBit.Log_Protocol.Event)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  Records Source offered that never reached Publish: Over_Budget shed by
   --  logstore's publication budget, Publisher_Full shed by the publisher
   --  (its ring was full). Counted in Source's tally.
   procedure Count_Shed
     (Item : in out Broker; Source : Unsigned_64; Over_Budget, Publisher_Full : Unsigned_64)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  The publisher Source ended (its channel closed or its process exited):
   --  its history stays, but its slot is the first given to a new source.
   procedure Retire (Item : in out Broker; Source : Unsigned_64)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  Whether any record of Source is retained: its identity is kept while
   --  it is (LOG-003, Source_Identities).
   function Holds_History (Item : Broker; Source : Unsigned_64) return Boolean
   with Pre => Accounted (Item);
   --  Source's accounting (all zero, Source set, when it has no history).
   function Tally (Item : Broker; Source : Unsigned_64) return CuBit.Log_Protocol.Source_Tally
   with Pre => Accounted (Item);
   --  A source whose history gave way to a new source (Source_Slots), and
   --  its accounting then; Found False when none since the last call.
   --  Missed counts those that gave way while one was already waiting.
   procedure Take_Reclaimed
     (Item : in out Broker; Gone : out CuBit.Log_Protocol.Source_Tally; Found : out Boolean;
      Missed : out Unsigned_64)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  Minimum filters by severity and Source by publishing process (0: every
   --  source) in the service, so an observer copies only what it asked for.
   --  A new subscription replays retained history that passes the filter
   --  (a viewer's "recent records of service X"), in publish order, each
   --  record with the gap before it (records of its source not retained),
   --  then, With_Tallies, each covered source's tally (a replay of one
   --  source always ends with its tally, all zero when it has no history,
   --  so a reader knows the replay is done); a retry keeps the queue and
   --  replay position and updates the filter.
   procedure Subscribe
     (Item : in out Broker; Caller : Process_ID; Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Report : CuBit.Log_Protocol.Replay_Report := CuBit.Log_Protocol.Records_Only)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  The next event for a subscription (replay first, then its tallies,
   --  then its queue), taken by logstore on the owner's behalf to write
   --  into its stream:
   --  - OK: Value, and in Missing what of its source was not retained
   --    just before it (in replay; live records omit nothing);
   --  - Gap: only Missing: records this subscriber was due were lost
   --    (Lost: its queue overflowed, or a record it was due was given up
   --    and no record after it is left in its replay), or records were not
   --    retained before one this reader's filter leaves out;
   --  - Summary: Tally, one covered source's accounting;
   --  - Empty: nothing more for now.
   --  Renew counts this as the owner's use for the lease; logstore's
   --  draining passes False, so only the reader's own calls keep a
   --  subscription alive.
   procedure Read_Next
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Value : out CuBit.Log_Protocol.Event; Missing : out CuBit.Log_Protocol.Omission;
      Tally : out CuBit.Log_Protocol.Source_Tally;
      Result : out CuBit.Log_Protocol.Status; Renew : Boolean := True)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  Whether Handle names a live subscription (not closed, not expired).
   function Active (Item : Broker; Handle : Unsigned_64) return Boolean;
   procedure Close
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Result : out CuBit.Log_Protocol.Status)
   with Pre => Accounted (Item), Post => Accounted (Item);
   --  Every published record also waits, in publish order, for the
   --  persistent log (logstore's file on the system volume): the oldest
   --  retained record it has not taken, and how many records published just
   --  before it were given up from the histories before it took them.
   procedure Take_Persistent
     (Item : in out Broker; Value : out CuBit.Log_Protocol.Event; Found : out Boolean;
      Lost : out Unsigned_64)
   with Pre => Accounted (Item), Post => Accounted (Item);
private
   --  Publish order: First_Order for the first record, No_Order before it.
   subtype Order_Number is Unsigned_64;
   No_Order : constant Order_Number := 0;
   First_Order : constant Order_Number := 1;

   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Count is Natural range 0 .. Capacity;
   type Events is array (Index) of CuBit.Log_Protocol.Event;
   type Queue is record
      Data : Events;
      Head : Index := 0;
      Used : Count := 0;
      Lost : Unsigned_64 := 0;
   end record;
   subtype Source_Slot is Positive range 1 .. Source_Slots;
   --  The next source a replay's tallies consider; No_Tally: none (not
   --  asked for, or all given).
   subtype Tally_Cursor is Natural range 0 .. Source_Slots;
   No_Tally : constant Tally_Cursor := 0;
   type Subscriber is record
      Owner : Process_ID := No_Process;   --  No_Process: a free entry
      Authority_Tag : Unsigned_64 := 0;
      Last_Use : Unsigned_64 := 0;
      Handle : Unsigned_64 := 0;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      --  Replay: retained records from Replay_Next up to (not including)
      --  Replay_End, the first record published after subscribing. Done
      --  when they meet; then the tallies from Tally_Next; later records
      --  are in Pending.
      Replay_Next : Order_Number := No_Order;
      Replay_End : Order_Number := No_Order;
      Tally_Next : Tally_Cursor := No_Tally;
      Pending : Queue;
   end record;
   subtype Subscriber_Index is Positive range 1 .. Maximum_Subscribers;
   type Subscribers is array (Subscriber_Index) of Subscriber;

   type Retained is record
      Order : Order_Number := No_Order;
      Value : CuBit.Log_Protocol.Event;
      --  Its source's records given up since the retained one before it.
      Gap_Before : Unsigned_64 := 0;
   end record;
   package Blocks is new Heap_Arrays (Retained);
   --  The first block's records: an allocation hint, never a limit.
   Initial_History : constant := 64;

   --  A source's records over its lifetime: integers, not words, so the
   --  counting invariant is plain arithmetic. 2**63 records at a million a
   --  second take almost 300 000 years.
   type Record_Total is range 0 .. 2**63 - 1;

   --  A source's retained records in publish order: its boot window at
   --  Store (0 .. Boot_Used - 1), then the ring of its newest in the rest
   --  of the block, Ring_Used of them from Ring_Head (relative to the
   --  ring's first index, Boot_Used). The boot window grows only while the
   --  ring is empty, so the order holds.
   type History is record
      In_Use : Boolean := False;
      Source : Unsigned_64 := 0;
      Ended : Boolean := False;
      Latest : Order_Number := No_Order;
      Store : Blocks.Block := Blocks.Empty;
      Boot_Used : Natural := 0;
      Ring_Head : Natural := 0;
      Ring_Used : Natural := 0;
      --  Accounting (Balanced): records published, and of them given up.
      Published : Record_Total := 0;
      Not_Retained : Record_Total := 0;
      --  Given up with no retained record after them yet: the next one's
      --  gap.
      Pending_Gap : Unsigned_64 := 0;
      --  Shed before publication (Count_Shed).
      Over_Budget, Publisher_Full : Unsigned_64 := 0;
   end record;
   type Histories is array (Source_Slot) of History;

   function Block_Size (H : History) return Natural is (Blocks.Capacity (H.Store));
   --  The ring's room: the block after the boot window.
   function Ring_Size (H : History) return Natural is (Block_Size (H) - H.Boot_Used)
   with Pre => H.Boot_Used <= Block_Size (H);
   function Well_Formed (H : History) return Boolean is
     (Block_Size (H) <= Greatest_Per_Source
      and then H.Boot_Used <= Block_Size (H)
      and then H.Ring_Used <= Ring_Size (H)
      and then (if Ring_Size (H) = 0 or else H.Ring_Used = 0 then H.Ring_Head = 0
                else H.Ring_Head < Ring_Size (H)));
   function Held (H : History) return Natural is (H.Boot_Used + H.Ring_Used)
   with Pre => Well_Formed (H);
   function Balanced (H : History) return Boolean is
     (H.Not_Retained <= H.Published
      and then H.Published - H.Not_Retained = Record_Total (Held (H)))
   with Pre => Well_Formed (H);
   function Valid (H : History) return Boolean is (Well_Formed (H) and then Balanced (H));

   type Broker is limited record
      Kept : Histories;
      Clients : Subscribers;
      Limits : Ceilings := Until_Configured;
      Next_Order : Order_Number := First_Order;
      --  The next record the persistent log is due.
      Persist_Next : Order_Number := First_Order;
      Next_Handle : Unsigned_64 := 1;
      Now_Ms : Unsigned_64 := 0;
      --  The last history that gave way to a new source, not yet taken.
      Reclaimed : CuBit.Log_Protocol.Source_Tally;
      Reclaim_Waiting : Boolean := False;
      Reclaims_Missed : Unsigned_64 := 0;
   end record;

   function Accounted (Item : Broker) return Boolean is
     (for all S in Source_Slot => Valid (Item.Kept (S)));
   function Limits (Item : Broker) return Ceilings is (Item.Limits);
end Log_Fanout;
