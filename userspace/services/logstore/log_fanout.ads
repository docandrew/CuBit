with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Process_IDs; use CuBit.Process_IDs;
package Log_Fanout with SPARK_Mode is
   --  Single-owner broker; logstore serializes access in its service loop.
   --  Caller and Authority_Tag must be authenticated by a trusted adapter.
   --  Publish is an internal trusted operation, not an IPC authorization gate.
   --  Each subscriber keeps an independent queue; no unbounded allocation.
   --
   --  Retained history is kept per publishing source (LOG-002), so one
   --  chatty source cannot erase another's: each source keeps its first
   --  Boot_Records records (the boot window, never displaced by that
   --  source's later records) and its newest Newest_Records. Every record
   --  carries a publish-order number, so a new subscription's replay and the
   --  persistent log take the per-source histories merged in publish order.
   --  Replay is read incrementally from the histories, not copied into the
   --  subscriber's queue, so it cannot overflow that queue.
   --  Records a subscriber's queue holds beyond its stream ring: live
   --  records published after it subscribed.
   Capacity : constant := CuBit.Log_Protocol.Observer_Queue_Records;
   Maximum_Subscribers : constant := 8;
   --  Publisher channels logstore holds at once (Publisher_Rings).
   Publishing_Sources : constant := 64;
   --  Sources with retained history at once: every publisher channel, and
   --  logstore's own records. A new source beyond that takes the slot of an
   --  ended source first, then of one the persistent log already has, then
   --  the least recently published.
   Source_Slots : constant := Publishing_Sources + 1;
   Boot_Records : constant := 32;
   Newest_Records : constant := 32;
   type Broker is limited private;
   Subscription_Lease_Ms : constant Unsigned_64 := 30_000;
   --  Service supplies trusted monotonic time before handling each message.
   --  Expiration frees dead/idle subscribers without polling the process table.
   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64);
   procedure Publish
     (Item : in out Broker; Value : CuBit.Log_Protocol.Event);
   --  The publisher Source ended (its channel closed or its process exited):
   --  its history stays, but its slot is the first given to a new source.
   procedure Retire (Item : in out Broker; Source : Unsigned_64);
   --  Minimum filters by severity and Source by publishing process (0: every
   --  source) in the service, so an observer copies only what it asked for.
   --  A new subscription replays retained history that passes the filter
   --  (a viewer's "recent records of service X"), in publish order and with
   --  no gap for history that was not retained; a retry keeps the queue and
   --  replay position and updates the filter.
   procedure Subscribe
     (Item : in out Broker; Caller : Process_ID; Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source);
   --  The next event for a subscription (replay first, then its queue),
   --  taken by logstore on the owner's behalf to write into its stream.
   --  Gap: records this subscriber was due were lost, because its queue
   --  overflowed or a replay record was given up before it was read.
   --  Renew counts this as the owner's use for the lease; logstore's
   --  draining passes False, so only the reader's own calls keep a
   --  subscription alive.
   procedure Read_Next
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Value : out CuBit.Log_Protocol.Event; Lost : out Unsigned_64;
      Result : out CuBit.Log_Protocol.Status; Renew : Boolean := True);
   --  Whether Handle names a live subscription (not closed, not expired).
   function Active (Item : Broker; Handle : Unsigned_64) return Boolean;
   procedure Close
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Result : out CuBit.Log_Protocol.Status);
   --  Every published record also waits, in publish order, for the
   --  persistent log (logstore's file on the system volume): the oldest
   --  retained record it has not taken, and how many records published just
   --  before it were given up from the histories before it took them.
   procedure Take_Persistent
     (Item : in out Broker; Value : out CuBit.Log_Protocol.Event; Found : out Boolean;
      Lost : out Unsigned_64);
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
   type Subscriber is record
      Owner : Process_ID := No_Process;   --  No_Process: a free entry
      Authority_Tag : Unsigned_64 := 0;
      Last_Use : Unsigned_64 := 0;
      Handle : Unsigned_64 := 0;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      --  Replay: retained records from Replay_Next up to (not including)
      --  Replay_End, the first record published after subscribing. Done
      --  when they meet; later records are in Pending.
      Replay_Next : Order_Number := No_Order;
      Replay_End : Order_Number := No_Order;
      Pending : Queue;
   end record;
   subtype Subscriber_Index is Positive range 1 .. Maximum_Subscribers;
   type Subscribers is array (Subscriber_Index) of Subscriber;

   type Retained is record
      Order : Order_Number := No_Order;
      Value : CuBit.Log_Protocol.Event;
   end record;
   subtype Boot_Index is Natural range 0 .. Boot_Records - 1;
   subtype Boot_Count is Natural range 0 .. Boot_Records;
   subtype Newest_Index is Natural range 0 .. Newest_Records - 1;
   subtype Newest_Count is Natural range 0 .. Newest_Records;
   --  A source's retained records in publish order: its boot window, then
   --  its newest ring from the oldest.
   subtype History_Count is Natural range 0 .. Boot_Records + Newest_Records;
   subtype History_Position is History_Count range 0 .. History_Count'Last - 1;
   type Boot_Window is array (Boot_Index) of Retained;
   type Newest_Ring is array (Newest_Index) of Retained;
   type History is record
      In_Use : Boolean := False;
      Source : Unsigned_64 := 0;
      Ended : Boolean := False;
      Latest : Order_Number := No_Order;
      Boot : Boot_Window;
      Boot_Used : Boot_Count := 0;
      Newest : Newest_Ring;
      Newest_Head : Newest_Index := 0;
      Newest_Used : Newest_Count := 0;
   end record;
   subtype Source_Slot is Positive range 1 .. Source_Slots;
   type Histories is array (Source_Slot) of History;

   type Broker is limited record
      Kept : Histories;
      Clients : Subscribers;
      Next_Order : Order_Number := First_Order;
      --  The next record the persistent log is due.
      Persist_Next : Order_Number := First_Order;
      Next_Handle : Unsigned_64 := 1;
      Now_Ms : Unsigned_64 := 0;
   end record;
end Log_Fanout;
