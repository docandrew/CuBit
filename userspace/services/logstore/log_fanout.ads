with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
package Log_Fanout with SPARK_Mode is
   --  Single-owner broker; logstore serializes access in its service loop.
   --  Caller and Authority_Tag must be authenticated by a trusted adapter.
   --  Publish is an internal trusted operation, not an IPC authorization gate.
   -- Bounded boot replay for a viewer opened after driver initialization.
   -- Each subscriber keeps an independent queue; no unbounded allocation.
   Capacity : constant := CuBit.Log_Protocol.Observer_Queue_Records;
   Maximum_Subscribers : constant := 8;
   type Broker is limited private;
   Subscription_Lease_Ms : constant Unsigned_64 := 30_000;
   --  Service supplies trusted monotonic time before handling each message.
   --  Expiration frees dead/idle subscribers without polling the process table.
   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64);
   procedure Publish
     (Item : in out Broker; Value : CuBit.Log_Protocol.Event);
   --  Minimum filters by severity and Source by publishing process (0: every
   --  source) in the service, so an observer copies only what it asked for.
   --  A new subscription replays retained history that passes the filter
   --  (a viewer's "recent records of service X"); a retry keeps the queue
   --  and updates the filter for later publications.
   procedure Subscribe
     (Item : in out Broker; Caller, Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source);
   --  The next queued event for a subscription, taken by logstore on the
   --  owner's behalf to write into its stream. Renew counts this as the
   --  owner's use for the lease; logstore's draining passes False, so only
   --  the reader's own calls keep a subscription alive.
   procedure Read_Next
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Value : out CuBit.Log_Protocol.Event; Lost : out Unsigned_64;
      Result : out CuBit.Log_Protocol.Status; Renew : Boolean := True);
   --  Whether Handle names a live subscription (not closed, not expired).
   function Active (Item : Broker; Handle : Unsigned_64) return Boolean;
   procedure Close
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Result : out CuBit.Log_Protocol.Status);
private
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
      Owner : Unsigned_64 := 0;
      Authority_Tag : Unsigned_64 := 0;
      Last_Use : Unsigned_64 := 0;
      Handle : Unsigned_64 := 0;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Pending : Queue;
   end record;
   type Subscribers is array (Positive range 1 .. Maximum_Subscribers)
     of Subscriber;
   type Broker is limited record
      Recent : Queue;
      Clients : Subscribers;
      Next_Handle : Unsigned_64 := 1;
      Now_Ms : Unsigned_64 := 0;
   end record;
end Log_Fanout;
