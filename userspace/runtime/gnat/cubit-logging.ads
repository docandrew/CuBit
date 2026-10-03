pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Log_Streams;
with CuBit.Channel_Rings;

--  Native adapter, not part of the portable SPARK proof. Keep these limited
--  objects alive while connected: their aligned pages back grants. Publishers
--  may be destroyed after Disconnect reports Done.
--  Readers remain process-lived.
--  Calls on each object are serialized by its application event loop.
package CuBit.Logging is
   type Publisher
     (Slot : CuBit.Messages.CapabilitySlot :=
        CuBit.Log_Protocol.Publisher_Slot)
     is limited private;
   --  One outstanding publication. Busy/unavailable returns False and counts
   --  a drop; never waits for the collector. Tokens must be unique among the
   --  application's live async requests. No hidden completion polling.
   --  Rate-limited completions count a drop but leave the publisher usable.
   --  No automatic retry: that would turn diagnostic loss into more load.
   procedure Emit
     (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
      Token : Unsigned_64; Submitted : out Boolean);
   procedure Complete
     (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry;
      Handled : out Boolean);
   function Dropped (Item : Publisher) return Unsigned_64;
   function Pending (Item : Publisher) return Boolean;
   --  The least severe record logstore keeps, as its last reply said (Trace
   --  before any reply). Records below it are discarded there; Wanted lets a
   --  publisher skip them and the IPC they cost.
   function Minimum (Item : Publisher) return CuBit.Log_Records.Severity;
   function Wanted (Item : Publisher; Level : CuBit.Log_Records.Severity) return Boolean;
   --  Terminal disconnect: prohibits further Emit calls from submitting work.
   --  No service RPC wait. Revoke, then query retirement; repeat after
   --  handling completions. Done requires BOTH grant retirement and no CQE
   --  outstanding. Failure/death alone never releases the buffer.
   --  No discovery/rebind/retry or automatic reconnection is performed.
   procedure Disconnect (Item : in out Publisher; Done : out Boolean);

   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited private;
   --  Interactive synchronous operations. Never use these in interrupt or
   --  latency-sensitive service paths. Close closes the subscription only;
   --  its buffer/grant stays owned by the Reader for safe reuse.
   --  Minimum filters by severity inside logstore.
   --  Records at or above Minimum from Source (a process; Every_Source for
   --  all). A new subscription first replays matching retained history.
   procedure Subscribe
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source);
   --  The next event, from the reader's stream ring (CuBit.Log_Streams):
   --  logstore writes into it, so reading takes no IPC. Result is Gap (with
   --  Lost) where logstore could not keep events for this reader, Empty when
   --  nothing new has arrived. Every RENEW_MS of reading, the subscription's
   --  lease is renewed with one Subscribe call.
   procedure Read_Next
     (Item : in out Reader; Value : out CuBit.Log_Protocol.Event;
      Lost : out Unsigned_64; Result : out CuBit.Log_Protocol.Status);
   procedure Close
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status);
   --  Well inside logstore's subscription lease (Log_Fanout).
   RENEW_MS : constant := 10_000;

   --  One record from a service that otherwise does not log: typically
   --  "started". A synchronous call through the caller's logstore binding
   --  (manifest request-service logstore), meant for startup and exit paths,
   --  not hot paths; it never touches the completion queue, so it is safe
   --  beside the caller's own asynchronous work. Published is False without
   --  the binding or when logstore refuses (for example, rate limited).
   --  What logstore keeps, for every publisher: records at Level and above.
   --  Set_Minimum needs the log-control role (manifest request-service
   --  log-control); Previous is the minimum it replaced. Get_Minimum works
   --  through any logstore endpoint, by default the observer's.
   procedure Set_Minimum
     (Level : CuBit.Log_Records.Severity; Previous : out CuBit.Log_Records.Severity;
      Result : out CuBit.Log_Protocol.Status;
      Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Control_Slot);
   procedure Get_Minimum
     (Level : out CuBit.Log_Records.Severity; Result : out CuBit.Log_Protocol.Status;
      Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot);

   --  One record, published now with a synchronous call through the
   --  logstore binding; never touches the completion queue. Result is OK
   --  (kept), Below_Minimum (delivered, not kept) or why not; Kept_From is
   --  the minimum logstore reported (Trace when it said nothing).
   procedure Publish_Now
     (Value : CuBit.Log_Records.Log_Record; Result : out CuBit.Log_Protocol.Status;
      Kept_From : out CuBit.Log_Records.Severity);

   procedure Announce
     (Text : String; Published : out Boolean;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
private
   type Transfer_Page is array (Positive range 1 .. 4096) of Unsigned_8
     with Alignment => 4096;
   type Writer_State is (Uninitialized, Ready, In_Flight, Disabled);
   type Publisher
     (Slot : CuBit.Messages.CapabilitySlot :=
        CuBit.Log_Protocol.Publisher_Slot)
     is limited record
      Page : Transfer_Page := [others => 0];
      Grant : CuBit.Memory_Grants.Grant_Reference;
      State : Writer_State := Uninitialized;
      Token : Unsigned_64 := 0;
      Loss : Unsigned_64 := 0;
      Has_Grant : Boolean := False;
      Disconnecting : Boolean := False;
      Kept_From : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
   end record;
   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited record
      Region : CuBit.Log_Streams.Stream_Region := [others => 0];
      Consumer : CuBit.Channel_Rings.Consumer :=
        CuBit.Channel_Rings.New_Consumer (CuBit.Log_Streams.RING_BYTES);
      --  The filter, resent when the lease is renewed, and when it was.
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Renewed_Ms : Unsigned_64 := 0;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Has_Grant : Boolean := False;
      Subscription : Unsigned_64 := 0;
   end record;
end CuBit.Logging;
