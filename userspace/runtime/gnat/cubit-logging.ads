pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Channels;

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
   --  A publisher's records go into its own channel to logstore
   --  (CuBit.Log_Publish_Rings, CuBit.Channels), opened on the first Emit.
   --  Emitting is a copy into shared memory: no IPC, no waiting, no
   --  completion. logstore drains the channel; the publisher kicks it only
   --  when it asked to be woken before sleeping. A full ring sheds the
   --  record and counts it (Dropped, and the channel's count logstore
   --  reports): logging never stalls the caller.
   --  Submitted False: shed, or no logstore binding. Publications complete
   --  in the ring, never on the completion queue.
   procedure Emit
     (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
      Submitted : out Boolean);
   function Dropped (Item : Publisher) return Unsigned_64;
   --  The least severe record logstore keeps, as it last published in the
   --  channel (Trace before it opens). Records below it are discarded there; Wanted
   --  lets a publisher skip them before encoding.
   function Minimum (Item : Publisher) return CuBit.Log_Records.Severity;
   function Wanted (Item : Publisher; Level : CuBit.Log_Records.Severity) return Boolean;
   --  For exit paths: Kick logstore and wait, at most Wait_Ms, until it has
   --  consumed everything emitted so far. Drained False on timeout.
   procedure Flush (Item : in out Publisher; Drained : out Boolean; Wait_Ms : Natural := 200);
   --  Terminal disconnect: no further records. The channel closes; logstore
   --  drains what is left. Done at once (CuBit.Channels keeps the pages
   --  until logstore lets go of them).
   procedure Disconnect (Item : in out Publisher; Done : out Boolean);

   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited private;
   --  Interactive synchronous operations. Never use these in interrupt or
   --  latency-sensitive service paths. Close ends the subscription and its
   --  channel; Subscribe again opens a new one.
   --  Minimum filters by severity inside logstore.
   --  Records at or above Minimum from Source (a process; Every_Source for
   --  all). A new subscription first replays matching retained history.
   procedure Subscribe
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source);
   --  The next event, from the reader's stream channel (CuBit.Log_Streams):
   --  logstore produces into it, so reading takes no IPC. Result is Gap (with
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

   --  One record, through the process's own announcement publisher (its
   --  ring), then waiting briefly (Flush) until logstore has taken it: for
   --  startup and exit paths, where waiting is harmless. Result is OK
   --  (kept), Below_Minimum (taken, not kept), or Unavailable (no binding,
   --  shed, or not taken in time); Kept_From is the minimum logstore keeps.
   procedure Publish_Now
     (Value : CuBit.Log_Records.Log_Record; Result : out CuBit.Log_Protocol.Status;
      Kept_From : out CuBit.Log_Records.Severity);

   procedure Announce
     (Text : String; Published : out Boolean;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
private
   type Writer_State is (Uninitialized, Ready, Disabled);
   type Publisher
     (Slot : CuBit.Messages.CapabilitySlot :=
        CuBit.Log_Protocol.Publisher_Slot)
     is limited record
      Link : CuBit.Channels.Channel;
      State : Writer_State := Uninitialized;
      Loss : Unsigned_64 := 0;
      Disconnecting : Boolean := False;
   end record;
   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited record
      Link : CuBit.Channels.Channel;
      --  The filter, resent when the lease is renewed, and when it was.
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Renewed_Ms : Unsigned_64 := 0;
      Subscription : Unsigned_64 := 0;
   end record;
end CuBit.Logging;
