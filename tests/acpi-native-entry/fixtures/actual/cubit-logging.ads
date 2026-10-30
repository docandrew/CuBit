pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.Services;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Channels;

--  Native adapter, not part of the portable SPARK proof. Keep these limited
--  objects alive while connected: their aligned pages back grants. Publishers
--  may be destroyed after Disconnect reports Done.
--  Readers remain process-lived.
--  Calls on each object are serialized by its application event loop.
package CuBit.Logging is
   package Logstore_Handles renames CuBit.Handles.Services.Logstore;
   package Observer_Handles renames CuBit.Handles.Services.Log_Observer;

   --  A publisher exists only bound (CAP-003): declaring one needs one of
   --  the functions below, and the endpoint they take comes from the
   --  program's generated Startup package (its logstore function), so a
   --  publisher that drops every record because nobody bound it cannot be
   --  written.
   type Publisher (<>) is limited private;
   --  Records go to Target (the program's Startup logstore function).
   function Bound_To (Target : Logstore_Handles.Endpoint) return Publisher;
   --  The manifest declares logstore optional: when the launch had none,
   --  records are counted (Dropped) and go nowhere.
   function Bound_To (Target : Logstore_Handles.Optional_Endpoint) return Publisher;
   --  A program handed logstore at run time (a driver's grant from devmgr):
   --  no target until Attach; records emitted before it are counted.
   function Awaiting_Grant return Publisher;
   function Awaiting (Item : Publisher) return Boolean;
   procedure Attach (Item : in out Publisher; Target : Logstore_Handles.Endpoint)
   with Pre => Awaiting (Item);
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

   --  A reader exists only bound, like a publisher: Target is the
   --  program's Startup log-observer function.
   type Reader (<>) is limited private;
   function Bound_To (Target : Observer_Handles.Endpoint) return Reader;
   --  Declared optional: when the launch had none, Subscribe answers
   --  Unavailable.
   function Bound_To (Target : Observer_Handles.Optional_Endpoint) return Reader;
   --  Interactive synchronous operations. Never use these in interrupt or
   --  latency-sensitive service paths. Close ends the subscription and its
   --  channel; Subscribe again opens a new one.
   --  Minimum filters by severity inside logstore.
   --  Records at or above Minimum from Source (a process; Every_Source for
   --  all). A new subscription first replays matching retained history.
   --  Limit_Ms bounds each call to logstore this subscription makes (the
   --  first and every renewal): a reader that must not block on logstore
   --  (the Desktop bootstrap screen) gets Unavailable instead of waiting.
   No_Limit : constant Unsigned_64 := Unsigned_64'Last;
   procedure Subscribe
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Limit_Ms : Unsigned_64 := No_Limit);
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
      Target : CuBit.Handles.Endpoint);
   procedure Get_Minimum
     (Level : out CuBit.Log_Records.Severity; Result : out CuBit.Log_Protocol.Status;
      Target : CuBit.Handles.Endpoint);

   --  One record, through the process's own announcement publisher (its
   --  ring), then waiting briefly (Flush) until logstore has taken it: for
   --  startup and exit paths, where waiting is harmless. Result is OK
   --  (kept), Below_Minimum (taken, not kept), or Unavailable (no binding,
   --  shed, or not taken in time); Kept_From is the minimum logstore keeps.
   --  Target is the process's logstore endpoint; the announcement publisher
   --  binds to the first one given.
   procedure Publish_Now
     (Value : CuBit.Log_Records.Log_Record; Result : out CuBit.Log_Protocol.Status;
      Kept_From : out CuBit.Log_Records.Severity; Target : Logstore_Handles.Endpoint);

   procedure Announce
     (Text : String; Published : out Boolean; Target : Logstore_Handles.Endpoint;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
private
   type Writer_State is (Uninitialized, Ready, Disabled);
   type Publisher is limited record
      Slot : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      --  Made by Awaiting_Grant and not yet attached.
      Pending : Boolean := False;
      Link : CuBit.Channels.Channel;
      State : Writer_State := Uninitialized;
      Loss : Unsigned_64 := 0;
      Disconnecting : Boolean := False;
   end record;
   function Awaiting (Item : Publisher) return Boolean is (Item.Pending);
   type Reader is limited record
      Slot : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Link : CuBit.Channels.Channel;
      --  The filter, resent when the lease is renewed, and when it was.
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := CuBit.Log_Protocol.Every_Source;
      Renewed_Ms : Unsigned_64 := 0;
      Subscription : Unsigned_64 := 0;
      Limit_Ms : Unsigned_64 := No_Limit;
   end record;
end CuBit.Logging;
