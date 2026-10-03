with Interfaces; use Interfaces;
with System;
with CuBit.Memory_Grants;
with CuBit.Channel_Rings;
with Log_Fanout;

--  logstore's side of the readers' log streams (CuBit.Log_Streams): each
--  subscription's region stays mapped while it lives, and its queued events
--  are written into the region's ring as the reader makes room. The broker
--  (Log_Fanout) still decides what each reader gets and what it lost; this
--  only moves events from its queues into shared memory.
package Stream_Writers is
   type Table is limited private;
   --  Map Ref (Owner's writable stream region) for subscription Handle. A
   --  repeated Subscribe with the same region keeps the stream as it is.
   procedure Attach
     (Item : in out Table; Handle, Owner, Authority : Unsigned_64;
      Ref : CuBit.Memory_Grants.Grant_Reference; Attached : out Boolean);
   --  Stop writing and return the region (before Close's reply).
   procedure Detach (Item : in out Table; Handle : Unsigned_64);
   --  Write queued events into every stream while its ring has room, and
   --  return the regions of subscriptions that ended. Backlog: some stream
   --  ran out of room, so call again soon.
   procedure Drain (Item : in out Table; Store : in out Log_Fanout.Broker; Backlog : out Boolean);
private
   type Stream is record
      Handle, Owner, Authority : Unsigned_64 := 0;
      Ref : CuBit.Memory_Grants.Grant_Reference;
      Base : System.Address := System.Null_Address;
      Producer : CuBit.Channel_Rings.Producer;
   end record;
   type Table is array (1 .. Log_Fanout.Maximum_Subscribers) of Stream;
end Stream_Writers;
