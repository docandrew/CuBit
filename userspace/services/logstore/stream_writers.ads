with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Messages;
with Log_Fanout;

--  logstore's side of the readers' log streams (CuBit.Log_Streams): each
--  stream is a channel a reader opened, consuming; logstore produces into
--  it. Subscribe binds a subscription to its reader's channel, and queued
--  events are written into it as the reader makes room. The broker
--  (Log_Fanout) still decides what each reader gets and what it lost; this
--  only moves events from its queues into shared memory.
package Stream_Writers is
   --  Stream channels are numbered from here, apart from publishers'.
   First_Number : constant := 16#1_0000#;

   type Table is limited private;

   --  Answer an observer's request to open a stream channel (From, with
   --  Authority). Reply is the reply to send.
   procedure Open
     (Item : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);

   --  Bind subscription Handle to From's stream channel Number (Subscribe).
   --  Bound False: From has no such channel. Binding again (a renewal)
   --  keeps the stream as it is.
   procedure Bind
     (Item : in out Table; Number, Handle : Unsigned_64; From : CuBit.Messages.Process_ID;
      Authority : Unsigned_64; Bound : out Boolean);

   --  The subscription ended (Close): stop writing for it.
   procedure Unbind (Item : in out Table; Handle : Unsigned_64);

   --  From closed one of its channels (Request, its OP_CLOSE).
   procedure Close (Item : in out Table; From : CuBit.Messages.Process_ID;
                    Request : CuBit.Messages.Message);

   --  A grant event: the channel it ends is let go.
   procedure Ended (Item : in out Table; Event : CuBit.Control_Events.Event);

   --  Write queued events into every bound stream while its ring has room.
   --  Backlog: some stream ran out of room, so call again soon.
   procedure Drain (Item : in out Table; Store : in out Log_Fanout.Broker; Backlog : out Boolean);
private
   type Stream is record
      Handle, Authority : Unsigned_64 := 0;   --  Handle 0: unbound
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Link : CuBit.Channels.Channel;
   end record;
   type Table is array (1 .. Log_Fanout.Maximum_Subscribers) of Stream;
end Stream_Writers;
