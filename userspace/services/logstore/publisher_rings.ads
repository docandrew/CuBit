pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Log_Records;
with CuBit.Messages;
with Log_Fanout;
with Log_Budgets;
--  logstore's side of the publishers' log channels (CuBit.Log_Publish_Rings,
--  docs/logstore-architecture.md step 1, docs/data-plane.md): each channel a
--  publisher opens is accepted here, and its records are drained into the
--  broker in batches. Identity (source, authority) comes from the opener,
--  never from the channel.
--
--  A channel ends when its publisher closes it, exits or dies: the kernel's
--  grant events say so (Ended). Its records are drained first, so what a
--  publisher wrote just before it ended is kept.
package Publisher_Rings is
   --  Channels come in chunks of Chunk_Rings, allocated (CuAlloc) as
   --  publishers open them, up to the named ceiling Maximum_Publishers
   --  (numbers stay below Source_Registrars.First_Number). A live channel
   --  is never let go for a new one (no lost records): at the ceiling, or
   --  without memory for a chunk, the open is refused (No_Room), and
   --  logstore says so in its own record; the publisher keeps its lines
   --  queued and counts what it cannot keep.
   Chunk_Rings : constant := 64;
   Maximum_Chunks : constant := 64;
   Maximum_Publishers : constant := Chunk_Rings * Maximum_Chunks;
   --  Records taken from one channel per pass, so one busy publisher cannot
   --  hold the loop.
   Batch_Records : constant := 256;

   type Table is limited private;

   --  Answer From's request to open a publishing channel (Authority: the
   --  kernel-stamped tag it came with), replacing any channel the same
   --  publisher had; refused (No_Room) when no entry is free and no chunk
   --  can be added. Reply is the reply to send.
   procedure Open
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message);

   --  Drain and end the channel Request (From's OP_CLOSE) names, if From
   --  opened it.
   procedure Close
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64);

   --  A grant event: drain and end the channel it belongs to, if any.
   procedure Ended
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Event : CuBit.Control_Events.Event; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64);

   --  Take records from every channel into Store (those at Minimum and
   --  above, within the publisher's budget), publish what was shed, and
   --  publish Minimum to each publisher. Pending: some channel still holds
   --  records.
   procedure Drain
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64; Pending : out Boolean);

   --  Tell publishers their drained records are done: after the pass has
   --  written them to the readers' streams, so a publisher's Flush means
   --  delivered.
   procedure Release (Item : in out Table);

   --  Console text (CuBit.Console_Lines) a publisher wrote, as it is
   --  taken: test builds' logstore mirrors it onto the serial console
   --  (Test_Console_ABI). Once set, every record is mirrored that is
   --  console text, whatever the minimum, and publishers are told the
   --  minimum is Trace, so they send it all; only records at the minimum
   --  and above are kept, as before.
   type Console_Mirror is access procedure
     (Owner : CuBit.Messages.Process_ID; Value : CuBit.Log_Records.Log_Record);
   procedure Set_Mirror (Mirror : not null Console_Mirror);

   --  Before sleeping: ask every publisher for a kick, then look again.
   --  Pending: records arrived meanwhile, so do not sleep.
   procedure Arm (Item : in out Table; Pending : out Boolean);
   --  Awake: no kicks wanted.
   procedure Disarm (Item : in out Table);
private
   type Ring is record
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Authority : Unsigned_64 := 0;
      Link : CuBit.Channels.Channel;
      Shed_Seen : Unsigned_64 := 0;
      Active_Ms : Unsigned_64 := 0;
      Unreleased : Boolean := False;   --  taken, not yet released
   end record;
   type Chunk is array (1 .. Chunk_Rings) of aliased Ring;
   type Chunk_Access is access Chunk;
   type Chunk_Array is array (1 .. Maximum_Chunks) of Chunk_Access;
   type Table is record
      Chunks : Chunk_Array;
      Used : Natural range 0 .. Maximum_Chunks := 0;   --  chunks allocated
   end record;
end Publisher_Rings;
