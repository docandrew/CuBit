------------------------------------------------------------------------------
--  The ipctest channel protocol (CAP-002 step 3, the async-ipc channel
--  markers): a request carries a value, the answer the value times three.
--  Shared by ipctest-server and ipctest-client.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Request_Channels;

package IPCtest_Channel is

   --  Control-plane calls around a session: the server serves channels
   --  between them.
   OP_CHANNEL_SESSION : constant Unsigned_32 := 16#0918#;
   OP_CHANNEL_END     : constant Unsigned_32 := 16#0919#;
   --  During a session: the system calls the server's thread made so far.
   OP_CHANNEL_COUNT   : constant Unsigned_32 := 16#091A#;
   --  During a session: a synchronous call answered at once (the same
   --  run's reference for a channel round trip).
   OP_CHANNEL_PING    : constant Unsigned_32 := 16#091B#;
   --  During a session: the event-loop wake samples so far, as words 0..2
   --  min, median and 99th percentile (TSC ticks); the samples are cleared.
   OP_CHANNEL_STATS   : constant Unsigned_32 := 16#091C#;
   --  During a session: the saved-reply path for the same probe, a call the
   --  server saves, holds Hold_Ms, then answers with the TSC (word 0).
   OP_CHANNEL_DELAYED : constant Unsigned_32 := 16#091D#;
   --  Samples kept per probe.
   Samples : constant := 256;

   type Request is record
      Value : Unsigned_64 := 0;
      Kind  : Unsigned_64 := 0;
   end record;
   type Result is record
      Value  : Unsigned_64 := 0;
      Echoed : Unsigned_64 := 0;
   end record;

   --  Request kinds.
   Echo    : constant Unsigned_64 := 1;
   --  Held by the server until a Release (the client's wait times out).
   Silent  : constant Unsigned_64 := 2;
   Release : constant Unsigned_64 := 3;

   --  Latency probes (TSC ticks, the same clock in both processes):
   --  Delayed_Stamp: the server holds it Hold_Ms (the client is asleep in
   --  its wait by then), then answers with the TSC read as it publishes the
   --  answer; the client subtracts on waking (publish to running, for a
   --  thread sleeping on the doorbell).
   --  Wake_Stamp: the client stamps the TSC as it submits to a sleeping
   --  server; the server subtracts as it takes it (submit to running, for
   --  an event loop) and keeps the sample for OP_CHANNEL_STATS.
   Delayed_Stamp : constant Unsigned_64 := 4;
   Wake_Stamp    : constant Unsigned_64 := 5;
   --  As Delayed_Stamp, but the server rings at once (Ring_Now) and stamps
   --  just before the ring: ring to running, the producer going on running.
   Ring_Stamp    : constant Unsigned_64 := 6;
   Hold_Ms       : constant := 2;

   Multiplier : constant Unsigned_64 := 3;

   package RC is new CuBit.Request_Channels
     (Request, Result, Request_Bits => 6, Completion_Bits => 5);

   Schema : constant CuBit.Protocols.Schema_Id := 16#1BC7_E57C_4A2E_0001#;
   Submission_Bytes : constant := 24;   --  token and request

   Element : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Submission_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => RC.Client_Pages,
      Buffers => RC.Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  The server's declared limits for this test: At_Most Per_Client
   --  channels per client (past the table's first block of 64, so the
   --  table grows), Server_Limit in total.
   Per_Client : constant := 66;

   --  Session report words (OP_CHANNEL_END's reply).
   Report_Served   : constant := 0;
   Report_Limit    : constant := 1;
   Report_Ended    : constant := 2;
   --  System calls the server's thread made during the session.
   Report_Server_Calls : constant := 3;

end IPCtest_Channel;
