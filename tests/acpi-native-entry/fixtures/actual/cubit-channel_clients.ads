------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A client's request channel to a server (CAP-002 step 3,
--  docs/capability-handles.md section 4): open with an explicit deadline,
--  submit without blocking, wait for an answer with an explicit deadline.
--
--  @description
--  The channel is a CuBit.Channels duplex in the standard request layout
--  (CuBit.Request_Channels): the client's region holds its requests, the
--  server's its answers. Each request carries a tag (CuBit.Request_Tags):
--  tags only grow, every tag is answered once or failed when the channel
--  ends, and an answer with an unknown or repeated tag is dropped and
--  counted. Waking: the server is rung through the client's region grant
--  only when it sleeps; a waiting client sleeps on the server region's
--  wait for the process's activity (the server's channel wake), so no
--  reply is held anywhere.
--
--  Threads: any number may submit and wait on one Client. One waiting
--  thread at a time sleeps on the server's doorbell (the pump); it reaps
--  every answer that arrives, files each under its tag, and wakes the
--  other waiters (a futex event in this process), which find their own
--  answers or take the pump over. A lone thread pays nothing for this
--  beyond an uncontended lock. Drivers never see threads.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Deadlines;
with CuBit.Futex_Locks;
with CuBit.Handles;
with CuBit.Request_Channels;
with CuBit.Request_Tags;

generic
   with package RC is new CuBit.Request_Channels (<>);
   --  The element of the protocol's duplex contract (its schema).
   Element : CuBit.Channel_Contracts.Contract;
package CuBit.Channel_Clients is

   --  The duplex contract an open sends: the client's region and the
   --  server's, in RC's layout.
   function Channel_Contract return CuBit.Channel_Contracts.Contract;

   package Tags is new CuBit.Request_Tags (Capacity => RC.Queues.Completion_Slots);
   subtype Tag is Tags.Tag;

   type Client is limited private;

   type Open_Outcome is (Connected, Refused, Limit_Reached, Timed_Out, Failed);
   procedure Open
     (C : in out Client; Endpoint : CuBit.Handles.Endpoint;
      Deadline : CuBit.Deadlines.Deadline;
      Outcome : out Open_Outcome; Refusal : out CuBit.Channel_Protocol.Open_Refusal);

   function Is_Open (C : Client) return Boolean;

   --  Why a channel ended.
   type End_Reason is (Open_Channel, Closed, Server_Ended);
   function Reason (C : Client) return End_Reason;

   procedure Close (C : in out Client);

   type Submit_Result is (Queued, Full, Ended);
   procedure Submit (C : in out Client; Item : RC.Request; Number : out Tag;
                     Result : out Submit_Result);

   --  Reap every answer waiting (never blocks).
   procedure Poll (C : in out Client);

   type Wait_Outcome is (Answered, Timed_Out, Ended);
   --  The answer to Number: reaps, then declares itself asleep and waits
   --  until Deadline (no spinning: the server's channel wake wakes it).
   --  A strict request-then-wait belongs on a synchronous call; channels
   --  carry batches, streams and completions.
   procedure Wait (C : in out Client; Number : Tag; Deadline : CuBit.Deadlines.Deadline;
                   Answer : out RC.Result; Outcome : out Wait_Outcome);

   --  A kernel grant event: Ends tells whether it ended this channel (its
   --  outstanding requests then fail Ended).
   procedure Handle_Event (C : in out Client; Event : CuBit.Control_Events.Event;
                           Ends : out Boolean);

   --  Answers dropped because their tag was unknown or already answered.
   function Stray_Answers (C : Client) return Unsigned_64;

private
   type Answer_Array is array (Tags.Entry_Index) of RC.Result;
   type Client is limited record
      Channel : CuBit.Channels.Channel;
      Ring    : RC.Client;
      Table   : Tags.Table;
      Answers : Answer_Array;
      Why     : End_Reason := Closed;
      Routed  : Boolean := False;
      Strays  : Unsigned_64 := 0;
      --  Threads: the state above is changed only under Guard; Pumping: a
      --  waiter sleeps on the doorbell; Followers: waiters sleeping on
      --  Answers_Ready, which the pump signals after reaping.
      Guard     : CuBit.Futex_Locks.Mutex;
      Answers_Ready : CuBit.Futex_Locks.Event;
      Pumping   : Boolean := False;
      Followers : Natural := 0;
   end record;
end CuBit.Channel_Clients;
