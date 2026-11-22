------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A server's request channels (CAP-002 step 3, docs/capability-handles.md
--  section 4): admission by the server's own check and its declared
--  limits, priority between channels by class, and ending on the kernel's
--  grant notices.
--
--  @description
--  Configure with the offers the manifest declares (contract, per-client
--  and total limits, class). Handle_Open answers an open request: the
--  contract must be one offered, Admit (the server's check of the badge
--  and the kernel-reported sender) must say yes, and CuBit.Channel_Admission
--  must count it; otherwise a typed refusal (Unknown_Type, Not_Admitted,
--  Limit_Reached, No_Room, Bad_Grant) goes back.
--
--  The loop: Refresh after waking (rung grants and rings mark channels
--  ready), then Next for the channel to serve (CuBit.Channel_Schedule:
--  classes in order, round robin, each channel once per turn, a preemption
--  point after each batch); Take up to the server's budget; Complete each
--  request now or later (a request the server cannot serve yet stays owed in
--  the server, never in the kernel's reply slot); Served when the batch is
--  done. Arm before sleeping. Handle_Event ends a channel on its grant
--  notice: its limits are released and what it was owed is dropped.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Admission;
with CuBit.Channel_Contracts;
with CuBit.Channel_Limits;
with CuBit.Channel_Schedule;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Deadlines;
with CuBit.Doorbells;
with CuBit.Messages;
with CuBit.Request_Channels;
with System;

generic
   with package RC is new CuBit.Request_Channels (<>);
   Offer_Count : Positive;
   --  The server's own admission: whether From (the kernel-reported
   --  sender), having come through an endpoint with Badge, may open a
   --  channel of offer Offer.
   with function Admit (Offer : Positive; From : CuBit.Messages.Process_ID; Badge : Unsigned_64)
     return Boolean;
   --  The server's live capacity: whether it can serve one more channel of
   --  Offer now (a GPU context free, say). This is what Server_Limit means.
   with function Can_Serve_Another (Offer : Positive) return Boolean;
package CuBit.Channel_Servers is

   package Admission is new CuBit.Channel_Admission (Offer_Count);
   package Schedule is new CuBit.Channel_Schedule (CuBit.Channel_Limits.Channel_Class);

   subtype Offer_Index is Admission.Offer_Index;
   subtype Index is Schedule.Index;
   subtype Channel_Limit is Admission.Channel_Limit;

   type Offer is record
      Element    : CuBit.Channel_Contracts.Contract;   --  the protocol's schema
      Per_Client : Channel_Limit;
      Total      : Channel_Limit;
      Service    : CuBit.Channel_Limits.Channel_Class;
   end record;
   type Offer_Table is array (Offer_Index) of Offer;

   --  Channels the table holds before its first growth, and the step it
   --  grows by (it doubles, up to Schedule.Maximum_Size, as memory allows).
   Block_Size : constant := 64;

   type Server is limited private;

   --  False: no memory for the first block of the table.
   procedure Configure (S : in out Server; Offers : Offer_Table; Configured : out Boolean);

   --  Channels the table can hold now.
   function Size (S : Server) return Natural;

   --  An incoming request. Is_Open: it was an open (Reply answers it, sent
   --  by the caller); Opened: a channel at At_Index now exists.
   procedure Handle_Open
     (S : in out Server; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Reply : out CuBit.Messages.Message;
      Opened : out Boolean; At_Index : out Index);

   --  After waking: note rung grants and look at every channel's ring.
   procedure Refresh (S : in out Server);

   procedure Next (S : in out Server; Found : out Boolean; I : out Index);
   procedure Take (S : in out Server; I : Index; Item : out RC.Queues.Submission; Got : out Boolean);
   procedure Complete (S : in out Server; I : Index; Tag : RC.Token; Answer : RC.Result);
   procedure Served (S : in out Server; I : Index);

   --  Ring I's client now if it is owed a ring, instead of at the end of
   --  the turn (a completion someone waits on with a deadline: a present,
   --  a GPU job Desktop needs for its next frame).
   procedure Ring_Now (S : in out Server; I : Index);

   --  Arm the channels that need it before sleeping. False: work arrived.
   function Arm (S : in out Server) return Boolean;

   --  The end of a turn: arm the channels that need it; wake every client
   --  owed a wake (answers and room are signalled once per turn, not per
   --  entry, and only to clients asleep); then, if nothing arrived
   --  meanwhile, block in this process's ordinary receive until Deadline:
   --  one system call per wake. What it returns:
   --    Work_Waiting: work arrived while arming; nothing was received.
   --    Channels_Woken: a channel wake; Refresh looks at those channels.
   --    Message_Received: a request or another event (From, Msg), for the
   --      caller (opens: Handle_Open; grant ends: Handle_Event).
   --    Deadline_Passed, Closed.
   type Wake_Kind is (Work_Waiting, Channels_Woken, Message_Received, Deadline_Passed);
   type Wake is record
      Kind : Wake_Kind := Deadline_Passed;
      From : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Msg  : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   end record;
   procedure Sleep (S : in out Server; Deadline : CuBit.Deadlines.Deadline; Woke : out Wake);

   procedure Handle_Event (S : in out Server; Event : CuBit.Control_Events.Event;
                           Ended : out Boolean; At_Index : out Index);

   function Is_Active (S : Server; I : Index) return Boolean;
   function Client_Of (S : Server; I : Index) return CuBit.Messages.Process_ID;
   function Offer_Of (S : Server; I : Index) return Offer_Index;
   function Owed (S : Server; I : Index) return Natural;

   --  Refusals by kind, for the server's log. Limit: a declared At_Most
   --  limit; Exhausted: the server's own capacity (Server_Limit) or memory
   --  for a larger table. Both answer the client Limit_Reached.
   type Refusal_Counts is record
      Unknown, Not_Admitted, Limit, Exhausted, Bad_Grant : Unsigned_64 := 0;
   end record;
   function Refusals (S : Server) return Refusal_Counts;

private
   Bells_Per_Wake : constant := CuBit.Doorbells.ABI.Wakes_Per_Event;
   Maximum_Blocks : constant := Schedule.Maximum_Size / Block_Size;
   type Block_Directory is array (1 .. Maximum_Blocks) of System.Address;

   type Connection is limited record
      Active  : Boolean := False;
      Channel : CuBit.Channels.Channel;
      Ring    : RC.Server;
      Client  : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Offer   : Offer_Index := Offer_Index'First;
      Routed  : Boolean := False;
      --  The client asked to be rung (answers, or room); rung at Sleep.
      Ring_Owed : Boolean := False;
      --  Its waiting word must be armed again before the next sleep: it is
      --  new, was served, or its client rang (and so used the last arming).
      --  Channels nothing touched keep their arming: a sleep costs only
      --  the channels that were active.
      Needs_Arm : Boolean := True;
   end record;
   type Server is limited record
      Offers  : Offer_Table;
      --  Size channels: the connection blocks, and the admission and
      --  schedule states (Admission.State (Size), Schedule.State (Size))
      --  in memory from the process heap, replaced when the table grows.
      Size    : Natural := 0;
      Blocks  : Block_Directory := [others => System.Null_Address];
      Limits_At, Order_At : System.Address := System.Null_Address;
      Counts  : Refusal_Counts;
      Started : Boolean := False;
      --  The grants the last wake event named (Refresh looks only at their
      --  channels when Wake_Bells_Valid).
      Wake_Bells : CuBit.Doorbells.Bell_Array (1 .. Bells_Per_Wake);
      Wake_Bell_Count : CuBit.Doorbells.Wake_Count := 0;
      Wake_Bells_Valid : Boolean := False;
   end record;
end CuBit.Channel_Servers;
