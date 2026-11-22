------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The standard request/completion channel (CAP-002 step 3,
--  docs/capability-handles.md section 4): a client's requests and a
--  server's completions in two regions the sides share over grants, the
--  layout every protocol instantiates (it generalises CuBit.GPU_Queues').
--
--  @description
--  The producer owns: the client writes only its region (its request ring
--  and its indices and waiting words) and the server only its own (the
--  completion ring, its indices, its waiting word and a protocol status
--  block); each maps the other's read-only. Each side wakes the other by
--  ringing its own region's grant (the glue does: this unit only says when,
--  Ring_Peer), and only when the peer armed its waiting word, once per
--  arming. Rules are CuBit.Submission_Queues' (proved): the server takes a
--  request only with a completion slot reserved, so every taken request is
--  answered exactly once and a completion is never dropped; a full request
--  ring is back pressure on that client alone.
--
--  No system call here: the same code runs in a process and in a hosted
--  test. Everything read from the peer's region is copied, then the caller
--  validates it (rule 1 of docs/data-plane.md).
--
--  Region layout (byte offsets; each writer's word on its own line):
--    client: 0 Submitted, 64 Reaped, 128 Client_Waiting, 192 Room_Waiting,
--            256 .. 1023 protocol client state; page 1.. the request ring
--    server: 0 Completed, 64 Taken, 128 Server_Waiting,
--            1024 .. 4095 protocol status block; page 1.. the completion ring
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Submission_Queues;

generic
   type Request is private;
   type Result is private;
   Request_Bits    : Natural;   --  2 ** Request_Bits request slots
   Completion_Bits : Natural;   --  2 ** Completion_Bits completion slots
package CuBit.Request_Channels is

   package Queues is new CuBit.Submission_Queues (Request, Result, Request_Bits, Completion_Bits);
   subtype Token is Queues.Token;

   Page_Bytes : constant := 4_096;
   Line_Bytes : constant := 64;

   Submitted_At      : constant := 0;
   Reaped_At         : constant := 64;
   Client_Waiting_At : constant := 128;
   Room_Waiting_At   : constant := 192;
   Client_State_At   : constant := 256;
   Client_State_Last : constant := 1_023;

   Completed_At      : constant := 0;
   Taken_At          : constant := 64;
   Server_Waiting_At : constant := 128;
   Status_At         : constant := 1_024;
   Status_Last       : constant := 4_095;

   Ring_At : constant := Page_Bytes;

   Request_Ring_Bytes    : constant Natural := Queues.Submissions.Ring'Size / 8;
   Completion_Ring_Bytes : constant Natural := Queues.Completions.Ring'Size / 8;

   --  Pages of each region: the header page and the ring.
   Client_Pages : constant Positive := 1 + (Request_Ring_Bytes + Page_Bytes - 1) / Page_Bytes;
   Server_Pages : constant Positive := 1 + (Completion_Ring_Bytes + Page_Bytes - 1) / Page_Bytes;

   ---------------------------------------------------------------------------
   --  The client's half.
   ---------------------------------------------------------------------------
   type Client is limited private;

   procedure Attach_Client (C : in out Client; Own_Region, Server_Region : System.Address);
   procedure Detach_Client (C : in out Client);
   function Client_Attached (C : Client) return Boolean;

   --  Room for one more request (taking the server's consumed index).
   function Can_Submit (C : in out Client) return Boolean;
   function Outstanding (C : Client) return Natural;

   --  Write and publish one request. Submitted False: no room. Ring_Peer:
   --  the server sleeps and asked to be rung (once per arming).
   procedure Submit (C : in out Client; Tag : Token; Item : Request;
                     Submitted, Ring_Peer : out Boolean);

   --  One completion, copied, if one waits; never blocks.
   procedure Reap (C : in out Client; Item : out Queues.Completion; Got : out Boolean);
   function Completions_Waiting (C : in out Client) return Boolean;

   --  Before sleeping for completions: arm. False: one arrived meanwhile
   --  (do not sleep). Disarm after waking.
   function Arm_For_Completions (C : in out Client) return Boolean;
   procedure Disarm_For_Completions (C : Client);
   --  Before sleeping for room in the request ring.
   function Arm_For_Room (C : in out Client) return Boolean;
   procedure Disarm_For_Room (C : Client);

   --  The protocol's client state words (bytes 256 .. 1023 of the client's
   --  region): this side writes them.
   function Client_State (C : Client) return System.Address;
   --  The server's status block (read-only here; copy before use).
   function Server_Status (C : Client) return System.Address;

   ---------------------------------------------------------------------------
   --  The server's half.
   ---------------------------------------------------------------------------
   type Server is limited private;

   procedure Attach_Server (S : in out Server; Own_Region, Client_Region : System.Address);
   procedure Detach_Server (S : in out Server);
   function Server_Attached (S : Server) return Boolean;

   --  A request waits and its answer has a reserved slot.
   function Can_Take (S : in out Server) return Boolean;
   --  Answers owed (taken, not yet completed).
   function Owed (S : Server) return Natural;

   --  Copy the next request out and owe its answer (the caller validates
   --  the copy). Ring_Peer: the client waits for room and asked to be rung.
   procedure Take (S : in out Server; Item : out Queues.Submission; Got, Ring_Peer : out Boolean);

   --  Answer one taken request; it always has room. Ring_Peer: the client
   --  sleeps for completions and asked to be rung.
   procedure Complete (S : in out Server; Tag : Token; Answer : Result; Ring_Peer : out Boolean)
   with Pre => Owed (S) > 0;

   --  Before sleeping for requests.
   function Arm_For_Requests (S : in out Server) return Boolean;
   procedure Disarm_For_Requests (S : Server);

   function Status_Block (S : Server) return System.Address;
   function Peer_Client_State (S : Server) return System.Address;

private
   type Client is limited record
      Own, Peer : System.Address := System.Null_Address;
      Ring : Queues.Client;
      Rung_Arming, Arming, Room_Arming : Unsigned_32 := 0;
   end record;
   type Server is limited record
      Own, Peer : System.Address := System.Null_Address;
      Ring : Queues.Server;
      Rung_Arming, Rung_Room, Arming : Unsigned_32 := 0;
   end record;
end CuBit.Request_Channels;
