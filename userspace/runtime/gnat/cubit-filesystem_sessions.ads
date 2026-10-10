------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A client's session on the filesystem's request queue
--  (CuBit.Filesystem_Queues, docs/filesystem-data-plane.md): the queue pair
--  and transfer arena opened once, requests submitted and answers reaped
--  without blocking, and the wake (docs/filesystem-protocol-v2.md step 1)
--  that lets the client's own event loop sleep until answers wait.
--
--  @description
--  Async first. Submit never waits, and Reap takes only what is there. To
--  sleep, a client arms the wake (Arm_Wake: OP_FS_WAKE as a submission with
--  the client's token), waits in its own loop on everything it waits for
--  (CuBit.Messages.Wait_For_Activity_Until, with its deadline), and hands
--  each completion-queue entry to Complete_Wake, which takes the wake's.
--  Wait_Answer is the blocking form: it spins briefly, then calls OP_FS_WAKE
--  until the caller's deadline (Wait_Forever only when spelled out).
--
--  Copy, then validate: Arena gives the shared arena, which the service
--  also writes; callers copy what they read from it before checking it.
--  No dirty arena: a session never holds a write delegation.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Async_Requests;
with CuBit.Channels;
with CuBit.Filesystem_Events;
with CuBit.Filesystem_Queues;
with CuBit.Messages;

package CuBit.Filesystem_Sessions is

   package FQ renames CuBit.Filesystem_Queues;
   package Q renames FQ.Queues;
   subtype Token is Q.Token;

   type Session is limited private;

   --  Open the queue on the filesystem behind Endpoint, with a transfer
   --  arena of Arena_Pages pages. A process has one queue at the service.
   procedure Open
     (S : in out Session; Endpoint : CuBit.Messages.CapabilitySlot;
      Arena_Pages : Positive; Opened : out Boolean)
   with Pre => Arena_Pages <= FQ.Transfer_Pages;
   --  Close the channels. A held wake is answered by the service then.
   procedure Close (S : in out Session);

   function Is_Open (S : Session) return Boolean;
   function Arena (S : Session) return System.Address;
   function Arena_Bytes (S : Session) return Unsigned_64;
   --  The regions (for platform glue that keeps its own ring state): this
   --  client's, which it writes, and the service's, read-only here.
   function Client_Region (S : Session) return System.Address;
   function Server_Region (S : Session) return System.Address;
   --  The service's namespace generation (FQ.Server_Namespace_At).
   function Namespace (S : Session) return Unsigned_32;

   --  Room for one more request (taking the service's consumed index).
   function Can_Submit (S : in out Session) return Boolean;
   --  Requests whose answer is not reaped yet.
   function Outstanding (S : Session) return Natural;
   --  Push and publish one request, choosing its tag; the service is
   --  kicked only when its wake word asks (once per arming).
   procedure Submit (S : in out Session; Item : FQ.Request; Tag : out Token)
   with Pre => Is_Open (S);
   --  Take one answer if one waits; never blocks. Got False: none waits
   --  (or the service sent one nobody asked for, which is dropped).
   procedure Reap (S : in out Session; Item : out Q.Completion; Got : out Boolean);
   function Answers_Waiting (S : in out Session) return Boolean;

   --  Change notifications (docs/filesystem-protocol-v2.md step 4): open the
   --  event ring once (after Open), then watch folders with Queue_Watch
   --  requests. Take_Event takes one record, copied and then checked
   --  (CuBit.Filesystem_Events.Decode): Malformed means the service wrote
   --  something that is not a record, which is never skipped silently.
   procedure Open_Events (S : in out Session; Opened : out Boolean);
   function Events_Open (S : Session) return Boolean;
   type Event_Result is (Taken, Empty, Malformed);
   procedure Take_Event
     (S : in out Session; Item : out CuBit.Filesystem_Events.Event;
      Name : out CuBit.Filesystem_Events.Name_Bytes;
      Length : out CuBit.Filesystem_Events.Name_Length; Result : out Event_Result);

   --  The wake: OP_FS_WAKE submitted with Wake_Token (process-wide unique,
   --  increasing; CuBit.Async_Requests). The service answers it once
   --  answers or event records wait (at once if some do). Accepted False: not sent (one is
   --  armed already, the token is not fresh, or the kernel refused it).
   function Wake_Armed (S : Session) return Boolean;
   procedure Arm_Wake
     (S : in out Session; Wake_Token : CuBit.Async_Requests.Token; Accepted : out Boolean);
   --  A completion-queue entry: Consumed when it answers the armed wake
   --  (then it is no longer armed); Woken when the service said answers
   --  wait. Consumed and not Woken: the queue ended, or the transport
   --  failed. Others' entries are left alone (Consumed False).
   procedure Complete_Wake
     (S : in out Session; Receipt : CuBit.Messages.CompletionEntry;
      Consumed, Woken : out Boolean);

   --  Block until an answer waits, or until Deadline (an absolute monotonic
   --  millisecond, or CuBit.Messages.Wait_Forever). Do not mix with an armed
   --  wake: the call supersedes it (its completion then says Woken).
   type Wait_Result is (Answer_Waiting, Deadline_Reached, Queue_Ended);
   procedure Wait_Answer
     (S : in out Session; Deadline : Unsigned_64; Result : out Wait_Result);

private
   type Session is limited record
      Endpoint  : CuBit.Messages.CapabilitySlot := 0;
      Transfer  : CuBit.Channels.Channel;
      Queue     : CuBit.Channels.Channel;
      Events    : CuBit.Channels.Channel;
      Opened    : Boolean := False;
      Client    : Q.Client;
      Next_Tag  : Token := 0;
      Kicked    : Unsigned_32 := 0;
      Wake      : CuBit.Async_Requests.Tracker;
   end record;
end CuBit.Filesystem_Sessions;
