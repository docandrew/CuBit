------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A client's GPU session queue (CuBit.GPU_Queues, docs/gpu-async-submission.md,
--  GPU-001 step 2): the queue opened once on the Intel GPU driver's endpoint,
--  jobs submitted and records reaped without blocking, and the wake
--  (OP_GPU_WAKE) that lets the client's own event loop sleep until a context
--  reaches a value. The shared-memory half is CuBit.GPU_Queue_Clients.
--
--  @description
--  Async first. Submit never waits, and Reap takes only what is there. To
--  sleep, a client arms the wake (Arm_Wake: OP_GPU_WAKE as a submission with
--  the client's token), waits in its own loop on everything it waits for
--  (CuBit.Messages.Wait_For_Activity_Until, with its deadline), and hands
--  each completion-queue entry to Complete_Wake. Wait_Reached is the
--  blocking form: it spins briefly, then calls OP_GPU_WAKE until the
--  caller's deadline (Wait_Forever only when spelled out). The driver holds
--  one wake at a time: when it is held for another session the answer is
--  Not_Held and Wait_Reached sleeps one millisecond between looks instead.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Async_Requests;
with CuBit.Channels;
with CuBit.GPU_Queue_Clients;
with CuBit.GPU_Queues;
with CuBit.Messages;

package CuBit.GPU_Sessions is

   package GQ renames CuBit.GPU_Queues;
   package Clients renames CuBit.GPU_Queue_Clients;

   type Session is limited private;

   --  Open the queue on the GPU driver behind Endpoint (the session's
   --  contexts must be registered).
   procedure Open
     (S : in out Session; Endpoint : CuBit.Messages.CapabilitySlot; Opened : out Boolean);
   --  Close the channel. A held wake is answered by the driver then.
   procedure Close (S : in out Session);
   function Is_Open (S : Session) return Boolean;

   --  The shared-memory client (status, Next_Signal, Reached, Reap).
   function Can_Submit (S : in out Session) return Boolean;
   function Next_Signal (S : Session; Context : GQ.Context_Index) return GQ.Timeline_Value;
   function Reached (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value)
     return Boolean;
   function State (S : Session; Context : GQ.Context_Index) return GQ.Context_State;
   --  A consistent copy of a context's status line (OK False: not open, or
   --  the driver kept rewriting it).
   procedure Read_Status
     (S : Session; Context : GQ.Context_Index; Line : out GQ.Status_Line; OK : out Boolean);

   --  Write and publish one job; the driver is kicked only when its wake
   --  word asks (once per arming). Submitted False: the queue is full.
   procedure Submit
     (S : in out Session; Item : Clients.Job; Tag : out Clients.Token;
      Signal : out GQ.Timeline_Value; Submitted : out Boolean);
   procedure Reap (S : in out Session; Item : out Clients.Q.Completion; Got : out Boolean);

   --  The wake: OP_GPU_WAKE for Context and Target submitted with Wake_Token
   --  (process-wide unique, increasing). Accepted False: not sent (one is
   --  armed already, the token is not fresh, or the kernel refused it).
   function Wake_Armed (S : Session) return Boolean;
   procedure Arm_Wake
     (S : in out Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Wake_Token : CuBit.Async_Requests.Token; Accepted : out Boolean);
   --  A completion-queue entry: Consumed when it answers the armed wake;
   --  Result says what the driver answered (Woken: look at the status line).
   procedure Complete_Wake
     (S : in out Session; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean; Result : out GQ.Wake_Result);

   --  Block until Context reaches Target, the context fails, or Deadline
   --  (an absolute monotonic millisecond, or CuBit.Messages.Wait_Forever).
   --  Spins briefly, then sleeps in OP_GPU_WAKE. When the driver's wake is
   --  held for another session (Not_Held) it goes on as Poll_Reached: no
   --  further calls.
   type Wait_Result is (Reached_Target, Context_Failed, Deadline_Reached, Queue_Ended);
   procedure Wait_Reached
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Deadline : Unsigned_64; Result : out Wait_Result);
   --  The same wait without any call: the status line, read every
   --  Poll_Pause_Ms. For a second waiter on a session whose one wake another
   --  thread holds, and after Not_Held.
   Poll_Pause_Ms : constant := 1;
   procedure Poll_Reached
     (S : Session; Context : GQ.Context_Index; Target : GQ.Timeline_Value;
      Deadline : Unsigned_64; Result : out Wait_Result);

private
   type Session is limited record
      Endpoint : CuBit.Messages.CapabilitySlot := 0;
      Queue    : CuBit.Channels.Channel;
      Opened   : Boolean := False;
      Client   : Clients.Client;
      Wake     : CuBit.Async_Requests.Tracker;
   end record;
end CuBit.GPU_Sessions;
