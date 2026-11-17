------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  WAIT (ABI-002 slice 2): the one entry point that waits. Shared by the
--  kernel (Wait_Policy, Syscall.IPC) and the runtime (CuBit.Waits).
--
--  @description
--  Wait (Sources, Deadline, A, B, C) waits for one of a typed set of
--  sources until an explicit deadline. It replaces RECEIVE,
--  RECEIVE_UNTIL_MONOTONIC_MILLISECOND, POLL_ANY_IPC, RECEIVE_EVENT_NB,
--  WAIT_COMPLETION, POLL_COMPLETION,
--  WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND,
--  POLL_SERVICE_REQUEST, SLEEP, SLEEP_UNTIL_MONOTONIC_MICROSECOND,
--  FUTEX_WAIT and ACCEPT_OFFERED_CAPABILITY.
--
--  Registers: arg0 the Sources word (Source bits, plus the modifiers
--  Readiness_Only and Wake_First), arg1 the deadline, arg2 .. arg4 the
--  source's parameters A, B, C, arg5 the wake operand.
--
--  Wake_First and arg5 are reserved for IPC-010 (wake-plus-wait, the
--  futex counterpart of Reply_Wait): with Wake_First set, the kernel will
--  wake the futex arg5 names (a word address now; a descriptor for a
--  shared futex, grant and offset, later) before it waits on the sources,
--  in one call. Today every Sources word with Wake_First is refused
--  (Unsupported_Form) and arg5 is ignored without it, so adding it
--  renumbers and reshapes nothing.
--
--  Deadline: an absolute monotonic millisecond (the clock of Call and
--  CuBit.Deadlines), Now (0: look once, never block) or Forever
--  (Unsigned_64'Last). There is no default.
--
--  The forms (any other combination is refused, Unsupported_Form):
--
--    Messages  {Requests, Events}: take one message, request or event,
--              whichever the mailbox's fair order gives. A: the caller's
--              Message (48 bytes). Result: the sender's identity word,
--              or Nothing when the deadline passed with none.
--    Requests  {Requests}, deadline Now: take one service request (a
--              synchronous call, an asynchronous request, a one-way
--              message); never an event. A: Message. Result as Messages.
--    Events    {Events}, deadline Now: take one event (a kernel notice, a
--              device doorbell, a published event). A: Message; B: where
--              to write the publisher's identity word (0: not wanted).
--              Result: Taken or Nothing.
--    Completions {Completions}: take up to B completion entries into A
--              (CompletionEntry each); with deadline Forever, block until
--              at least C are there (C <= B). With Now, C must be 0.
--              Result: how many were taken.
--    Ready     {Requests, Events, Completions} + Readiness_Only: wait
--              without taking a request or a completion. With A /= 0, a
--              pending kernel notice is taken into A (a Message) in the
--              same call. Result: a Readiness value.
--    Futex     {Futex_Word}: sleep while the 32-bit word at A holds B.
--              Result: Futex_Woken, Futex_Retry, Futex_Timed_Out or
--              Futex_Fault.
--    Offered   {Offered_Capability}, deadline Now: take the capability the
--              current caller (identity word A) offered with its call.
--              Result: a handle word or a Capability_ABI refusal code.
--    Timer     {Timer}, deadline Forever: sleep until the absolute
--              monotonic microsecond A (READ_MONOTONIC_MICROSECONDS's
--              clock). A time already passed returns at once. Result 0.
--
--  Every form that takes a message replaces the thread's reply authority
--  exactly as the receive it replaces did (Process.IPC.receive).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wait_ABI with Pure, SPARK_Mode is

   System_Call_Number : constant := 149;

   type Source is
     (Requests, Events, Completions, Futex_Word, Offered_Capability, Timer);

   --  Each source's bit in the Sources word (bit Source'Pos).
   Requests_Bit           : constant := 2 ** 0;
   Events_Bit             : constant := 2 ** 1;
   Completions_Bit        : constant := 2 ** 2;
   Futex_Word_Bit         : constant := 2 ** 3;
   Offered_Capability_Bit : constant := 2 ** 4;
   Timer_Bit              : constant := 2 ** 5;
   Readiness_Only         : constant := 2 ** 8;
   --  Reserved (IPC-010): wake arg5's futex first. Refused today.
   Wake_First             : constant := 2 ** 9;

   function Bit (S : Source) return Unsigned_64 is
     (case S is
         when Requests           => Requests_Bit,
         when Events             => Events_Bit,
         when Completions        => Completions_Bit,
         when Futex_Word         => Futex_Word_Bit,
         when Offered_Capability => Offered_Capability_Bit,
         when Timer              => Timer_Bit)
   with Post => Bit'Result = 2 ** Source'Pos (S);

   --  Deadlines.
   Now     : constant Unsigned_64 := 0;
   Forever : constant Unsigned_64 := Unsigned_64'Last;

   --  The Sources words of the forms.
   Messages_Sources    : constant := Requests_Bit + Events_Bit;
   Requests_Sources    : constant := Requests_Bit;
   Events_Sources      : constant := Events_Bit;
   Completions_Sources : constant := Completions_Bit;
   Ready_Sources       : constant :=
     Requests_Bit + Events_Bit + Completions_Bit + Readiness_Only;
   Futex_Sources       : constant := Futex_Word_Bit;
   Offered_Sources     : constant := Offered_Capability_Bit;
   Timer_Sources       : constant := Timer_Bit;

   --  Results.
   --  Messages / Requests: nothing arrived before the deadline.
   Nothing : constant Unsigned_64 := Unsigned_64'Last;
   --  Events: an event was taken.
   Taken   : constant Unsigned_64 := 1;
   No_Event : constant Unsigned_64 := 0;
   --  Ready.
   Ready_Deadline : constant Unsigned_64 := 0;
   Ready_Work     : constant Unsigned_64 := 1;
   Ready_Event    : constant Unsigned_64 := 2;
   Ready_Closed   : constant Unsigned_64 := Unsigned_64'Last;
   --  Futex (as Process.Futex.wait answers).
   Futex_Woken     : constant Unsigned_64 := 0;
   --  The word did not hold B: recheck it and retry.
   Futex_Retry     : constant Unsigned_64 := 1;
   Futex_Timed_Out : constant Unsigned_64 := 2;
   --  Unaligned, not user RAM, or not mapped.
   Futex_Fault     : constant Unsigned_64 := Unsigned_64'Last;
   --  A form the kernel does not offer, or a malformed argument.
   Unsupported_Form : constant Unsigned_64 := Unsigned_64'Last - 1;

end Wait_ABI;
