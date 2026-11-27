------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Notifications (IPC revision 2, S1; Notification_ABI): a word of bits and
--  one waiter, signalled through badged Wake handles; and private futex
--  words, waited on by a process's own threads.
--
--  @description
--  Create gives a notification charged to this process and its one Wait
--  handle (never copied). Mint_Waker gives a Wake handle that carries a
--  badge for good; pass it on (with Grant) to whoever should signal. Wake
--  ORs that badge into the notification; Wait takes every pending bit.
--  Bind_Leaf makes a notification signal a root with the root Waker's
--  badge (two levels only). Bind_Thread binds a root to the calling
--  thread: until the endpoint receive of S2, a signal with no waiter rings
--  this process's doorbell (one Channel_Wake_ABI notice in its receive).
--  Closing the Wait handle ends the notification.
--
--  Deadlines are absolute monotonic microseconds (CuBit.Monotonic), Now or
--  Forever, spelled out.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Notification_ABI;

package CuBit.Handles.Notifications is

   package ABI renames Notification_ABI;

   type Waitable is private;   --  the Wait handle: one per notification
   type Waker is private;      --  a Wake handle, with its badge
   No_Waitable : constant Waitable;
   No_Waker    : constant Waker;
   function Valid (Item : Waitable) return Boolean;
   function Valid (Item : Waker) return Boolean;

   Now     : constant Unsigned_64 := ABI.Now;
   Forever : constant Unsigned_64 := ABI.Forever;

   --  This process's own process handle (the launcher's "self-process"),
   --  the authority Create takes.
   function Own_Process return Process;

   --  A new notification charged to this process (Own_Process names it).
   function Create (Self : Process; Item : out Waitable) return ABI.Outcome;

   --  A Wake handle with Badge (1 .. ABI.Badge_Last); Grant: it may be
   --  copied to another process.
   function Mint_Waker (Item : Waitable; Badge : Unsigned_64; Grant : Boolean;
                        Result : out Waker) return ABI.Outcome;

   --  The bits pending (cleared now), waiting until the deadline for some:
   --  0 when none came, else ABI.Busy, ABI.Ended or ABI.Refused.
   function Wait (Item : Waitable; Deadline : Unsigned_64) return Unsigned_64;

   --  Signal: ABI.Signalled, ABI.Ended or ABI.Refused.
   function Wake (Item : Waker) return Unsigned_64;

   function Bind_Leaf (Leaf : Waitable; Root : Waker) return ABI.Outcome;
   function Unbind_Leaf (Leaf : Waitable) return ABI.Outcome;
   function Bind_Thread (Root : Waitable) return ABI.Outcome;
   function Unbind_Thread (Root : Waitable) return ABI.Outcome;

   --  Ends the notification (Wait handle) or gives up a Wake handle.
   function Close (Item : Waitable) return Boolean;
   function Close (Item : Waker) return Boolean;

   --  A Wake handle placed in another process's table (it names nothing
   --  here); the word it received, for the startup or reply that carries it.
   function Word_Of (Item : Waker) return Unsigned_64;
   function To_Waker (Word : Unsigned_64) return Waker;

   ---------------------------------------------------------------------------
   --  Private futex words: an aligned 32-bit word of this process's own
   --  memory, waited on by its own threads only.
   ---------------------------------------------------------------------------
   --  Sleep while the word at Where holds Expected: ABI.Woken, ABI.Retry
   --  (it did not), ABI.Timed_Out or ABI.Bad_Argument.
   function Private_Wait (Where : System.Address; Expected, Deadline : Unsigned_64)
     return Unsigned_64;

   --  Wake up to Count waiters of the word, oldest first: how many.
   function Private_Wake (Where : System.Address; Count : Unsigned_64) return Unsigned_64;

private
   type Waitable is new Word_Holder;
   type Waker is new Word_Holder;
   No_Waitable : constant Waitable := (Word => Capability_ABI.No_Handle);
   No_Waker    : constant Waker := (Word => Capability_ABI.No_Handle);
end CuBit.Handles.Notifications;
