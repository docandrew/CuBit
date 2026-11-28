------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  WAIT (ABI-002 slice 2): the one system call that waits, and the typed
--  helpers the runtime builds on it (Wait_ABI has the forms).
--
--  @description
--  Deadlines are explicit: an absolute monotonic millisecond (CuBit.Deadlines,
--  CuBit.Messages.Deadline_After), Now (look once) or Forever, spelled out.
--  CuBit.Messages' receive, poll, completion and activity wrappers keep their
--  names and contracts and call this.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with Wait_ABI;
with Futex_ABI;
with CuBit.Grant_References;

package CuBit.Waits is

   package W renames Wait_ABI;
   package FA renames Futex_ABI;
   package GR renames CuBit.Grant_References;

   Now     : constant Unsigned_64 := W.Now;
   Forever : constant Unsigned_64 := W.Forever;

   --  The call itself: Sources (a Wait_ABI form's word), Deadline, A, B, C.
   function Wait (Sources, Deadline : Unsigned_64; A, B, C : Unsigned_64 := 0)
     return Unsigned_64
   with Inline;

   --  Sleep until the absolute monotonic microsecond Instant (the clock of
   --  CuBit.Monotonic.Microseconds). A time already passed returns at once.
   procedure Sleep_Until (Instant : Unsigned_64);

   --  Sleep for Milliseconds from now (0: return at once). The function form
   --  returns 0, for call sites that keep the old system call's result word.
   procedure Sleep_For (Milliseconds : Unsigned_64);
   function Sleep_For (Milliseconds : Unsigned_64) return Unsigned_64;

   --  Sleep while the 32-bit word at Word holds Expected, until a
   --  Futex_Wake on it or the deadline. A Wait_ABI futex result.
   function Futex_Wait (Word : System.Address; Expected, Deadline : Unsigned_64)
     return Unsigned_64;

   --  A futex word (IPC-010): a private word of this process, or a word of
   --  a grant this process is a party of (its owner or its grantee), named
   --  by the grant and the word's byte offset in it. Both parties name a
   --  shared word the same way, whatever their mappings.
   --  Where: this process's mapping of the word, when known (the futex
   --  pause watches it); Null_Address: no pause.
   type Futex_Name is record
      Shared : Boolean := False;
      Word   : Unsigned_64 := 0;
      Where  : System.Address := System.Null_Address;
   end record;

   function Private_Word (Where : System.Address) return Futex_Name is
     ((Shared => False, Word => Unsigned_64 (System.Storage_Elements.To_Integer (Where)),
       Where => Where));

   --  Mapped: where this process maps the grant (its owner's pages, or
   --  its grantee's window), for the pause; Null_Address: no pause.
   function Shared_Word (Grant : GR.Reference; Offset : FA.Grant_Offset;
                         Mapped : System.Address := System.Null_Address) return Futex_Name is
     ((Shared => True, Word => FA.Shared_Word (Grant.slot, Offset),
       Where => (if System."=" (Mapped, System.Null_Address) then System.Null_Address
                 else System.Storage_Elements."+" (Mapped, System.Storage_Elements.Storage_Offset (Offset)))))
   with Pre => Grant.slot /= 0;

   --  Sleep while Name holds Expected, until a wake or the deadline. A
   --  Wait_ABI futex result; Futex_Revoked when a shared word's grant life
   --  ended; Futex_Fault when not a party of a live grant. With a known
   --  mapping and the kernel's pause published (CuBit.System_Facts.
   --  User_Wait_Cycles, IPC-010), it first waits up to Pause_Cycles in user
   --  mode on the word (UMONITOR, UMWAIT): a change in that time returns
   --  Futex_Retry with no system call.
   function Futex_Wait (Name : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64;

   --  Wake up to Count waiters of Name, oldest first: how many were woken,
   --  or FA.Wake_Refused (a shared word of a grant this process is not a
   --  party of).
   function Futex_Wake (Name : Futex_Name; Count : Unsigned_64) return Unsigned_64;

   --  Wake-plus-wait (IPC-010), one kernel entry: wake one waiter of
   --  Peer, placed on this CPU, then sleep while Mine holds Expected,
   --  switching straight to the woken thread when it is still ready here.
   --  No slice is donated; deadline order holds. The wake happens whatever
   --  the wait answers (a futex result; Futex_Fault, and no wait, when
   --  Peer is refused).
   function Hand_Off (Peer, Mine : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64
   --  Shared words only: a private word's wait-plus-wake is retired (IPC
   --  revision 2; private words use Notification_ABI's Wait and Wake).
   with Pre => Peer.Shared and then Mine.Shared;

   --  Wake a channel peer (IPC-010 option B): a futex wake of its wake
   --  word through Grant, a live channel grant (made with the notify flag)
   --  this process is a party of. Sync: this thread is about to block (the
   --  woken thread is placed on its CPU). False: refused (not a party, or
   --  the grant or the peer has ended).
   --  Urgent: an input wake (Futex_ABI.Urgent_Hint): a holder of an
   --  Input_Wakes_Only INTERACTIVE contract lends the woken thread an urgent
   --  turn (Latency_Contract_ABI).
   function Wake_Peer
     (Grant : GR.Reference; Sync : Boolean := False; Urgent : Boolean := False) return Boolean;

   --  The futex pause's length, under the kernel's limit.
   Pause_Cycles : constant Unsigned_64 := 4_000;

end CuBit.Waits;
