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
with Interfaces; use Interfaces;
with Wait_ABI;

package CuBit.Waits is

   package W renames Wait_ABI;

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

end CuBit.Waits;
