------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's wake word (IPC-010 option B): the futex word its activity
--  waits name (Wait_ABI's Ready_Word form).
--
--  @description
--  Every activity wait (CuBit.Messages.Wait_For_Activity_Until and
--  Wait_For_Activity_Or_Event) names Word, so a wake of it ends the wait:
--  a channel peer's (CuBit.Channels.Signal, a futex wake through the
--  channel's grant), or a thread of this process's (Wake). The word's value
--  never changes: the kernel's pending wake is what the wait reports, and
--  it is taken by the wait (or by a receive, as a Channel_Wake_ABI notice).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;

package CuBit.Wake_Words is

   --  The value the word always holds.
   Value : constant Unsigned_64 := 0;

   Word : aliased Unsigned_32 := 0 with Volatile, Alignment => 4;

   function Address return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (Word'Address)));

   --  Wake this process's activity wait (another thread's, or a later one).
   procedure Wake;

end CuBit.Wake_Words;
