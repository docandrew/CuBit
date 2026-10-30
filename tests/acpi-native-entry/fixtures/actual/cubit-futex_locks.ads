------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Thread coordination inside one process (docs/threads.md): a futex mutex
--  and an event word (a generation threads sleep on until it moves). Used
--  by CuBit.Channel_Clients to route completions to the threads waiting
--  for them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Deadlines;

package CuBit.Futex_Locks is

   type Word is mod 2 ** 32 with Atomic, Size => 32;

   --  0 unlocked, 1 locked, 2 locked with waiters (the protocol Rust's std
   --  uses). Lock never spins in user space beyond one compare-and-swap.
   type Mutex is limited record
      State : aliased Word := 0;
   end record;

   procedure Lock (M : in out Mutex);
   procedure Unlock (M : in out Mutex);

   --  An event: Current samples it; Wait sleeps while it still equals Seen
   --  (False: the deadline passed); Signal moves it and wakes every waiter.
   type Event is limited record
      Generation : aliased Word := 0;
   end record;

   function Current (E : Event) return Unsigned_32;
   function Wait (E : in out Event; Seen : Unsigned_32; Deadline : CuBit.Deadlines.Deadline)
     return Boolean;
   procedure Signal (E : in out Event);

end CuBit.Futex_Locks;
