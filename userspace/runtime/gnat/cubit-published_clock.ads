------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Monotonic time from the clock publication (KERN-002, docs/fast-clock.md):
--  a memory load and RDTSC, no system call (there is none for the time).
--  Shared by the Ada runtime and the libc's Ada.
--
--  @description
--  The kernel maps the page read-only into every process at
--  Clock_Publication.Page_Address (shared/time/clock_publication.ads). When
--  the conversion is unpublished (no invariant TSC) or a read keeps
--  colliding with an update, these read the kernel's millisecond clock
--  from the page's millisecond word instead (Milliseconds_Address), at a
--  tick's resolution.
--
--  Milliseconds is never behind the kernel's msTicks (which receive and
--  call deadlines use; msTicks is the same conversion sampled at its last
--  CPU 0 tick), so a deadline computed from it does not expire early.
--  Microseconds is on the same epoch (Milliseconds = Microseconds / 1000).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Published_Clock is

   --  Nanoseconds since boot from the conversion; Published False when it
   --  is unpublished or kept changing (then Nanoseconds is 0).
   procedure Read_Nanoseconds
     (Nanoseconds : out Unsigned_64; Published : out Boolean);

   --  The kernel's millisecond clock: milliseconds since boot.
   function Milliseconds return Unsigned_64;

   --  Microseconds since boot, on Milliseconds' epoch: from the conversion,
   --  or the millisecond word times 1000. Available is always True (kept
   --  for callers written when the clock could be missing).
   procedure Microseconds (Value : out Unsigned_64; Available : out Boolean);

   --  Microseconds as one word (always available).
   function Microseconds_Now return Unsigned_64;

   --  The TSC's published rate in ticks per second, or 0 if unpublished.
   function Counter_Frequency return Unsigned_64;

end CuBit.Published_Clock;
