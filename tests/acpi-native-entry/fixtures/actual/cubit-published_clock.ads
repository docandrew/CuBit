------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Monotonic time from the clock publication (KERN-002, docs/fast-clock.md):
--  a memory load and RDTSC, no system call. Shared by the Ada runtime and the
--  libc's Ada, so it uses only CuBit.Kernel_Calls for its fallbacks.
--
--  @description
--  The kernel maps the page read-only into every process at
--  Clock_Publication.Page_Address (shared/time/clock_publication.ads). When
--  it is unpublished (no invariant TSC) or a read keeps colliding with an
--  update, these fall back to GET_TIME and READ_MONOTONIC_MICROSECONDS.
--
--  With the page, Milliseconds is never behind GET_TIME (the kernel's
--  msTicks is the same conversion sampled at its last CPU 0 tick), so a
--  deadline computed from it does not expire early; Microseconds is on the
--  same epoch (Milliseconds = Microseconds / 1000). Without it,
--  Microseconds is the kernel's HPET clock, on an epoch of its own.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Published_Clock is

   --  Nanoseconds since boot; Published False when the page cannot be
   --  read (then Nanoseconds is 0 and the caller uses a system call).
   procedure Read_Nanoseconds
     (Nanoseconds : out Unsigned_64; Published : out Boolean);

   --  GET_TIME's clock: milliseconds since boot.
   function Milliseconds return Unsigned_64;

   --  READ_MONOTONIC_MICROSECONDS's clock; Available False when the kernel
   --  has no high-resolution clock either.
   procedure Microseconds (Value : out Unsigned_64; Available : out Boolean);

   --  The TSC's published rate in ticks per second, or 0 if unpublished.
   function Counter_Frequency return Unsigned_64;

end CuBit.Published_Clock;
