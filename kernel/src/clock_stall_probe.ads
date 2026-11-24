-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The clock-stall probe: test kernels only (Build.Clock_Stall_Probe,
-- CLOCK_STALL_PROBE=1), for headless --test clock-keeper.
--
-- @description
-- On the N150 laptop (image 33) CPU 0's timer ran no clock work for about
-- ten seconds, and with CPU 0 the system's only time keeper, msTicks and
-- every deadline measured on it stopped for every process. QEMU's CPU 0
-- takes its timer interrupts, so this probe makes it skip them: on the
-- console request Stall_Request, CPU 0's timer interrupts run no clock work
-- (Time.clockTick) for Stall_Ms. CPU 0 still schedules, takes IPIs and
-- rearms its timer, so only the clock work is missing, as on that CPU 0.
-- Nothing here is reachable in a normal kernel.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Process;

package Clock_Stall_Probe is
   Stall_Request : constant String := "CLOCKK-PROBE-STALL-CPU0";
   Stall_Ms : constant := 800;

   -- Text is the caller's console write (a chunk): the request when it is
   -- Stall_Request. Handled reports True.
   procedure Handle_Request
     (Caller : Process.ProcessID; Text : String; Handled : out Boolean);

   -- On a timer interrupt: True while this CPU is CPU 0 and a requested
   -- stall lasts (its clock work is then skipped).
   function Skips_Clock_Work return Boolean;
end Clock_Stall_Probe;
