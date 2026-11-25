-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- Interrupt windows: long kernel work lets its CPU's interrupts in at
-- bounded intervals.
--
-- @description
-- A system call runs with interrupts off (the FMASK MSR clears IF on
-- entry), so work that runs long holds off its CPU's interrupts: the
-- timer, devices (an audio period), other CPUs' TLB requests (which then
-- wait in turn). Long work opens a window at bounded intervals, at points
-- where it holds no lock and no secondary-stack data: interrupts are
-- enabled and disabled again, and whatever is pending is taken there.
-- The timer may switch the thread away and back, perhaps onto another
-- CPU, as in Vm_Objects' run loop; so nothing per-CPU may be held across
-- a window, and the caller re-checks what another thread may have changed
-- meanwhile (a lock holder drops its locks first and checks again after).
-- System call paths only, never an interrupt handler.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Interrupt_Windows with SPARK_Mode => Off is
   -- The longest a paced loop runs between two windows.
   Interval_Us : constant := 50;
   -- How long a lock break keeps interrupts on: the spinlocks are not
   -- fair, and a CPU that takes a lock again at once wins it before a
   -- waiter spinning on it sees it free (hda.drv and mixer.svc waited
   -- 12-14 ms behind an owned-memory release that did, 2026-11).
   Lock_Break_Us : constant := 2;

   -- Opens a window now when this CPU holds no lock, has interrupts off
   -- and runs a thread, else does nothing. Hold_Us: interrupts stay on at least that
   -- long (a lock break that lets spinning waiters in).
   procedure Open (Hold_Us : Natural := 0);

   -- A loop's pace: when its last window was.
   type Pace is private;
   function Start return Pace;
   -- In a loop holding no lock: opens a window once Interval_Us has passed
   -- since the last one (or Start).
   procedure Point (P : in out Pace);
   -- For a loop holding locks: True once a window is due; the loop drops
   -- its locks, calls Point, takes them again and re-checks.
   function Due (P : Pace) return Boolean;
private
   type Pace is record
      Last : Unsigned_64 := 0;
   end record;
end Interrupt_Windows;
