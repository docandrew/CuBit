-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
pragma Ada_2022;
with PerCPUData;
with Process;
with System.Machine_Code;
with Time;
with x86;

package body Interrupt_Windows with SPARK_Mode => Off is
   function Ticks_For (Us : Natural) return Unsigned_64 is
     (Unsigned_64 (Us) * Unsigned_64 (Time.tscPerDuration));

   procedure Open (Hold_Us : Natural := 0) is
      Until_TSC : Unsigned_64;
   begin
      -- Not with a lock held, not where interrupts are on already, and not
      -- before this CPU runs threads (boot loads its first images with no
      -- thread to come back to).
      if PerCPUData.numCLI /= 0 or else x86.getFlags.interrupt
        or else Process."=" (PerCPUData.getCurrentThread, Process.NO_THREAD)
      then
         return;
      end if;
      if Hold_Us = 0 then
         -- STI takes effect after the next instruction, so the NOP is the
         -- window: a pending interrupt is taken before the CLI.
         System.Machine_Code.Asm ("sti" & ASCII.LF & ASCII.HT & "nop" & ASCII.LF & ASCII.HT & "cli",
                                  Volatile => True, Clobber => "memory");
      else
         x86.sti;
         Until_TSC := x86.rdtsc + Ticks_For (Hold_Us);
         while x86.rdtsc < Until_TSC loop
            System.Machine_Code.Asm ("pause", Volatile => True);
         end loop;
         x86.cli;
      end if;
   end Open;

   function Start return Pace is ((Last => x86.rdtsc));

   function Due (P : Pace) return Boolean is
     (x86.rdtsc - P.Last >= Ticks_For (Interval_Us));

   procedure Point (P : in out Pace) is
   begin
      if Due (P) then
         Open;
         P.Last := x86.rdtsc;
      end if;
   end Point;
end Interrupt_Windows;
