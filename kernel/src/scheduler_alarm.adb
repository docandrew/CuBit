pragma Ada_2022;
with Interfaces; use Interfaces;
with Build;
with Config;
with Deadline_Ownership;
with Interrupts;
with Lapic;
with PerCPUData;
with Scheduler_Timing;
with TextIO;
with Time;
with x86;
with Trace;

-- MMIO and CPU-local interrupt state. The deadline slot itself is the shared
-- SPARK-proved ADT. This slot belongs to the CPU, not an admitted process.
package body Scheduler_Alarm is
   package Alarm is new Deadline_Ownership (Natural, Natural'Last);
   use type Alarm.Outcome;
   type CPU_State is record
      Slot : Alarm.State;
      Started : Boolean := False;
      Disabled : Boolean := False;
   end record with Alignment => 64;
   States : array (0 .. Config.MAX_SMP_CPUS - 1) of CPU_State;

   procedure Program (Delay_Us : Delay_Microseconds) is
      package HW is new Lapic (Interrupts.lapicAddr);
      Count : constant Unsigned_64 :=
        (Unsigned_64 (Interrupts.getLAPICTimerInterval) * Unsigned_64 (Delay_Us) + 999) / 1000;
   begin
      HW.armTimer (Unsigned_32 (Unsigned_64'Max (1, Count)));
   end Program;

   procedure Request_Earlier (Delay_Us : Delay_Microseconds) is
      CPU : constant Natural := PerCPUData.getCPUNumber;
      S : CPU_State renames States(CPU);
      Now : Unsigned_64;
      Step, Target : Unsigned_64;
      Ticket : Alarm.Ticket;
      Accepted : Boolean;
   begin
      if not Build.OneShot_Scheduling or else S.Disabled or else
        Interrupts.getLAPICTimerInterval = 0 or else Time.tscPerDuration = 0
      then return; end if;
      Now := x86.readOrderedTSC;
      Step := Time.tscPerDuration * Unsigned_64 (Delay_Us);
      Target := (if Step > Unsigned_64'Last - Now then Unsigned_64'Last else Now + Step);
      if not Alarm.View(S.Slot).Active or else Target < Alarm.View(S.Slot).Deadline then
         Alarm.Arm (S.Slot, CPU, Target, Ticket, Accepted);
         if Accepted then
            S.Started := True;
            Program (Delay_Us);
         else
            -- Never wrap a deadline ticket. Retain ordinary periodic service
            -- rather than leaving the CPU without a timer on exhaustion.
            fallback : declare
               package HW is new Lapic (Interrupts.lapicAddr);
            begin
               S.Disabled := True;
               HW.restorePeriodicTimer (Unsigned_32'Max (1,
                 Interrupts.getLAPICTimerInterval / Scheduler_Timing.Ticks_Per_Millisecond));
               TextIO.println ("SCHED-ALARM: fallback after ticket exhaustion");
            end fallback;
         end if;
      end if;
   end Request_Earlier;

   function Interrupt_Due return Boolean is
      CPU : constant Natural := PerCPUData.getCPUNumber;
      S : CPU_State renames States(CPU);
      Now, Late, Remaining, Delay_Us : Unsigned_64;
      Result : Alarm.Outcome;
   begin
      if not Build.OneShot_Scheduling or else S.Disabled then return True; end if;
      -- The initial LAPIC arm precedes the first software opportunity.
      if not S.Started then return True; end if;
      Now := x86.readOrderedTSC;
      Alarm.Poll (S.Slot, Now, CPU, Result, Late);
      if Result = Alarm.Expired then
         Trace.Emit (Trace.EVENT_TIMER_LATE, Late, 0);
         return True;
      end if;
      if Alarm.View(S.Slot).Active then
         Remaining := Alarm.View(S.Slot).Deadline - Now;
         -- Round up without overflowing Remaining + Rate - 1.
         Delay_Us := Remaining / Time.tscPerDuration;
         if Remaining mod Time.tscPerDuration /= 0 then Delay_Us := Delay_Us + 1; end if;
         Program (Delay_Microseconds (Unsigned_64'Max (1, Unsigned_64'Min (1000, Delay_Us))));
      end if;
      return False;
   end Interrupt_Due;
end Scheduler_Alarm;
