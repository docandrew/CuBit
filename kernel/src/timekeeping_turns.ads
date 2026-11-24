-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Who keeps the system's time on a timer interrupt, and what msTicks
-- becomes (Time.clockTick, docs/fast-clock.md).
--
-- @description
-- msTicks, the clock page's millisecond word, and the deadlines measured on
-- them (IPC receives and calls, futex waits, halted vCPUs, the stuck-call
-- report) are the system's, not one CPU's. CPU 0 used to be their only
-- keeper, so a CPU 0 that took no timer interrupt stopped them all for
-- every process: image 33 on the N150 shows no stuck-call report from about
-- 2 s to 11.8 s while calls were held throughout.
--
-- Where every CPU's counter is one clock (an invariant TSC that agrees with
-- CPU 0's on every CPU after TSC_Sync: the clock page is shared,
-- Clock_Page.Shared), any CPU's timer keeps time: msTicks is the
-- page's conversion of that CPU's counter. Without one clock CPU 0 alone
-- keeps it, on its own elapsed ticks. Time.clockTick serializes keepers
-- with a try-lock; this unit is the decision and the arithmetic.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Timekeeping_Turns with Pure, SPARK_Mode is

   subtype Milliseconds is Unsigned_64;

   -- The keeper when CPUs share no clock: the boot CPU.
   Boot_CPU : constant := 0;

   -- CPU 0's timer work missing this long is reported (kernel log and the
   -- stuck-call panel): far past an SMI or a guest entry (VM_Objects
   -- reports one past 20 ms), far below a stall anyone sees.
   Silence_Report_Ms : constant := 100;

   -- Whether CPU may keep time on this timer interrupt.
   function May_Keep (CPU : Natural; Shared_Clock : Boolean) return Boolean is
     (CPU = Boot_CPU or Shared_Clock)
   with Post =>
     (if CPU = Boot_CPU then May_Keep'Result) and then
     (if CPU /= Boot_CPU then May_Keep'Result = Shared_Clock);

   -- msTicks after a keeper read Published (Valid False: no reading). Never
   -- backward, whichever CPU read it and however late its lock came.
   function Next (Previous, Published : Milliseconds; Valid : Boolean)
     return Milliseconds is
     (if Valid and then Published > Previous then Published else Previous)
   with Post =>
     Next'Result >= Previous and then
     (if Valid then Next'Result = Milliseconds'Max (Previous, Published)
      else Next'Result = Previous);

   -- msTicks after CPU 0's own Elapsed ticks (no shared clock): saturating.
   function Advanced (Previous, Elapsed : Milliseconds) return Milliseconds is
     (if Elapsed > Milliseconds'Last - Previous then Milliseconds'Last
      else Previous + Elapsed)
   with Post =>
     Advanced'Result >= Previous and then
     (if Elapsed <= Milliseconds'Last - Previous
      then Advanced'Result = Previous + Elapsed
      else Advanced'Result = Milliseconds'Last);

   -- How long CPU 0's timer work has been missing at Now, given msTicks at
   -- its last run (0: it ran this millisecond, or Now is not past it).
   function Silence (Last_Run, Now : Milliseconds) return Milliseconds is
     (if Now > Last_Run then Now - Last_Run else 0)
   with Post =>
     (if Now > Last_Run then Silence'Result = Now - Last_Run
      else Silence'Result = 0);

   -- Reported: missing at least Silence_Report_Ms.
   function Silent (Last_Run, Now : Milliseconds) return Boolean is
     (Silence (Last_Run, Now) >= Silence_Report_Ms)
   with Post =>
     Silent'Result = (Now > Last_Run and then Now - Last_Run >= Silence_Report_Ms);

end Timekeeping_Turns;
