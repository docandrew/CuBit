------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CPU load over an interval from two readings of the kernel's per-CPU
--  times (CPU_Times_Publication): busy ticks over all ticks, per CPU and
--  over all CPUs.
--
--  @description
--  Pure arithmetic on what CuBit.CPU_Times read; no page access here, so
--  it is proved and tested hosted. A CPU's load is Known only when it was
--  accounted in both readings and time passed between them:
--  - A CPU that came up, went away or stopped being accounted between the
--    readings (hotplug, an accounting fault) is Unknown for that interval
--    and counts from the next one.
--  - The kernel's counters saturate instead of wrapping; a counter that
--    went backwards (a reader's estimate of the interval in progress ran
--    ahead of the kernel's next boundary by the writer's few nanoseconds,
--    or a counter restarted) counts as no time in that category, never as
--    a huge one.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CPU_Times_Publication;

package CuBit.CPU_Load with Pure, SPARK_Mode is
   package CTP renames CPU_Times_Publication;
   use type CTP.Reading_Kind;

   Full_Percent : constant := 100;
   subtype Percent is Natural range 0 .. Full_Percent;
   Full : constant Percent := Full_Percent;

   type Load (Known : Boolean := False) is record
      case Known is
         when True  => Busy : Percent;
         when False => null;
      end case;
   end record;
   Unknown : constant Load := (Known => False);

   --  One reading of every CPU the kernel publishes (Count of them).
   type Reading_Table is array (CTP.Processor) of CTP.CPU_Reading;
   type Snapshot is record
      Count : CTP.Processor_Count := 0;
      CPUs  : Reading_Table := [others => (Kind => CTP.Not_Present, others => <>)];
   end record;

   --  Ticks in each category between two readings of one counter.
   function Elapsed (Earlier, Later : Unsigned_64) return Unsigned_64 is
     (if Later >= Earlier then Later - Earlier else 0);

   --  Busy and all ticks between two readings.
   type Interval is record
      Busy  : Unsigned_64 := 0;
      Total : Unsigned_64 := 0;
   end record;
   function Valid (I : Interval) return Boolean is (I.Busy <= I.Total);

   function Between (Earlier, Later : CTP.Times) return Interval
   with Post => Valid (Between'Result);

   --  Part of Whole in whole percent, rounded to nearest; Whole 0: 0.
   function Share (Part, Whole : Unsigned_64) return Percent
   with Pre  => Part <= Whole,
        Post => (if Part = 0 then Share'Result = 0)
                and then (if Part = Whole and then Whole > 0 then Share'Result = Full);

   --  The load of an interval: Unknown when no time passed.
   function Of_Interval (I : Interval) return Load is
     (if I.Total = 0 then Unknown else (Known => True, Busy => Share (I.Busy, I.Total)))
   with Pre => Valid (I);

   --  Whether a CPU was accounted in both readings.
   function Comparable (Earlier, Later : CTP.CPU_Reading) return Boolean is
     (Earlier.Kind = CTP.Accounted_Times and then Later.Kind = CTP.Accounted_Times);

   --  One CPU's load between two readings.
   function CPU (Earlier, Later : CTP.CPU_Reading) return Load is
     (if Comparable (Earlier, Later)
      then Of_Interval (Between (Earlier.Totals, Later.Totals))
      else Unknown);

   --  Every CPU comparable in both snapshots, added up (saturating): the
   --  share of all CPUs' time that was busy.
   function Sum (Earlier, Later : Snapshot) return Interval
   with Post => Valid (Sum'Result);

   function Aggregate (Earlier, Later : Snapshot) return Load is
     (Of_Interval (Sum (Earlier, Later)));

   --  CPUs present (accounted or not) in a snapshot.
   function Present (S : Snapshot) return Natural
   with Post => Present'Result <= Natural (S.Count);
end CuBit.CPU_Load;
