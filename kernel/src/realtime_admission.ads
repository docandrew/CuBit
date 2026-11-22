-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Real-time admission (docs/scheduler.md, "Soft real time")
--
-- A thread may reserve real-time CPU (a budget each period) only when its
-- process holds a CAP_SCHEDULING covering the request, and only while the
-- sum of admitted utilizations stays within Realtime_Share of every online
-- CPU. The rest of the system therefore always keeps its share. Utilization
-- is in parts per million of one CPU, rounded up, so admission never
-- understates a reservation.
-------------------------------------------------------------------------------
package Realtime_Admission with Pure, SPARK_Mode => On is

   Parts_Per_Million : constant := 1_000_000;
   -- The largest share of each CPU that admitted real time may take (as
   -- Kolivas's SCHED_ISO capped it).
   Realtime_Share : constant := 700_000;

   Maximum_CPUs : constant := 256;
   subtype CPU_Count is Positive range 1 .. Maximum_CPUs;

   -- One reservation's share of a CPU.
   type Utilization is range 0 .. Parts_Per_Million;
   -- The sum admitted across the system.
   type Total is range 0 .. Parts_Per_Million * Maximum_CPUs;

   -- Products of shares, budgets and periods, without wraparound, in 64-bit
   -- arithmetic (the kernel has no 128-bit division).
   type Product is range 0 .. 2 ** 62;

   -- A reservation's shape: Budget microseconds of CPU every Period.
   -- Up to about 18 minutes: a product of two stays within Product.
   type Microseconds is range 0 .. 2 ** 30;
   subtype Period_Microseconds is Microseconds range 1 .. Microseconds'Last;
   function Valid (Budget, Period : Microseconds) return Boolean is
     (Period > 0 and then Budget > 0 and then Budget <= Period);

   function Utilization_Of
     (Budget : Microseconds; Period : Period_Microseconds) return Utilization
     with Pre => Budget <= Period,
          Post =>
            Product (Utilization_Of'Result) * Product (Period) >=
              Product (Budget) * Parts_Per_Million;

   -- Whether a capability allowing Cap_Budget every Cap_Period covers a
   -- request of Budget every Period: no larger a share of the CPU.
   function Covers
     (Cap_Budget : Microseconds; Cap_Period : Period_Microseconds;
      Budget     : Microseconds; Period     : Period_Microseconds) return Boolean
   is (Product (Budget) * Product (Cap_Period) <=
         Product (Cap_Budget) * Product (Period));

   function Capacity (CPUs : CPU_Count) return Total is
     (Total (Realtime_Share) * Total (CPUs));

   -- Admit Request if the total stays within the capacity.
   procedure Admit
     (Admitted : in out Total;
      Request  : Utilization;
      CPUs     : CPU_Count;
      Granted  : out Boolean)
     with Post =>
       Granted = (Admitted'Old <= Capacity (CPUs) and then
                  Request <= Utilization (Realtime_Share) and then
                  Total (Request) <= Capacity (CPUs) - Admitted'Old) and then
       Admitted = (if Granted then Admitted'Old + Total (Request)
                   else Admitted'Old);

   -- Give back a reservation admitted earlier.
   procedure Release (Admitted : in out Total; Held : Utilization)
     with Pre  => Total (Held) <= Admitted,
          Post => Admitted = Admitted'Old - Total (Held);

end Realtime_Admission;
