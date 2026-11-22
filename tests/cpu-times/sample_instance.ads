------------------------------------------------------------------------------
--  An instance of the reader generic for proof (run.sh --prove).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CPU_Times_Publication; use CPU_Times_Publication;
with CPU_Times_Publication.Sample;

package Sample_Instance with SPARK_Mode is
   Published : constant Fields :=
     (State => CPU_State'Enum_Rep (Idle), Since => 10,
      Totals => (Idle_Ticks => 5, Thread_Ticks => 7, Scheduler_Ticks => 1),
      Dispatches => 3);
   function Load_Sequence (CPU : Processor) return Unsigned_64 is (if CPU = 0 then 2 else 4);
   function Load_Values (CPU : Processor) return Fields is
     (if CPU = 0 then Published else (others => <>));
   function Load_Counter return Unsigned_64 is (1_000);
   procedure Read is new CPU_Times_Publication.Sample
     (Load_Sequence, Load_Values, Load_Counter);
end Sample_Instance;
