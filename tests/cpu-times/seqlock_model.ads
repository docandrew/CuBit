------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A SPARK model of one CPU record's update protocol in the per-CPU times
--  publication (shared/publications/cpu_times_publication.ads).
--
--  @description
--  A history is the record's state after each step of an interleaving. The
--  writer (the record's own CPU) may, at each step, do nothing, open
--  (counter even to odd), store while the counter is odd, or close (odd to
--  even). A store may land word by word: the model lets any state follow
--  while writing as long as no time counter and no dispatch count goes
--  down, which every partial store of an Accounted result satisfies.
--
--  Proved: a reader whose two counter samples are Stable copied one
--  version the writer finished (every state between them holds the same
--  values), and the published counters never decrease over any history.
--
--  Assumed, not proved: the hardware keeps the reader's loads in program
--  order between the two counter loads (x86 TSO, and LFENCE around RDTSC),
--  the writer's stores become visible in program order (TSO), only the
--  record's CPU writes it (each CPU accounts its own boundaries under the
--  scheduler's lock), and the TSCs of all CPUs agree. The reader's
--  estimate of the interval in progress (Current_Totals) may run ahead of
--  the next published value by the writer's latency between sampling the
--  counter and opening the record; CuBit.CPU_Load counts a decrease as no
--  time.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CPU_Times_Publication; use CPU_Times_Publication;

package Seqlock_Model with SPARK_Mode is

   Maximum_Steps : constant := 10_000;
   subtype Step is Natural range 0 .. Maximum_Steps;

   type Snapshot is record
      Sequence : Unsigned_64 := 0;
      Values   : Fields;
   end record;
   type History is array (Step) of Snapshot;

   --  No counter of Later is below Earlier's.
   function Not_Behind (Earlier, Later : Fields) return Boolean is
     (Grown (Earlier.Totals, Later.Totals)
      and then Earlier.Dispatches <= Later.Dispatches);

   function Writer_Step (Before, After : Snapshot) return Boolean is
     (After = Before
      or else (not Writing (Before.Sequence)
               and then Before.Sequence < Unsigned_64'Last - 1
               and then After.Sequence = Opened (Before.Sequence)
               and then After.Values = Before.Values)
      or else (Writing (Before.Sequence)
               and then After.Sequence = Before.Sequence
               and then Not_Behind (Before.Values, After.Values))
      or else (Writing (Before.Sequence)
               and then Before.Sequence < Unsigned_64'Last
               and then After.Sequence = Closed (Before.Sequence)
               and then After.Values = Before.Values));

   function Follows_Protocol (H : History) return Boolean is
     (for all S in 0 .. Maximum_Steps - 1 => Writer_Step (H (S), H (S + 1)));

   procedure Lemma_Counter_Monotonic (H : History; First, Last : Step)
   with Ghost,
        Pre  => Follows_Protocol (H) and then First <= Last,
        Post => (for all S in First .. Last =>
                   H (First).Sequence <= H (S).Sequence
                   and then H (S).Sequence <= H (Last).Sequence);

   procedure Theorem_Stable_Read (H : History; First, Last : Step)
   with Ghost,
        Pre  => Follows_Protocol (H) and then First <= Last
                and then Stable (H (First).Sequence, H (Last).Sequence),
        Post => (for all S in First .. Last =>
                   H (S).Values = H (First).Values);

   procedure Theorem_Counters_Grow (H : History; First, Last : Step)
   with Ghost,
        Pre  => Follows_Protocol (H) and then First <= Last,
        Post => Not_Behind (H (First).Values, H (Last).Values);

   --  The kernel's step is a store the model admits.
   procedure Lemma_Accounted_Admitted
     (F : Fields; Ran : Charged_State; Ticks : Unsigned_64;
      Next : Charged_State; Now : Unsigned_64; Started : Boolean)
   with Ghost,
        Post => Not_Behind (F, Accounted (F, Ran, Ticks, Next, Now, Started))
                and then Not_Behind (F, Stopped (F, Now));

end Seqlock_Model;
