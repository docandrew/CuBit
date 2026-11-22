------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A SPARK model of the clock publication's update protocol
--  (shared/time/clock_publication.ads, docs/fast-clock.md).
--
--  @description
--  A history is the page's state after each step of an interleaving. The
--  writer may, at each step, do nothing, open (counter even to odd), store
--  any parameters while the counter is odd, or close (odd to even). A
--  reader samples the counter at step First, copies the fields at any steps
--  between, and samples the counter again at step Last.
--
--  Proved: if the reader's two samples are Stable, every state between
--  them holds the same parameters (so whatever it copied is one version
--  the writer finished), and a rebase never makes the clock run backward
--  for readers whose counter precedes the writer's sample.
--
--  Assumed, not proved: the hardware keeps the reader's loads and RDTSC in
--  program order between the two counter loads (x86 TSO plus LFENCE), the
--  writer's stores become visible in program order (TSO), the writer
--  samples the new base counter after its opening store is visible, and
--  the TSCs of all CPUs agree.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication; use Clock_Publication;

package Seqlock_Model with SPARK_Mode is

   Maximum_Steps : constant := 10_000;
   subtype Step is Natural range 0 .. Maximum_Steps;

   type History is array (Step) of Page;

   function Writer_Step (Before, After : Page) return Boolean is
     (After = Before
      or else (not Writing (Before.Sequence)
               and then Before.Sequence < Unsigned_64'Last - 1
               and then After.Sequence = Opened (Before.Sequence)
               and then After.Fields = Before.Fields)
      or else (Writing (Before.Sequence)
               and then After.Sequence = Before.Sequence)
      or else (Writing (Before.Sequence)
               and then Before.Sequence < Unsigned_64'Last
               and then After.Sequence = Closed (Before.Sequence)
               and then After.Fields = Before.Fields));

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
                   H (S).Fields = H (First).Fields);

   --  The update protocol for a new rate: continue from the old clock's
   --  time at the writer's counter sample.
   function Rebase
     (Old : Parameters; At_Ticks : Unsigned_64; Frequency : Counter_Frequency)
      return Parameters
   is (Initial (Frequency, At_Ticks, Time_At (Old, At_Ticks)))
   with Pre => Readable (Old, At_Ticks)
               and then Time_At (Old, At_Ticks) <= Maximum_Base;

   procedure Theorem_Rebase_Monotonic
     (Old       : Parameters;
      At_Ticks  : Unsigned_64;
      Frequency : Counter_Frequency;
      Earlier   : Unsigned_64;
      Later     : Unsigned_64)
   with Ghost,
        Pre  => Readable (Old, At_Ticks)
                and then Time_At (Old, At_Ticks) <= Maximum_Base
                and then Earlier <= At_Ticks and then At_Ticks <= Later
                and then Readable (Rebase (Old, At_Ticks, Frequency), Later),
        Post => Time_At (Old, Earlier)
                  <= Time_At (Rebase (Old, At_Ticks, Frequency), Later);

end Seqlock_Model;
