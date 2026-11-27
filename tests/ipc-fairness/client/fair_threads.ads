--  The calling threads of fairness-client (tests/ipc-fairness): library
--  level, so thread entry points need no trampolines. Thread bodies use no
--  secondary stack and do not print.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Timing_Histograms;

package Fair_Threads is

   Max_Threads : constant := 64;
   subtype Thread_Index is Positive range 1 .. Max_Threads;

   type Word is mod 2 ** 32 with Atomic, Size => 32;

   --  The timed window, in monotonic milliseconds; set before any thread
   --  starts.
   Window_Start, Window_End : Unsigned_64 := 0 with Volatile;

   --  Per thread: round-trip times (TSC ticks) of the calls started in the
   --  window, how many, whether a reply was wrong, and its exit word (the
   --  kernel clears it and wakes it when the thread ends).
   Samples : array (Thread_Index) of CuBit.Timing_Histograms.Histogram :=
     [others => CuBit.Timing_Histograms.Empty];
   Calls   : array (Thread_Index) of Unsigned_64 := [others => 0];
   Failed  : array (Thread_Index) of Boolean := [others => False];
   Running : array (Thread_Index) of aliased Word := [others => 0];

   --  The thread body: Argument is its Thread_Index.
   procedure Caller (Argument : Unsigned_64) with Convention => C;

   --  Start thread I on a fresh stack: False when refused.
   function Start (I : Thread_Index) return Boolean;

   --  Wait until thread I has ended.
   procedure Join (I : Thread_Index);

end Fair_Threads;
