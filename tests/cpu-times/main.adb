------------------------------------------------------------------------------
--  Hosted tests for the per-CPU times publication (shared/publications)
--  and CuBit.CPU_Load: the writer's step, the interval in progress, the
--  reader generic against a writer racing it on another CPU (and a reader
--  without the seqlock, which must see torn records), and the load
--  arithmetic: shares, hotplug, counters going backwards, saturation.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CPU_Times_Publication; use CPU_Times_Publication;
with CuBit.CPU_Load; use CuBit.CPU_Load;
with Racing_Writer;
with Sample_Instance;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("cpu-times: " & Name & " PASS");
      else
         Failures := Failures + 1;
         Put_Line ("cpu-times: " & Name & " FAIL");
      end if;
   end Check;

   function Accounted_Reading (Idle, Thread, Scheduler : Unsigned_64) return CPU_Reading is
     ((Kind => Accounted_Times,
       Totals => (Idle_Ticks => Idle, Thread_Ticks => Thread, Scheduler_Ticks => Scheduler),
       Dispatches => 0));

   function Is_Load (L : Load; Busy : Percent) return Boolean is
     (L.Known and then L.Busy = Busy);

   F : Fields;
   T : Times;
   R : CPU_Reading;
   A, B : Snapshot;
   Torn : Unsigned_64;
   Stable : Natural;
begin
   --  The writer's step.
   F := Accounted (F, Idle, 0, Running, 100, True);
   Check (State_Of (F.State) = Running and then F.Since = 100 and then F.Dispatches = 1
          and then F.Totals = (others => 0), "first boundary starts a thread");
   F := Accounted (F, Running, 40, Scheduling, 140, False);
   F := Accounted (F, Scheduling, 2, Idle, 142, False);
   Check (F.Totals = (Idle_Ticks => 0, Thread_Ticks => 40, Scheduler_Ticks => 2)
          and then F.Dispatches = 1 and then State_Of (F.State) = Idle,
          "boundaries charge what ran");
   F := Accounted (F, Idle, Unsigned_64'Last, Idle, 200, False);
   F := Accounted (F, Idle, 5, Idle, 205, False);
   Check (F.Totals.Idle_Ticks = Unsigned_64'Last, "counters saturate, never wrap");

   --  The interval in progress.
   F := (State => CPU_State'Enum_Rep (Idle), Since => 100,
         Totals => (Idle_Ticks => 10, Thread_Ticks => 20, Scheduler_Ticks => 1),
         Dispatches => 0);
   T := Current_Totals (F, 150);
   Check (T.Idle_Ticks = 60 and then T.Thread_Ticks = 20, "interval in progress is the state's");
   Check (Current_Totals (F, 90) = F.Totals, "a counter behind Since adds nothing");
   Check (Reading_Of (Stopped (F, 150), 200).Kind = Unavailable, "a faulted CPU is unavailable");
   Check (Reading_Of ((others => <>), 200).Kind = Not_Present, "an unstarted CPU is not present");
   Check (State_Of (99) = Offline, "an unknown state word reads as offline");

   --  The reader generic.
   Sample_Instance.Read (0, R);
   Check (R.Kind = Accounted_Times and then R.Totals.Idle_Ticks = 5 + 990
          and then R.Dispatches = 3, "a stable record is read with its interval");
   Racing_Writer.Run (Torn, Stable);
   Put_Line ("cpu-times: racing reads stable=" & Stable'Image & " torn=" & Torn'Image);
   Check (Torn = 0 and then Stable > 0, "no torn record under a racing writer");
   Racing_Writer.Run_Unprotected (Torn);
   Put_Line ("cpu-times: unprotected reads torn=" & Torn'Image);
   Check (Torn > 0, "mutation: a reader without the seqlock sees torn records");

   --  Shares.
   Check (Share (0, 100) = 0 and then Share (100, 100) = 100 and then Share (25, 100) = 25,
          "share of whole");
   Check (Share (1, 1_000_000) = 1 and then Share (999_999, 1_000_000) = 99,
          "some use never rounds to none or full");
   Check (Share (Unsigned_64'Last / 2, Unsigned_64'Last) = 50, "share of huge counts");
   Check (Share (2, 3) = 67, "share rounds to nearest");

   --  One CPU.
   Check (Is_Load (CPU (Accounted_Reading (0, 0, 0), Accounted_Reading (750, 200, 50)), 25),
          "busy is threads plus scheduler over all");
   Check (not CPU (Accounted_Reading (1, 1, 1), Accounted_Reading (1, 1, 1)).Known,
          "no time passed: unknown");
   Check (not CPU ((Kind => Not_Present, others => <>), Accounted_Reading (5, 5, 0)).Known,
          "hotplug: a CPU that just came up is unknown");
   Check (not CPU (Accounted_Reading (5, 5, 0), (Kind => Unavailable, others => <>)).Known,
          "an accounting fault: unknown");
   Check (Is_Load (CPU (Accounted_Reading (100, 100, 0), Accounted_Reading (200, 90, 0)), 0),
          "a counter going backwards counts as no time");
   Check (not CPU (Accounted_Reading (Unsigned_64'Last, Unsigned_64'Last, 0),
                   Accounted_Reading (Unsigned_64'Last, Unsigned_64'Last, 0)).Known,
          "saturated counters: unknown");
   Check (Is_Load (CPU (Accounted_Reading (Unsigned_64'Last - 1000, 0, 0),
                        Accounted_Reading (Unsigned_64'Last, 1000, 0)), 50),
          "near-saturated counters still divide");

   --  All CPUs.
   A.Count := 3;
   B.Count := 3;
   A.CPUs (0) := Accounted_Reading (0, 0, 0);
   B.CPUs (0) := Accounted_Reading (0, 1000, 0);      --  100 % busy
   A.CPUs (1) := Accounted_Reading (0, 0, 0);
   B.CPUs (1) := Accounted_Reading (1000, 0, 0);      --  idle
   A.CPUs (2) := (Kind => Not_Present, others => <>);
   B.CPUs (2) := Accounted_Reading (0, 5000, 0);      --  came up: left out
   Check (Is_Load (Aggregate (A, B), 50), "aggregate over comparable CPUs");
   Check (Present (B) = 3 and then Present (A) = 2, "present CPUs");
   B.Count := 1;
   Check (Is_Load (Aggregate (A, B), 100), "a CPU count that shrank bounds the sum");
   Check (not Aggregate ((others => <>), (others => <>)).Known, "nothing published: unknown");

   if Failures = 0 then
      Put_Line ("cpu-times: PASS");
   else
      Put_Line ("cpu-times: FAIL" & Failures'Image);
   end if;
end Main;
