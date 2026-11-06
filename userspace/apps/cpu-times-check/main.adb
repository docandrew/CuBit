------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-CPU times publication checks (KERN-002, CPU_Times_Publication,
--  tests/headless/run.sh --test cpu-times).
--
--  @description
--  The page is granted by this program's manifest and published for every
--  CPU the kernel started. Over an idle interval every CPU's counters
--  advance and the machine is mostly idle; over an interval this program
--  spends spinning, the busy time summed over all CPUs is close to one CPU
--  (wherever the scheduler ran it), and per-CPU loads stay in range. A
--  snapshot costs no system call (its cost is printed). Finally a write to
--  the page must fault: the kernel maps it read-only.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CPU_Times_Publication;
with CuBit.Benchmark_Clock;
with CuBit.CPU_Load;
with CuBit.CPU_Times;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;
with CuBit.Published_Clock;

procedure Main is
   use ASCII;
   package K renames CuBit.Kernel_ABI;
   package CL renames CuBit.CPU_Load;
   package CTP renames CPU_Times_Publication;
   use type CTP.Reading_Kind, CTP.Processor_Count;

   --  The QEMU guest's CPUs (run.sh --cpus default).
   Expected_Processors : constant := 4;
   Idle_Interval_Us    : constant := 500_000;
   Spin_Interval_Us    : constant := 1_000_000;
   --  A headless boot is mostly idle: under half of all CPU time busy.
   Idle_Ceiling_Percent : constant := 50;
   --  Spinning on one CPU for the interval: at least this much of one CPU,
   --  summed over all of them (the scheduler may move the spinner).
   Spin_Floor_Percent  : constant := 85;
   Read_Rounds         : constant := 100_000;
   Percent_Scale       : constant := 100;

   Failures : Natural := 0;

   procedure Print (Text : String) renames CuBit.Messages.debugPrint;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Print ("cpu-times-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         Print ("cpu-times-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Now_Us return Unsigned_64 is
      Value : Unsigned_64;
      Ok : Boolean;
   begin
      CuBit.Published_Clock.Microseconds (Value, Ok);
      return (if Ok then Value else 0);
   end Now_Us;

   procedure Sleep (Microseconds : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (K.Sleep_Until_Monotonic_Microsecond, Now_Us + Microseconds);
   end Sleep;

   --  Every CPU accounted in both snapshots, and its counters advanced.
   function All_Advanced (A, B : CL.Snapshot) return Boolean is
     (A.Count = B.Count and then B.Count > 0 and then
      (for all C in 0 .. Natural (B.Count) - 1 =>
         CL.Comparable (A.CPUs (C), B.CPUs (C))
         and then CL.Between (A.CPUs (C).Totals, B.CPUs (C).Totals).Total > 0
         and then B.CPUs (C).Dispatches >= A.CPUs (C).Dispatches));

   procedure Report (Label : String; A, B : CL.Snapshot) is
      Line : Unsigned_64;
   begin
      for C in 0 .. Natural (B.Count) - 1 loop
         declare
            L : constant CL.Load := CL.CPU (A.CPUs (C), B.CPUs (C));
            I : constant CL.Interval := CL.Between (A.CPUs (C).Totals, B.CPUs (C).Totals);
         begin
            Line := Unsigned_64 (C);
            Print ("cpu-times-check: " & Label & " cpu" & Image (Line) &
                   (if L.Known then " busy=" & Image (Unsigned_64 (L.Busy)) & "%" else " busy=unknown") &
                   " busy_ticks=" & Image (I.Busy) & " ticks=" & Image (I.Total) &
                   " dispatches=" & Image (B.CPUs (C).Dispatches - A.CPUs (C).Dispatches) & LF);
         end;
      end loop;
   end Report;

   Before, After : CL.Snapshot;
   Frequency : constant Unsigned_64 := CuBit.CPU_Times.Tick_Frequency;
begin
   Print ("cpu-times-check: starting" & LF);
   Check (CuBit.CPU_Times.Available, "CPU times publication granted and present");
   if not CuBit.CPU_Times.Available then
      Print ("TEST: FAIL cpu-times" & LF);
      return;
   end if;
   Check (Frequency = CuBit.Published_Clock.Counter_Frequency and then Frequency > 0,
          "the counter rate is the clock publication's");
   Check (CuBit.CPU_Times.Memory_Bytes > 0
          and then CuBit.CPU_Times.Free_Memory_Bytes in 1 .. CuBit.CPU_Times.Memory_Bytes,
          "free memory is published and within RAM");

   --  An idle interval: every CPU started and accounted, counters advance.
   CuBit.CPU_Times.Read (Before);
   Sleep (Idle_Interval_Us);
   CuBit.CPU_Times.Read (After);
   Print ("cpu-times-check: CPUs" & Image (Unsigned_64 (After.Count)) & LF);
   Check (After.Count = Expected_Processors and then CL.Present (After) = Expected_Processors,
          "every started CPU is published");
   Check (All_Advanced (Before, After), "every CPU's counters advance");
   Report ("idle", Before, After);
   declare
      L : constant CL.Load := CL.Aggregate (Before, After);
   begin
      Check (L.Known and then L.Busy < Idle_Ceiling_Percent, "an idle machine is mostly idle");
   end;

   --  Spin on this CPU for the interval: about one CPU's worth of busy.
   declare
      Start : constant Unsigned_64 := Now_Us;
      Spins : Unsigned_64 := 0 with Volatile;
      Busy_Sum : CL.Interval;
      One_CPU : Unsigned_64;
      Highest : Natural := 0;
   begin
      CuBit.CPU_Times.Read (Before);
      while Now_Us < Start + Spin_Interval_Us loop
         Spins := Spins + 1;
      end loop;
      CuBit.CPU_Times.Read (After);
      Report ("spin", Before, After);
      Busy_Sum := CL.Sum (Before, After);
      One_CPU := Busy_Sum.Total / Unsigned_64 (Natural'Max (1, Natural (After.Count)));
      for C in 0 .. Natural (After.Count) - 1 loop
         declare
            L : constant CL.Load := CL.CPU (Before.CPUs (C), After.CPUs (C));
         begin
            if L.Known then
               Highest := Natural'Max (Highest, L.Busy);
            end if;
         end;
      end loop;
      Print ("cpu-times-check: spin busy_ticks=" & Image (Busy_Sum.Busy) & " one_cpu_ticks=" &
             Image (One_CPU) & " highest=" & Image (Unsigned_64 (Highest)) & "%" & LF);
      Check (One_CPU > 0 and then Busy_Sum.Busy / Percent_Scale * Percent_Scale
               >= One_CPU / Percent_Scale * Spin_Floor_Percent,
             "a spinning thread drives one CPU's worth of busy time");
      Check (CL.Aggregate (Before, After).Known
             and then CL.Aggregate (Before, After).Busy >= Spin_Floor_Percent / Expected_Processors,
             "aggregate load shows the spinner");
   end;

   --  A snapshot's cost: loads only (no system call).
   declare
      T0, T1 : Unsigned_64;
      Nanoseconds_Per_Second : constant := 1_000_000_000;
   begin
      T0 := CuBit.Benchmark_Clock.Read_Counter;
      for I in 1 .. Read_Rounds loop
         CuBit.CPU_Times.Read (After);
      end loop;
      T1 := CuBit.Benchmark_Clock.Read_Counter;
      Print ("cpu-times-check: cost snapshot_ticks=" & Image ((T1 - T0) / Read_Rounds) &
             " snapshot_ns=" & Image ((T1 - T0) / Read_Rounds * Nanoseconds_Per_Second / Frequency) &
             " cpus=" & Image (Unsigned_64 (After.Count)) & LF);
   end;

   if Failures = 0 then
      Print ("TEST: PASS cpu-times" & LF);
   else
      Print ("TEST: FAIL cpu-times" & LF);
   end if;

   --  Last: the kernel must refuse a write to the page.
   declare
      Target : Unsigned_64
      with Import, Volatile,
           Address => System.Storage_Elements.To_Address (CTP.Page_Address);
   begin
      Print ("cpu-times-check: write to the CPU times page armed" & LF);
      Target := 0;
      Print ("cpu-times-check: FAIL write to the CPU times page succeeded" & LF);
   end;
end Main;
