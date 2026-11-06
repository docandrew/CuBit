------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-CPU time (KERN-002 publication, MET-001 source): how long each CPU
--  has been idle, running threads, and in the kernel's scheduler since it
--  started, in counter ticks.
--
--  @description
--  One page the kernel writes and maps read-only and non-executable only
--  into processes whose manifest requests it and whose launcher grants it,
--  (request-publication cpu-times): which CPUs are busy and when is a side
--  channel on other processes' activity, so it is not in every process
--  like the clock and the system facts.
--
--  Unit: ticks of the invariant counter the kernel's clocks use (the TSC),
--  Tick_Frequency per second (the header). Load over an interval does not
--  need the rate: it is a ratio of tick deltas.
--
--  Each CPU's record has its own seqlock (Sequence, as the clock page's)
--  and its own 64-byte line: only that CPU writes it, at each scheduling
--  boundary the kernel already accounts (Process.accountBoundary), with
--  no lock beyond the one that path already holds. Counters only grow: a
--  boundary adds the interval since Since to the category of what ran (the
--  idle thread, a thread, or the scheduler between them), saturating at
--  Unsigned_64'Last (about 36 years at 16 GHz). The interval in progress
--  belongs to State and is not yet in the counters: a reader adds
--  Counter - Since to State's category (Current_Totals), so a CPU that
--  idles in HLT for seconds without a boundary still reads as idle.
--
--  A CPU the kernel has not started is Offline; one whose accounting clock
--  faulted (Execution_Accounting: clock reversed, owner mismatch) is
--  Unaccounted from then on, and its counters stop.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;

package CPU_Times_Publication with Pure, SPARK_Mode is

   Page_Bytes   : constant := Clock_Publication.Page_Bytes;
   --  After the clock page, the three system publications and the
   --  device-memory diagnostics.
   Page_Address : constant := Clock_Publication.Page_Address + 5 * Page_Bytes;

   Layout_Version : constant := 1;
   Unpublished    : constant := 0;

   --  One cache line per record (no false sharing between writers).
   Line_Bytes   : constant := 64;
   Header_Bytes : constant := Line_Bytes;
   Record_Bytes : constant := Line_Bytes;

   --  Named ceiling: the records one page holds after its header. The
   --  kernel checks its own CPU bound against it at compile time.
   Maximum_Processors : constant := (Page_Bytes - Header_Bytes) / Record_Bytes;
   subtype Processor_Count is Unsigned_64 range 0 .. Maximum_Processors;
   subtype Processor is Natural range 0 .. Maximum_Processors - 1;

   ---------------------------------------------------------------------------
   --  What a CPU is doing since Since.
   ---------------------------------------------------------------------------
   type CPU_State is (Offline, Idle, Running, Scheduling, Unaccounted);
   for CPU_State use
     (Offline => 0, Idle => 1, Running => 2, Scheduling => 3, Unaccounted => 4);

   --  Where an accounted interval goes.
   subtype Charged_State is CPU_State range Idle .. Scheduling;

   --  Idle: the CPU's idle thread. Thread: threads, with their system
   --  calls and the interrupts they took. Scheduler: the kernel's
   --  scheduler between threads.
   type Times is record
      Idle_Ticks      : Unsigned_64 := 0;
      Thread_Ticks    : Unsigned_64 := 0;
      Scheduler_Ticks : Unsigned_64 := 0;
   end record;

   type Fields is record
      State       : Unsigned_64 := CPU_State'Enum_Rep (Offline);
      Since       : Unsigned_64 := 0;     --  counter at the last boundary
      Totals      : Times;
      Dispatches  : Unsigned_64 := 0;     --  boundaries that started a thread
   end record;
   for Times use record
      Idle_Ticks      at 0 range 0 .. 63;
      Thread_Ticks    at 8 range 0 .. 63;
      Scheduler_Ticks at 16 range 0 .. 63;
   end record;
   for Fields use record
      State      at 0 range 0 .. 63;
      Since      at 8 range 0 .. 63;
      Totals     at 16 range 0 .. 3 * 64 - 1;
      Dispatches at 40 range 0 .. 63;
   end record;
   for Fields'Size use 6 * 64;

   type CPU_Record is record
      Sequence : Unsigned_64 := 0;
      Values   : Fields;
      Reserved : Unsigned_64 := 0;
   end record;
   for CPU_Record use record
      Sequence at 0 range 0 .. 63;
      Values   at 8 range 0 .. 6 * 64 - 1;
      Reserved at 56 range 0 .. 63;
   end record;
   for CPU_Record'Size use Record_Bytes * 8;

   type Header is record
      Version         : Unsigned_64 := Unpublished;
      --  CPUs the kernel starts (records 0 .. Processors - 1 are written).
      Processors      : Unsigned_64 := 0;
      --  Counter ticks per second; 0: unknown (load is still a ratio).
      Tick_Frequency  : Unsigned_64 := 0;
      --  Free physical memory, bytes: one aligned word written with one
      --  store (no seqlock), refreshed by a CPU crossing a boundary at most
      --  every Memory_Refresh_Microseconds of its own.
      Free_Memory_Bytes : Unsigned_64 := 0;
      --  RAM the kernel manages, bytes (written once).
      Memory_Bytes    : Unsigned_64 := 0;
   end record;
   for Header use record
      Version           at 0 range 0 .. 63;
      Processors        at 8 range 0 .. 63;
      Tick_Frequency    at 16 range 0 .. 63;
      Free_Memory_Bytes at 24 range 0 .. 63;
      Memory_Bytes      at 32 range 0 .. 63;
   end record;
   for Header'Size use 5 * 64;

   Memory_Refresh_Microseconds : constant := 10_000;

   type Record_Table is array (Processor) of CPU_Record;

   type Page is record
      Head    : Header;
      Records : Record_Table;
   end record;
   for Page use record
      Head    at 0 range 0 .. 5 * 64 - 1;
      Records at Header_Bytes range 0 .. Maximum_Processors * Record_Bytes * 8 - 1;
   end record;
   pragma Compile_Time_Error (Page'Size / 8 > Page_Bytes,
                              "CPU times exceed one page");

   function State_Of (Word : Unsigned_64) return CPU_State is
     (case Word is
         when 1 => Idle, when 2 => Running, when 3 => Scheduling,
         when 4 => Unaccounted, when others => Offline);

   ---------------------------------------------------------------------------
   --  Counters
   ---------------------------------------------------------------------------
   function Saturating_Add (Left, Right : Unsigned_64) return Unsigned_64 is
     (if Right > Unsigned_64'Last - Left then Unsigned_64'Last else Left + Right);

   function Charge (T : Times; To : Charged_State; Ticks : Unsigned_64) return Times is
     (case To is
         when Idle       => (T with delta Idle_Ticks => Saturating_Add (T.Idle_Ticks, Ticks)),
         when Running    => (T with delta Thread_Ticks => Saturating_Add (T.Thread_Ticks, Ticks)),
         when Scheduling => (T with delta Scheduler_Ticks => Saturating_Add (T.Scheduler_Ticks, Ticks)));

   --  Every counter of Later is at least Earlier's.
   function Grown (Earlier, Later : Times) return Boolean is
     (Earlier.Idle_Ticks <= Later.Idle_Ticks
      and then Earlier.Thread_Ticks <= Later.Thread_Ticks
      and then Earlier.Scheduler_Ticks <= Later.Scheduler_Ticks);

   --  The writer's step at a boundary: the interval since the last one
   --  (Ticks, measured by the kernel's accounting clock) goes to what ran
   --  (Ran), and Next runs from Now. Started: Next is a thread the
   --  scheduler dispatched.
   function Accounted
     (F : Fields; Ran : Charged_State; Ticks : Unsigned_64;
      Next : Charged_State; Now : Unsigned_64; Started : Boolean) return Fields
   is ((State      => CPU_State'Enum_Rep (Next),
        Since      => Now,
        Totals     => Charge (F.Totals, Ran, Ticks),
        Dispatches => (if Started then Saturating_Add (F.Dispatches, 1) else F.Dispatches)))
   with Post => Grown (F.Totals, Accounted'Result.Totals)
                and then F.Dispatches <= Accounted'Result.Dispatches
                and then State_Of (Accounted'Result.State) = Next;

   --  Accounting stopped on this CPU (its clock faulted): counters kept.
   function Stopped (F : Fields; Now : Unsigned_64) return Fields is
     ((F with delta State => CPU_State'Enum_Rep (Unaccounted), Since => Now))
   with Post => Stopped'Result.Totals = F.Totals;

   --  The totals including the interval in progress at Counter.
   function Current_Totals (F : Fields; Counter : Unsigned_64) return Times is
     (if State_Of (F.State) in Charged_State and then Counter > F.Since
      then Charge (F.Totals, State_Of (F.State), Counter - F.Since)
      else F.Totals)
   with Post => Grown (F.Totals, Current_Totals'Result);

   ---------------------------------------------------------------------------
   --  The seqlock (the clock page's protocol, per record)
   ---------------------------------------------------------------------------
   Read_Attempts : constant := Clock_Publication.Read_Attempts;
   subtype Read_Attempt is Positive range 1 .. Read_Attempts;

   function Writing (Sequence : Unsigned_64) return Boolean is
     (Sequence mod 2 = 1);
   function Stable (Before, After : Unsigned_64) return Boolean is
     (Before = After and then not Writing (Before));

   --  The writer's two counter stores. Assumed, not proved, for the
   --  kernel's writer: the counter never reaches Unsigned_64'Last (2**63
   --  boundaries per CPU: about 14,000 years at ten million a second).
   function Opened (Sequence : Unsigned_64) return Unsigned_64 is
     (Sequence + 1)
   with Pre  => not Writing (Sequence) and then Sequence < Unsigned_64'Last - 1,
        Post => Writing (Opened'Result) and then Opened'Result > Sequence;
   function Closed (Sequence : Unsigned_64) return Unsigned_64 is
     (Sequence + 1)
   with Pre  => Writing (Sequence) and then Sequence < Unsigned_64'Last,
        Post => not Writing (Closed'Result) and then Closed'Result > Sequence;

   ---------------------------------------------------------------------------
   --  A reader's view of one CPU.
   ---------------------------------------------------------------------------
   type Reading_Kind is (Not_Present, Unavailable, Accounted_Times);
   --  Totals and Dispatches mean something only when Accounted_Times.
   type CPU_Reading is record
      Kind       : Reading_Kind := Not_Present;
      Totals     : Times;
      Dispatches : Unsigned_64 := 0;
   end record;

   --  What a stable copy F, read at Counter, says.
   function Reading_Of (F : Fields; Counter : Unsigned_64) return CPU_Reading is
     (case State_Of (F.State) is
         when Offline     => (Kind => Not_Present, others => <>),
         when Unaccounted => (Kind => Unavailable, others => <>),
         when Charged_State =>
           (Kind => Accounted_Times, Totals => Current_Totals (F, Counter),
            Dispatches => F.Dispatches));

end CPU_Times_Publication;
