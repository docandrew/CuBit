-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Virtual-deadline scheduling policy (docs/scheduler.md)
--
-- Pure SPARK rules of the MuQSS-style scheduler. The earliest virtual
-- deadline runs. A deadline changes only when a slice is used up: the fresh
-- slice sets deadline = now + slice. A sleeper keeps its deadline, even one
-- already passed, so a thread that runs briefly and sleeps runs first when
-- it wakes. It gets no more than the rest of its slice that way, so waiting
-- stays bounded (Kolivas's BFS rule). All times are ordered TSC ticks.
--
-- Run keys order ready lists: an ordinary thread's key is its deadline, and
-- the idle thread's is Idle_Key, after every deadline.
--
-- The two placement rules, where a woken thread goes and which ready list a
-- CPU takes its next thread from, are pure functions of per-CPU keys the
-- kernel gathers under its scheduler lock.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Virtual_Deadlines with Pure, SPARK_Mode => On is

   subtype Ticks is Unsigned_64;

   -- The idle thread's key, and a CPU's running key while it is idle.
   Idle_Key : constant Ticks := Ticks'Last;
   subtype Deadline is Ticks range 0 .. Idle_Key - 1;

   -- A slice long enough for any policy (about 5 minutes at 4 GHz); the
   -- bound keeps deadline arithmetic exact.
   Maximum_Slice : constant Ticks := 2 ** 40;
   subtype Slice_Length is Ticks range 1 .. Maximum_Slice;

   ---------------------------------------------------------------------------
   -- Deadlines
   ---------------------------------------------------------------------------

   -- The deadline of a fresh slice starting at Now, saturating at the last
   -- ordinary deadline.
   function Refill (Now : Deadline; Slice : Slice_Length) return Deadline
     with Post =>
       (if Now <= Deadline'Last - Slice then Refill'Result = Now + Slice
        else Refill'Result = Deadline'Last);

   -- A dispatch costs at least Minimum, so a thread that wakes and sleeps
   -- every microsecond pays for its context switches.
   function Dispatch_Charge (Ran, Minimum : Ticks) return Ticks is
     (Ticks'Max (Ran, Minimum))
     with Post => Dispatch_Charge'Result >= Ran and then
                  Dispatch_Charge'Result >= Minimum;

   -- Wakee runs ahead of Running only if earlier by more than Margin, which
   -- keeps near-equal deadlines from preempting each other back and forth.
   -- An idle CPU (Idle_Key) is preempted by any deadline.
   function Preempts (Wakee, Running, Margin : Ticks) return Boolean is
     (Running = Idle_Key or else
      (Running > Margin and then Wakee < Running - Margin));

   ---------------------------------------------------------------------------
   -- Placement over CPUs
   ---------------------------------------------------------------------------

   Maximum_CPUs : constant := 256;
   type CPU is range 0 .. Maximum_CPUs - 1;
   type CPU_Keys is array (CPU range <>) of Ticks;
   type CPU_Flags is array (CPU range <>) of Boolean;

   -- A CPU with nothing to do: running its idle thread, nothing queued.
   function Idle (Running, Queued : CPU_Keys; C : CPU) return Boolean is
     (Running (C) = Idle_Key and then Queued (C) = Idle_Key)
     with Pre => C in Running'Range and then C in Queued'Range;

   -- Where a thread woken with deadline Wakee runs. Running and Queued hold
   -- each CPU's running key and first queued key; Allowed marks the CPUs
   -- this thread may use (online, in its peek domain). In order:
   --   1. pinned: Home;
   --   2. Home, if the wakee preempts there and nothing queued there is
   --      earlier;
   --   3. an allowed idle CPU, searching from Home + 1;
   --   4. the allowed CPU running the latest key, if the wakee preempts it;
   --   5. Home, to queue.
   function Place
     (Home    : CPU;
      Wakee   : Deadline;
      Pinned  : Boolean;
      Running : CPU_Keys;
      Queued  : CPU_Keys;
      Allowed : CPU_Flags;
      Margin  : Ticks) return CPU
     with
       Pre =>
         Running'First = 0 and then Running'Length > 0 and then
         Queued'First = 0 and then Allowed'First = 0 and then
         Queued'Last = Running'Last and then Allowed'Last = Running'Last and then
         Home in Running'Range,
       Post =>
         Place'Result in Running'Range and then
         (if Pinned then Place'Result = Home) and then
         (if Place'Result /= Home then
            Allowed (Place'Result) and then
            (Idle (Running, Queued, Place'Result) or else
             Preempts (Wakee, Running (Place'Result), Margin)));

   -- Which ready list CPU Own takes its next thread from. Heads holds each
   -- list's first key this CPU may take (Idle_Key: none); Allowed marks
   -- the lists in its peek domain. Own's list wins unless an allowed list
   -- holds a key earlier by more than Margin, so work moves between CPUs
   -- only when the difference matters.
   function Choose
     (Own     : CPU;
      Heads   : CPU_Keys;
      Allowed : CPU_Flags;
      Margin  : Ticks) return CPU
     with
       Pre =>
         Heads'First = 0 and then Heads'Length > 0 and then
         Allowed'First = 0 and then Allowed'Last = Heads'Last and then
         Own in Heads'Range,
       Post =>
         Choose'Result in Heads'Range and then
         (if Choose'Result /= Own then
            Allowed (Choose'Result) and then
            Heads (Choose'Result) /= Idle_Key and then
            Preempts (Heads (Choose'Result), Heads (Own), Margin)) and then
         -- No allowed list holds a key earlier than the chosen one by more
         -- than Margin, when another list was chosen.
         (if Choose'Result /= Own then
            (for all C in Heads'Range =>
               (if Allowed (C) then Heads (Choose'Result) <= Heads (C))));

end Virtual_Deadlines;
