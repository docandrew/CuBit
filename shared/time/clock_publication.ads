------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The clock publication (KERN-002, docs/fast-clock.md): one page the kernel
--  maps read-only into every process, from which user space reads monotonic
--  time with a memory load and RDTSC instead of a system call.
--
--  @description
--  Shared by the kernel (the writer) and the runtimes (the readers). The
--  page holds a seqlock counter and the parameters of one conversion:
--
--     nanoseconds = Base_Time + floor ((counter - Base_Ticks) * Scale / 2**32)
--
--  where counter is the invariant TSC and Scale is nanoseconds per tick in
--  32.32 fixed point. The kernel's own millisecond clock (Time.msTicks) is
--  this conversion sampled at its last CPU 0 tick, so a reader's
--  Milliseconds is never behind msTicks (docs/fast-clock.md, "Clocks").
--
--  Proved (tests/fast-clock): no wrap-around anywhere, for every page
--  content, not only a well-formed one (the reader validates first); the
--  conversion is monotonic in the counter and at most one nanosecond per
--  tick; 10 years at Maximum_Hz stays inside the convertible range. The
--  seqlock model there proves that a stable read returns one version the
--  writer completed, and that a rebased conversion never runs backward.
--
--  Readers retry at most Read_Attempts times and then fall back to the
--  system call, so a stalled writer cannot make them spin.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Clock_Publication with Pure, SPARK_Mode is

   --  Where the kernel maps the page in every process (read-only, never
   --  executable): its own top-level slot, above the launch arguments.
   Page_Address : constant := 16#0000_5B00_0000_0000#;
   Page_Bytes   : constant := 4096;

   --  Version word: the layout below, or not published (user space then
   --  uses the system calls). The page stays mapped either way.
   Layout_Version : constant := 1;
   Unpublished    : constant := 0;

   Nanoseconds_Per_Second      : constant := 1_000_000_000;
   Nanoseconds_Per_Millisecond : constant := 1_000_000;
   Nanoseconds_Per_Microsecond : constant := 1_000;

   --  Scale is nanoseconds per counter tick in 32.32 fixed point. A counter
   --  of at least 1 GHz needs at most one nanosecond per tick, which keeps
   --  every product inside 64 bits without a division on the read path.
   Fraction_Bits  : constant := 32;
   One_Nanosecond : constant := 2 ** Fraction_Bits;
   subtype Tick_Scale is Unsigned_64 range 1 .. One_Nanosecond;

   --  Counter rates the publication accepts. Invariant TSCs run at the
   --  CPU's nominal frequency, 1-6 GHz in practice; slower or faster
   --  counters keep the system calls.
   Minimum_Hz : constant := Nanoseconds_Per_Second;
   Maximum_Hz : constant := 16_000_000_000;
   subtype Counter_Frequency is Unsigned_64 range Minimum_Hz .. Maximum_Hz;

   --  Ticks since Base_Ticks that a reader converts; beyond, it reports
   --  unavailable. More than 18 years at Maximum_Hz.
   Maximum_Ticks : constant := 2 ** 63 - 1;
   subtype Elapsed_Ticks is Unsigned_64 range 0 .. Maximum_Ticks;

   --  The time at Base_Ticks. Base plus elapsed nanoseconds (at most one
   --  per tick) stays below 2**64.
   Maximum_Base : constant := 2 ** 62;
   subtype Base_Nanoseconds is Unsigned_64 range 0 .. Maximum_Base;

   --  The uptime the conversion is guaranteed for without a rebase: ten
   --  Julian years at the fastest admitted counter. Checked when compiled.
   Seconds_Per_Year : constant := 31_557_600;
   Horizon_Years    : constant := 10;
   Horizon_Ticks    : constant := Horizon_Years * Seconds_Per_Year * Maximum_Hz;
   pragma Compile_Time_Error
     (Horizon_Ticks > Maximum_Ticks,
      "ten years at Maximum_Hz exceed the convertible range");

   --  The conversion as published. Readers validate it (Valid) before use:
   --  the kernel writes it, but nothing here trusts the bytes.
   type Parameters is record
      Version    : Unsigned_64 := Unpublished;
      Frequency  : Unsigned_64 := 0;   --  counter ticks per second
      Scale      : Unsigned_64 := 0;   --  nanoseconds per tick, 32.32
      Base_Ticks : Unsigned_64 := 0;   --  counter value at Base_Time
      Base_Time  : Unsigned_64 := 0;   --  nanoseconds at Base_Ticks
   end record;
   for Parameters use record
      Version    at  0 range 0 .. 63;
      Frequency  at  8 range 0 .. 63;
      Scale      at 16 range 0 .. 63;
      Base_Ticks at 24 range 0 .. 63;
      Base_Time  at 32 range 0 .. 63;
   end record;
   for Parameters'Size use 5 * 64;

   --  The page: the seqlock counter, then the parameters it guards.
   type Page is record
      Sequence : Unsigned_64 := 0;
      Fields   : Parameters;
   end record;
   for Page use record
      Sequence at 0 range 0 .. 63;
      Fields   at 8 range 0 .. 5 * 64 - 1;
   end record;
   for Page'Size use 6 * 64;

   Not_Published : constant Parameters :=
     (Version => Unpublished, Frequency | Scale | Base_Ticks | Base_Time => 0);

   function Valid (P : Parameters) return Boolean is
     (P.Version = Layout_Version
      and then P.Frequency in Counter_Frequency
      and then P.Scale in Tick_Scale
      and then P.Base_Time in Base_Nanoseconds);

   --  32.32 nanoseconds per tick of a counter running at Frequency.
   function Scale_Of (Frequency : Counter_Frequency) return Tick_Scale is
     (Unsigned_64 (One_Nanosecond) * Nanoseconds_Per_Second / Frequency);

   --  The kernel's conversion: time Base_Time at counter Base_Ticks.
   function Initial
     (Frequency  : Counter_Frequency;
      Base_Ticks : Unsigned_64;
      Base_Time  : Base_Nanoseconds) return Parameters
   is ((Version    => Layout_Version,
        Frequency  => Frequency,
        Scale      => Scale_Of (Frequency),
        Base_Ticks => Base_Ticks,
        Base_Time  => Base_Time))
   with Post => Valid (Initial'Result);

   --  floor (Ticks * Scale / 2**32), without a 128-bit product.
   function Elapsed_Nanoseconds
     (Ticks : Elapsed_Ticks; Scale : Tick_Scale) return Unsigned_64
   with Post => Elapsed_Nanoseconds'Result
                  = Ticks / One_Nanosecond * Scale
                    + Ticks mod One_Nanosecond * Scale / One_Nanosecond
                and then Elapsed_Nanoseconds'Result <= Ticks,
        Inline;

   --  Ticks since Base_Ticks. A counter behind the base (another CPU's TSC
   --  a little behind the one that sampled the base) counts as the base.
   function Ticks_Since (P : Parameters; Counter : Unsigned_64)
     return Unsigned_64
   is (if Counter <= P.Base_Ticks then 0 else Counter - P.Base_Ticks);

   function Readable (P : Parameters; Counter : Unsigned_64) return Boolean is
     (Valid (P) and then Ticks_Since (P, Counter) <= Maximum_Ticks);

   --  Nanoseconds at Counter. Never wraps: Base_Time <= 2**62 and the
   --  elapsed part is at most Maximum_Ticks.
   function Time_At (P : Parameters; Counter : Unsigned_64)
     return Unsigned_64
   is (P.Base_Time + Elapsed_Nanoseconds (Ticks_Since (P, Counter), P.Scale))
   with Pre => Readable (P, Counter);

   function Microseconds (Nanoseconds : Unsigned_64) return Unsigned_64 is
     (Nanoseconds / Nanoseconds_Per_Microsecond);
   function Milliseconds (Nanoseconds : Unsigned_64) return Unsigned_64 is
     (Nanoseconds / Nanoseconds_Per_Millisecond);

   --  Validate and convert in one step: what a reader does with a stable
   --  snapshot and the counter it read inside it.
   procedure Convert
     (P           : Parameters;
      Counter     : Unsigned_64;
      Nanoseconds : out Unsigned_64;
      Success     : out Boolean)
   with Post => Success = Readable (P, Counter)
                and then (if Success then Nanoseconds = Time_At (P, Counter)
                          else Nanoseconds = 0),
        Inline;

   ---------------------------------------------------------------------------
   --  Seqlock protocol. The writer makes the counter odd, writes the
   --  parameters, then makes it even again. A reader keeps a snapshot only
   --  if it saw the same even counter before and after copying it.
   ---------------------------------------------------------------------------
   Read_Attempts : constant := 4;
   subtype Read_Attempt is Positive range 1 .. Read_Attempts;

   function Writing (Sequence : Unsigned_64) return Boolean is
     (Sequence mod 2 = 1);

   function Stable (Before, After : Unsigned_64) return Boolean is
     (Before = After and then not Writing (Before));

   --  The writer's two counter steps. The counter never wraps: 2**63
   --  updates are not reachable.
   function Opened (Sequence : Unsigned_64) return Unsigned_64 is
     (Sequence + 1)
   with Pre  => not Writing (Sequence) and then Sequence < Unsigned_64'Last - 1,
        Post => Writing (Opened'Result);
   function Closed (Sequence : Unsigned_64) return Unsigned_64 is
     (Sequence + 1)
   with Pre  => Writing (Sequence) and then Sequence < Unsigned_64'Last,
        Post => not Writing (Closed'Result) and then Closed'Result > Sequence;

   ---------------------------------------------------------------------------
   --  Lemmas (proved; used by tests/fast-clock).
   ---------------------------------------------------------------------------
   procedure Lemma_Elapsed_Monotonic
     (Earlier, Later : Elapsed_Ticks; Scale : Tick_Scale)
   with Ghost,
        Pre  => Earlier <= Later,
        Post => Elapsed_Nanoseconds (Earlier, Scale)
                  <= Elapsed_Nanoseconds (Later, Scale);

   procedure Lemma_Time_Monotonic
     (P : Parameters; Earlier, Later : Unsigned_64)
   with Ghost,
        Pre  => Readable (P, Earlier) and then Readable (P, Later)
                and then Earlier <= Later,
        Post => Time_At (P, Earlier) <= Time_At (P, Later);

end Clock_Publication;
