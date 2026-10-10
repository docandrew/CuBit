------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Clock publication checks (KERN-002, docs/fast-clock.md), for a CPU with
--  an invariant TSC so the kernel publishes the page.
--
--  A million page reads never run backward; the page agrees with
--  READ_MONOTONIC_MICROSECONDS (same epoch) and is never behind GETTIME
--  (msTicks), and by at most a few ticks; kernel deadlines computed from
--  the page do not expire early; the cost of a page read against the
--  system calls it replaces. Finally a write to the page must fault: the
--  kernel maps it read-only.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements;
with Clock_Publication;
with CuBit.Benchmark_Clock;
with CuBit.Published_Clock;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;

procedure Main is
   use ASCII;
   package K renames CuBit.Kernel_ABI;

   Reads               : constant := 1_000_000;
   System_Call_Reads   : constant := 100_000;
   Agreement_Rounds    : constant := 10_000;
   --  GETTIME is the page's time at CPU 0's last tick (250 us apart).
   Maximum_Lag_Ms      : constant := 10;
   Deadline_Ms         : constant := 30;
   Sleep_Microseconds  : constant := 5_000;

   Failures : Natural := 0;

   procedure Print (Text : String) renames CuBit.Messages.debugPrint;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Print ("clock-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         Print ("clock-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Kernel (Number : K.System_Call; A0, A1, A2 : Unsigned_64 := 0)
     return Unsigned_64 is (CuBit.Kernel_Calls.Call (Number, A0, A1, A2));

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   Frequency : constant Unsigned_64 := CuBit.Published_Clock.Counter_Frequency;
   Published : constant Boolean := Frequency /= 0;

   Page_Cost, Milliseconds_Cost, Microsecond_Call_Cost,
     Get_Time_Cost : Unsigned_64 := 0;

   --  Costs are timed with the TSC, so they are comparable whether or not
   --  the page is published (without it, a page read is a system call).
   Ticks_Per_Millisecond : Unsigned_64 := 0;
   Picoseconds_Per_Millisecond : constant := 1_000_000_000;

   function Ticks return Unsigned_64 renames CuBit.Benchmark_Clock.Read_Counter;

   --  Nanoseconds per operation, from Elapsed TSC ticks over Count.
   function Per_Operation (Elapsed, Count : Unsigned_64) return Unsigned_64 is
     (if Ticks_Per_Millisecond = 0 then 0
      else Elapsed * (Picoseconds_Per_Millisecond / Count) / Ticks_Per_Millisecond / 1_000);
begin
   Print ("clock-check: starting" & LF);
   Check (Published, "clock publication present");
   Print ("clock-check: TSC " & Image (Frequency) & " Hz" & LF);
   if Published then
      Ticks_Per_Millisecond := Frequency / 1_000;
   else
      CuBit.Benchmark_Clock.Calibrate (Ticks_Per_Millisecond);
   end if;

   --  A million reads, each at or after the one before.
   declare
      Previous, Value : Unsigned_64;
      Ok : Boolean;
      Monotonic, All_Published : Boolean := True;
      Start : constant Unsigned_64 := Ticks;
   begin
      CuBit.Published_Clock.Read_Nanoseconds (Previous, Ok);
      for I in 1 .. Reads loop
         CuBit.Published_Clock.Read_Nanoseconds (Value, Ok);
         All_Published := All_Published and then Ok;
         Monotonic := Monotonic and then Value >= Previous;
         Previous := Value;
      end loop;
      Page_Cost := Per_Operation (Ticks - Start, Reads);
      Check (All_Published and then Monotonic,
             "1000000 page reads are published and monotonic");
   end;

   declare
      Previous, Value : Unsigned_64 := 0;
      Monotonic : Boolean := True;
      Start : constant Unsigned_64 := Ticks;
   begin
      for I in 1 .. Reads loop
         Value := CuBit.Published_Clock.Milliseconds;
         Monotonic := Monotonic and then Value >= Previous;
         Previous := Value;
      end loop;
      Milliseconds_Cost := Per_Operation (Ticks - Start, Reads);
      Check (Monotonic, "1000000 page millisecond reads are monotonic");
   end;

   --  The system calls the page replaces.
   declare
      Ignore : Unsigned_64;
      Start : Unsigned_64 := Ticks;
   begin
      for I in 1 .. System_Call_Reads loop
         Ignore := Kernel (K.Read_Monotonic_Microseconds);
      end loop;
      Microsecond_Call_Cost := Per_Operation (Ticks - Start, System_Call_Reads);
      Start := Ticks;
      for I in 1 .. System_Call_Reads loop
         Ignore := Kernel (K.Get_Time);
      end loop;
      Get_Time_Cost := Per_Operation (Ticks - Start, System_Call_Reads);
   end;
   Print ("clock-check: cost ns/read page=" & Image (Page_Cost) &
          " page_ms=" & Image (Milliseconds_Cost) &
          " syscall_us=" & Image (Microsecond_Call_Cost) &
          " syscall_ms=" & Image (Get_Time_Cost) & LF);
   Check (Page_Cost < Microsecond_Call_Cost and then Page_Cost < Get_Time_Cost,
          "a page read is cheaper than either system call");

   --  One epoch with the kernel's clocks.
   declare
      Before, Kernel_Us, After, Get_Time, Lag, Max_Lag : Unsigned_64 := 0;
      Ok : Boolean;
      Agree, Not_Behind : Boolean := True;
   begin
      for I in 1 .. Agreement_Rounds loop
         CuBit.Published_Clock.Microseconds (Before, Ok);
         Kernel_Us := Kernel (K.Read_Monotonic_Microseconds);
         CuBit.Published_Clock.Microseconds (After, Ok);
         Agree := Agree and then Before <= Kernel_Us and then Kernel_Us <= After;
         Get_Time := Kernel (K.Get_Time);
         After := CuBit.Published_Clock.Milliseconds;
         Not_Behind := Not_Behind and then Get_Time <= After;
         Lag := (if After >= Get_Time then After - Get_Time else 0);
         Max_Lag := Unsigned_64'Max (Max_Lag, Lag);
      end loop;
      Print ("clock-check: GETTIME at most " & Image (Max_Lag) &
             " ms behind the page" & LF);
      Check (Agree, "READ_MONOTONIC_MICROSECONDS lies between two page reads");
      Check (Not_Behind and then Max_Lag <= Maximum_Lag_Ms,
             "the page is never behind GETTIME, and at most 10 ms ahead");
   end;

   --  Kernel deadlines (msTicks) computed from the page.
   declare
      Word : aliased Unsigned_32 := 0;
      Deadline : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds + Deadline_Ms;
      Result : constant Unsigned_64 := Kernel
        (K.Futex_Wait,
         Unsigned_64 (System.Storage_Elements.To_Integer (Word'Address)),
         Unsigned_64 (Word), Deadline);
      Woke : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
   begin
      Check (Result = K.Futex_Timed_Out and then Woke >= Deadline,
             "a futex deadline from the page does not expire early");
   end;

   declare
      Start, Woke : Unsigned_64;
      Ok : Boolean;
      Ignore : Unsigned_64;
   begin
      CuBit.Published_Clock.Microseconds (Start, Ok);
      Ignore := Kernel (K.Sleep_Until_Monotonic_Microsecond,
                        Start + Sleep_Microseconds);
      CuBit.Published_Clock.Microseconds (Woke, Ok);
      Check (Woke >= Start + Sleep_Microseconds,
             "sleeping until a page microsecond does not return early");
   end;

   if Failures = 0 then
      Print ("TEST: PASS clock" & LF);
   else
      Print ("TEST: FAIL clock" & LF);
   end if;

   --  Last: the kernel must refuse a write to the page.
   declare
      Target : Unsigned_64
      with Import, Volatile,
           Address => System.Storage_Elements.To_Address
                        (Clock_Publication.Page_Address);
   begin
      Print ("clock-check: write to the clock page armed" & LF);
      Target := 0;
      Print ("clock-check: FAIL write to the clock page succeeded" & LF);
   end;
end Main;
