------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The system's time without CPU 0 (headless clock-keeper, Time.clockTick,
--  Timekeeping_Turns).
--
--  @description
--  On the N150 laptop (image 33) CPU 0's timer ran no clock work for about
--  ten seconds, and msTicks and every deadline measured on it stopped for
--  every process. A probe kernel (CLOCK_STALL_PROBE=1) reproduces that on
--  request: CPU 0's timer interrupts skip their clock work for
--  Clock_Stall_Probe.Stall_Ms (800 ms). During each such stall:
--  1. a receive with a deadline Wait_Ms ahead, which nothing answers, must
--     end on time: not before the deadline, at most Late_Ms after it;
--  2. a futex wait with such a deadline, on a word nobody wakes, likewise;
--  3. the kernel's msTicks (the page's millisecond word) must advance with
--     the page's clock over Watch_Ms.
--  With CPU 0 the only keeper, each ends with the stall instead (~800 ms).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with Clock_Publication;
with CuBit.Published_Clock;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;
   package K renames CuBit.Kernel_ABI;

   --  Clock_Stall_Probe's request and stall, in the kernel.
   Stall_Request : constant String := "CLOCKK-PROBE-STALL-CPU0";
   Stall_Ms : constant := 800;
   --  Long enough for CPU 0's next timer interrupt (at most 1 ms apart) to
   --  start skipping.
   Settle_Ms : constant := 20;
   --  Each deadline: well inside the stall.
   Wait_Ms : constant := 100;
   --  How late a deadline may end: a keeper's millisecond, a wake and a
   --  dispatch, with room for a loaded host.
   Late_Ms : constant := 30;
   --  How long msTicks is watched, and how far it may trail the page then.
   Watch_Ms : constant := 200;
   Trail_Ms : constant := 10;

   Failures : Natural := 0;

   procedure Print (Text : String) renames CuBit.Messages.debugPrint;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Print ("clock-keeper-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         Print ("clock-keeper-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Now return Unsigned_64 renames CuBit.Published_Clock.Milliseconds;

   procedure Spin_Until (Instant : Unsigned_64) is
   begin
      while Now < Instant loop
         null;
      end loop;
   end Spin_Until;

   --  Straight to the kernel's console write: the probe kernel reads it.
   --  Returns once CPU 0 is skipping its clock work.
   procedure Stall_CPU_0 is
      Ignore : constant Unsigned_64 := syscall
        (SYSCALL_WRITE, STDOUT,
         Unsigned_64 (System.Storage_Elements.To_Integer (Stall_Request'Address)),
         Stall_Request'Length);
   begin
      Spin_Until (Now + Settle_Ms);
   end Stall_CPU_0;

   --  Past the stall, so the next check starts its own.
   procedure Outlast_Stall (Started : Unsigned_64) is
   begin
      Spin_Until (Started + Stall_Ms + Settle_Ms);
   end Outlast_Stall;

   function On_Time (Took : Unsigned_64) return Boolean is
     (Took >= Wait_Ms and then Took <= Wait_Ms + Late_Ms);

   Kernel_Ms : Unsigned_64
     with Import, Volatile, Atomic,
          Address => System.Storage_Elements.To_Address
            (Clock_Publication.Milliseconds_Address);
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Print ("clock-keeper-check: starting" & LF);
   Check (CuBit.Published_Clock.Counter_Frequency /= 0, "clock publication present");

   --  1. A receive deadline.
   declare
      Started, Begun, Took : Unsigned_64;
      From : Process_ID;
      Msg : Message;
      Received : Boolean;
   begin
      Started := Now;
      Stall_CPU_0;
      Begun := Now;
      receiveUntil (Begun + Wait_Ms, From, Msg, Received);
      Took := Now - Begun;
      Print ("clock-keeper-check: receive deadline" & Natural'Image (Wait_Ms) &
             " ms ended after " & Image (Took) & " ms" & LF);
      Check (not Received and then On_Time (Took),
             "a receive deadline ends on time while CPU 0 runs no timer work");
      Outlast_Stall (Started);
   end;

   --  2. A futex deadline.
   declare
      Word : aliased Unsigned_32 := 0;
      Started, Begun, Took, Result : Unsigned_64;
   begin
      Started := Now;
      Stall_CPU_0;
      Begun := Now;
      Result := CuBit.Kernel_Calls.Call
        (K.Wait, Wait_ABI.Futex_Sources, Begun + Wait_Ms,
         Unsigned_64 (System.Storage_Elements.To_Integer (Word'Address)),
         Unsigned_64 (Word));
      Took := Now - Begun;
      Print ("clock-keeper-check: futex deadline" & Natural'Image (Wait_Ms) &
             " ms ended after " & Image (Took) & " ms" & LF);
      Check (Result = K.Futex_Timed_Out and then On_Time (Took),
             "a futex deadline ends on time while CPU 0 runs no timer work");
      Outlast_Stall (Started);
   end;

   --  3. msTicks keeps up with the page.
   declare
      Started, First_Ms, Last_Ms, Page_Last : Unsigned_64;
   begin
      Started := Now;
      Stall_CPU_0;
      First_Ms := Kernel_Ms;
      Spin_Until (Now + Watch_Ms);
      Last_Ms := Kernel_Ms;
      Page_Last := Now;
      Print ("clock-keeper-check: msTicks advanced " & Image (Last_Ms - First_Ms) &
             " ms over" & Natural'Image (Watch_Ms) & " ms" & LF);
      Check (Last_Ms <= Page_Last and then Page_Last - Last_Ms <= Trail_Ms,
             "msTicks advances while CPU 0 runs no timer work");
      Outlast_Stall (Started);
   end;

   if Failures = 0 then
      Print ("TEST: PASS clock-keeper" & LF);
   else
      Print ("TEST: FAIL clock-keeper" & LF);
   end if;
   Ignore_Exit : declare
      Ignore : constant Unsigned_64 := syscall (SYSCALL_EXIT, 0);
   begin
      null;
   end Ignore_Exit;
end Main;
