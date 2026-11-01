------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-thread growable secondary stack regression checks (headless --test
--  secondary-stacks; CuBit.Thread_Areas, CuBit.Secondary_Stack_Layout).
--
--  @description
--  The main thread (a 96 MiB declared primary stack) uses 20 MiB of it,
--  past the 16 MiB at which the old fixed secondary stack sat below the
--  stack top, and at the deepest point builds 9 MiB of unconstrained
--  results on its secondary stack, checking every byte both ways. Then two
--  extra threads, each with its own primary stack and thread area, do the
--  same while the main thread does it again. Every secondary stack must be
--  its own region, outside every primary stack, and the threads' must be
--  given back when they end. Last, the main thread grows its secondary
--  stack until its reservation (or the frame quota) refuses: the runtime
--  must report and raise Storage_Error, which ends the program through
--  the last-chance handler, not corrupt memory.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");
with CuBit.Messages; use CuBit.Messages;
with CuBit.Secondary_Stack_Layout;
with CuBit.System_Facts;
with Process_Layout;
with Stack_Threads; use Stack_Threads;
with Stack_Work; use Stack_Work;

procedure Main is
   use ASCII;
   package Layout renames CuBit.Secondary_Stack_Layout;

   --  The declared primary stack (component.ccl).
   Main_Stack_Bytes : constant := 96 * MiB;
   Main_Stack_Base : constant Unsigned_64 := Process_Layout.Stack_Top - Main_Stack_Bytes;

   --  More than the kernel's own bookkeeping for the threads (page tables)
   --  can explain; less than one worker's secondary stack.
   Release_Slack_Bytes : constant := 4 * MiB;

   --  The last phase's allocations: eight of them pass the reservation.
   Exhaust_Chunk_Bytes : constant := 32 * MiB;
   Exhaust_Chunks : constant := Layout.Reservation_Bytes / Exhaust_Chunk_Bytes;
   Marker : constant Unsigned_64 := 16#5EC0_57AC_0000_0000#;
   Marker_Bytes : constant := Unsigned_64'Size / System.Storage_Unit;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("secondary-stack-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("secondary-stack-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Owned_Bytes return Unsigned_64 is
      Value : CuBit.System_Facts.SP.Statistics;
      Success : Boolean;
   begin
      CuBit.System_Facts.Read_Statistics (Value, Success);
      return (if Success then Value.Owned_Bytes else Unsigned_64'Last);
   end Owned_Bytes;

   --  Grow the secondary stack by whole chunks, each marked at both ends
   --  and every mark checked after each growth, until the runtime refuses.
   procedure Exhaust is
      type Chunk_Addresses is array (1 .. Exhaust_Chunks) of System.Address;
      Chunks : Chunk_Addresses;
      function Mark_At (Where : System.Address) return Unsigned_64 is
         Item : constant Unsigned_64 with Import, Volatile, Address => Where;
      begin
         return Item;
      end Mark_At;
      procedure Put_Mark (Where : System.Address; Value : Unsigned_64) is
         Item : Unsigned_64 with Import, Volatile, Address => Where;
      begin
         Item := Value;
      end Put_Mark;
      function Last_Word (Chunk : System.Address) return System.Address is
        (System'To_Address (Value_Of (Chunk) + Exhaust_Chunk_Bytes - Marker_Bytes));
   begin
      for K in Chunks'Range loop
         System.Secondary_Stack.SS_Allocate (Chunks (K), Exhaust_Chunk_Bytes);
         Put_Mark (Chunks (K), Marker + Unsigned_64 (K));
         Put_Mark (Last_Word (Chunks (K)), not (Marker + Unsigned_64 (K)));
         for J in 1 .. K loop
            if Mark_At (Chunks (J)) /= Marker + Unsigned_64 (J)
              or else Mark_At (Last_Word (Chunks (J))) /= not (Marker + Unsigned_64 (J))
            then
               debugPrint ("secondary-stack-check: grown secondary stack corrupted FAIL" & LF);
            end if;
         end loop;
      end loop;
      debugPrint ("secondary-stack-check: the reservation never refused FAIL" & LF);
   end Exhaust;

   Alone, Together : Outcome;
   Threads : array (Worker_Index) of Started;
   Before, After : Unsigned_64;
begin
   debugPrint ("secondary-stack-check: starting" & LF);

   Alone := Run (1);
   Check (Alone.Passed, "main thread: 20 MiB primary, 9 MiB secondary, all intact");
   Check (not Inside (Alone.Result_Address, Main_Stack_Base, Main_Stack_Bytes)
            and then not Inside (Alone.Region, Main_Stack_Base, Main_Stack_Bytes),
          "main thread's secondary stack is outside its primary stack");

   --  The main thread's stacks are grown now; what the threads add must go.
   Before := Owned_Bytes;
   for I in Worker_Index loop
      Threads (I) := Start (I);
   end loop;
   Check ((for all T of Threads => T.Created), "extra threads started with their own areas");
   Together := Run (2);
   for I in Worker_Index loop
      Finish (I, Threads (I));
   end loop;
   After := Owned_Bytes;

   Check (Together.Passed and then Together.Region = Alone.Region,
          "main thread again, alongside the extra threads, intact");
   for I in Worker_Index loop
      Check (Results (I).Passed, "extra thread" & I'Image & ": 20 MiB primary, 9 MiB secondary, intact");
      Check (not Inside (Results (I).Result_Address, Threads (I).Stack, Stack_Threads.Stack_Bytes)
               and then not Inside (Results (I).Result_Address, Main_Stack_Base, Main_Stack_Bytes),
             "extra thread" & I'Image & "'s secondary stack is outside every primary stack");
   end loop;
   Check (Results (1).Region /= Results (2).Region
            and then (for all R of Results => R.Region /= Alone.Region and then R.Region /= 0)
            and then not Inside (Results (1).Region, Results (2).Region, Layout.Reservation_Bytes)
            and then not Inside (Results (2).Region, Results (1).Region, Layout.Reservation_Bytes),
          "every thread has its own secondary-stack region");
   Check (After /= Unsigned_64'Last and then After < Before + Release_Slack_Bytes,
          "ended threads gave back their secondary stacks");

   if Failures = 0 then
      debugPrint ("TEST: PASS secondary-stacks" & LF);
   else
      debugPrint ("TEST: FAIL secondary-stacks" & LF);
   end if;

   --  Ends the program: Storage_Error after the runtime's report.
   debugPrint ("secondary-stack-check: exhausting the secondary-stack reservation" & LF);
   Exhaust;
end Main;
