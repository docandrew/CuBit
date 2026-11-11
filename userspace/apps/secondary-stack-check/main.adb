------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-thread secondary stack regression checks (headless --test
--  secondary-stacks; System.Secondary_Stack, CuBit.Thread_Areas,
--  CuBit.Secondary_Stack_Layout).
--
--  @description
--  The main thread (a 96 MiB declared primary stack) uses 20 MiB of it,
--  past the 16 MiB at which the old fixed secondary stack sat below the
--  stack top, and at the deepest point builds 9 MiB of unconstrained
--  results on its secondary stack, checking every byte both ways; a result
--  past the ceiling is refused with Storage_Error and the CAPACITY message,
--  handled, and the stack works after it; then it grows across 160 chunks
--  at once, releases, and grows again on the same chunks. Then four extra
--  threads, each with its own primary stack and thread area, do the same
--  while the main thread does it again, twice over: the second batch must
--  not take more memory than the first (ended threads gave their stacks
--  and heaps back). Then the main thread grows its secondary stack until
--  the runtime refuses (the ceiling, or the frame quota), handles the
--  Storage_Error and carries on. Last, a result past the ceiling that
--  nobody handles ends the program through the last-chance handler: the
--  CAPACITY message on the console and exit code 134 (the typed
--  Unhandled_Exception outcome), not corrupted memory.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Exceptions;
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
   package SS renames System.Secondary_Stack;
   package Layout renames CuBit.Secondary_Stack_Layout;
   use type SS.Mark_Id;

   --  The declared primary stack (component.ccl).
   Main_Stack_Bytes : constant := 96 * MiB;
   Main_Stack_Base : constant Unsigned_64 := Process_Layout.Stack_Top - Main_Stack_Bytes;

   --  What a second batch of threads may take beyond the first: less than
   --  one worker's growth (160 chunks of 64 KiB) or its results (9 MiB).
   Batch_Slack_Bytes : constant := 4 * MiB;

   --  The exhaustion's allocations: whole 32 MiB pieces until refused.
   Exhaust_Piece_Bytes : constant := 32 * MiB;
   --  The most that fit under the ceiling beside the static chunk.
   Pieces_Under_Ceiling : constant := (Layout.Ceiling_Bytes - Layout.Chunk_Bytes) / Exhaust_Piece_Bytes;
   Exhaust_Pieces : constant := Layout.Ceiling_Bytes / Exhaust_Piece_Bytes + 1;
   Marker : constant Unsigned_64 := 16#5EC0_57AC_0000_0000#;
   Marker_Bytes : constant := Unsigned_64'Size / System.Storage_Unit;

   --  The seeds of the main thread's runs.
   Alone_Seed : constant := 1;
   Together_Seed : constant := 2;
   After_Exhaustion_Seed : constant := 3;
   Past_Ceiling_Seed : constant := 4;

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

   function Outside_Main_Stack (Where : Unsigned_64) return Boolean is
     (not Inside (Where, Main_Stack_Base, Main_Stack_Bytes));

   --  The checks every Full_Run result must pass, named after Who.
   procedure Check_Run (Found : Outcome; Who : String) is
   begin
      Check (Found.Passed, Who & ": 20 MiB primary, 9 MiB secondary, intact");
      Check (Found.Refused and then Found.Continued,
             Who & ": a result past the ceiling refused (CAPACITY CEILING_REACHED), handled, stack usable");
      Check (Found.Grown,
             Who & ": grew across" & Found.Chunks'Image & " chunks, intact, released to its mark");
      Check (Found.Reused, Who & ": grew again on the same chunks after the release");
   end Check_Run;

   --  One batch: the workers and the main thread at once.
   function Batch return Outcome is
      Threads : array (Worker_Index) of Started;
      Together : Outcome;
   begin
      Results := [others => (others => <>)];
      for I in Worker_Index loop
         Threads (I) := Start (I);
      end loop;
      Check ((for all T of Threads => T.Created), "extra threads started with their own areas");
      Together := Full_Run (Together_Seed);
      for I in Worker_Index loop
         Finish (I, Threads (I));
      end loop;
      for I in Worker_Index loop
         Check (not Inside (Results (I).Result_Address, Threads (I).Stack, Stack_Threads.Stack_Bytes)
                  and then Outside_Main_Stack (Results (I).Result_Address),
                "extra thread" & I'Image & "'s secondary stack is outside every primary stack");
      end loop;
      return Together;
   end Batch;

   --  Grow the main thread's secondary stack by whole pieces, each marked
   --  at both ends and every mark checked after each growth, until the
   --  runtime refuses; handle the Storage_Error and go on.
   procedure Exhaust_And_Continue is
      type Piece_Addresses is array (1 .. Exhaust_Pieces) of System.Address;
      Pieces : Piece_Addresses;
      Placed : Natural := 0;
      Intact : Boolean := True;
      Refused : Boolean := False;
      Reason_Ceiling, Reason_Heap : Boolean := False;
      Mark : constant SS.Mark_Id := SS.SS_Mark;
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
      function Last_Word (Piece : System.Address) return System.Address is
        (System'To_Address (Value_Of (Piece) + Exhaust_Piece_Bytes - Marker_Bytes));
   begin
      begin
         for K in Pieces'Range loop
            SS.SS_Allocate (Pieces (K), Exhaust_Piece_Bytes);
            Placed := K;
            Put_Mark (Pieces (K), Marker + Unsigned_64 (K));
            Put_Mark (Last_Word (Pieces (K)), not (Marker + Unsigned_64 (K)));
            for J in 1 .. K loop
               if Mark_At (Pieces (J)) /= Marker + Unsigned_64 (J)
                 or else Mark_At (Last_Word (Pieces (J))) /= not (Marker + Unsigned_64 (J))
               then
                  Intact := False;
               end if;
            end loop;
         end loop;
      exception
         when E : Storage_Error =>
            Refused := True;
            declare
               Message : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Reason_Ceiling := Is_Capacity (Message, "CEILING_REACHED");
               Reason_Heap := Is_Capacity (Message, "HEAP_REFUSED");
               debugPrint ("secondary-stack-check: refused after" & Placed'Image
                           & " pieces of 32 MiB: " & Message & LF);
            end;
      end;
      Check (Refused and then (Reason_Ceiling or else Reason_Heap),
             "exhaustion caught as Storage_Error with a CAPACITY reason");
      Check (not Reason_Ceiling or else Placed = Pieces_Under_Ceiling,
             "the ceiling refused exactly the piece that would pass it");
      for J in 1 .. Placed loop
         if Mark_At (Pieces (J)) /= Marker + Unsigned_64 (J)
           or else Mark_At (Last_Word (Pieces (J))) /= not (Marker + Unsigned_64 (J))
         then
            Intact := False;
         end if;
      end loop;
      Check (Intact, "everything grown before the refusal is intact");
      SS.SS_Release (Mark);
      Check (SS.SS_Mark = Mark, "released to the mark after the refusal");
   end Exhaust_And_Continue;

   Alone, Together, Again, After : Outcome;
   Before, After_First, After_Second : Unsigned_64;
begin
   debugPrint ("secondary-stack-check: starting" & LF);

   Alone := Full_Run (Alone_Seed);
   Check_Run (Alone, "main thread");
   Check (Outside_Main_Stack (Alone.Result_Address) and then Outside_Main_Stack (Alone.Stack),
          "main thread's secondary stack is outside its primary stack");

   --  Two batches of threads; the second may not need more than the first.
   Before := Owned_Bytes;
   Together := Batch;
   After_First := Owned_Bytes;
   Check_Run (Together, "main thread alongside the extra threads");
   Check (Together.Stack = Alone.Stack, "the main thread kept its stack");
   for I in Worker_Index loop
      Check_Run (Results (I), "extra thread" & I'Image);
   end loop;
   Check ((for all I in Worker_Index =>
             Results (I).Stack /= 0 and then Results (I).Stack /= Alone.Stack
             and then (for all J in Worker_Index => I = J or else Results (I).Stack /= Results (J).Stack)),
          "every thread has its own secondary stack");
   Again := Batch;
   After_Second := Owned_Bytes;
   Check (Again.Passed and then (for all R of Results => R.Passed and then R.Grown and then R.Refused),
          "a second batch of threads, intact");
   Check (Before /= Unsigned_64'Last and then After_Second /= Unsigned_64'Last
            and then After_Second < After_First + Batch_Slack_Bytes,
          "ended threads gave back their secondary stacks (a second batch needs no more memory)");

   --  The main thread runs out, handles it, and carries on.
   Exhaust_And_Continue;
   After := Run (After_Exhaustion_Seed);
   Check (After.Passed, "after the exhaustion, the program continues: results intact");

   if Failures = 0 then
      debugPrint ("TEST: PASS secondary-stacks" & LF);
   else
      debugPrint ("TEST: FAIL secondary-stacks" & LF);
   end if;

   --  Ends the program: Storage_Error nobody handles.
   debugPrint ("secondary-stack-check: a result past the ceiling, unhandled" & LF);
   declare
      Huge : Byte_Array renames Pattern (Past_Ceiling_Seed, Natural'Last);
   begin
      debugPrint ("secondary-stack-check: the ceiling never refused FAIL" & Natural'Image (Huge'Length) & LF);
   end;
end Main;
