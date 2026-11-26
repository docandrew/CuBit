------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Wait_ABI;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Owned_Reservations;
with CuBit.Thread_Areas;
with Thread_ABI;

package body Stack_Threads is

   package ABI renames CuBit.Kernel_ABI;

   --  Cleared and futex-woken by the kernel when the thread ends.
   type Word is mod 2 ** 32 with Atomic, Size => 32;
   Exit_Words : array (Worker_Index) of aliased Word := [others => 1];

   --  The first seed of the workers' runs (the main thread's are below).
   First_Seed : constant := 10;
   --  A SysV function expects RSP + 8 to be 16-byte aligned at entry.
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;

   --  The kernel starts a thread with its argument in RDI and its
   --  thread-count slot in RSI.
   procedure Worker (Argument : Unsigned_64; Slot : SP.Thread_Slot)
   with Convention => C, No_Return;
   procedure Worker (Argument : Unsigned_64; Slot : SP.Thread_Slot) is
      Index : constant Worker_Index := Worker_Index (Argument);
      First : constant Unsigned_64 := CuBit.System_Facts.Thread_System_Calls (Slot);
      Before : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      CuBit.System_Facts.Enter_Thread (Slot);
      Before := CuBit.System_Facts.Thread_System_Calls;
      Ignore := CuBit.Kernel_Calls.Call (ABI.Yield);
      Counts (Index) := (Entry_Slot  => Slot,
                         First_Count => First,
                         Yield_Added => CuBit.System_Facts.Thread_System_Calls - Before);
      Results (Index) := Stack_Work.Full_Run (First_Seed + Argument);
      CuBit.Thread_Areas.Exit_Thread;
   end Worker;

   function Start (Index : Worker_Index) return Started is
      Thread : Started;
      Area : Unsigned_64;
      Result : Unsigned_64;
   begin
      --  Armed again for each batch: the kernel clears it when the thread ends.
      Counts (Index) := (others => <>);
      Exit_Words (Index) := 1;
      Thread.Stack := CuBit.Owned_Reservations.Reserve (Stack_Bytes);
      if Thread.Stack = 0 then
         return Thread;
      end if;
      --  A guard page at the bottom: it is never committed, so an overflow
      --  faults and never reaches a neighbour.
      if not CuBit.Owned_Reservations.Change
           (Thread.Stack + ABI.Page_Bytes, Stack_Bytes - ABI.Page_Bytes, CuBit.Owned_Reservations.Commit)
      then
         return Thread;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         return Thread;
      end if;
      Result := CuBit.Kernel_Calls.Call
        (ABI.Thread_Create, Unsigned_64 (To_Integer (Worker'Address)),
         ((Thread.Stack + Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
         Unsigned_64 (Index), Area,
         Unsigned_64 (To_Integer (Exit_Words (Index)'Address)));
      if Result = Thread_ABI.Refused or else not Thread_ABI.Is_Created (Result) then
         CuBit.Thread_Areas.Discard (Area);
         return Thread;
      end if;
      Thread.Slot := SP.Thread_Slot (Thread_ABI.Slot_Of (Result));
      Thread.Created := True;
      return Thread;
   end Start;

   procedure Finish (Index : Worker_Index; Thread : Started) is
      Seen : Word;
      Ignore : Unsigned_64;
      Ignore_Release : Boolean;
   begin
      if Thread.Created then
         loop
            Seen := Exit_Words (Index);
            exit when Seen = 0;
            Ignore := CuBit.Kernel_Calls.Call
              (ABI.Wait, Wait_ABI.Futex_Sources, ABI.Forever,
               Unsigned_64 (To_Integer (Exit_Words (Index)'Address)), Unsigned_64 (Seen));
         end loop;
      end if;
      if Thread.Stack /= 0 then
         Ignore_Release := CuBit.Owned_Reservations.Release (Thread.Stack, Stack_Bytes);
      end if;
   end Finish;

end Stack_Threads;
