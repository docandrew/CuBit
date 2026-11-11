------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Thread_Areas;

package body Stack_Threads is

   package ABI renames CuBit.Kernel_ABI;

   --  Cleared and futex-woken by the kernel when the thread ends.
   type Word is mod 2 ** 32 with Atomic, Size => 32;
   Exit_Words : array (Worker_Index) of aliased Word := [others => 1];

   --  PROTECT_OWNED_MEMORY modes (kernel Process.Owned_Memory.Protect).
   Inaccessible : constant := 0;
   --  The first seed of the workers' runs (the main thread's are below).
   First_Seed : constant := 10;
   --  A SysV function expects RSP + 8 to be 16-byte aligned at entry.
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;

   procedure Worker (Argument : Unsigned_64) with Convention => C, No_Return;
   procedure Worker (Argument : Unsigned_64) is
      Index : constant Worker_Index := Worker_Index (Argument);
   begin
      Results (Index) := Stack_Work.Full_Run (First_Seed + Argument);
      CuBit.Thread_Areas.Exit_Thread;
   end Worker;

   function Start (Index : Worker_Index) return Started is
      Thread : Started;
      Area : Unsigned_64;
   begin
      --  Armed again for each batch: the kernel clears it when the thread ends.
      Exit_Words (Index) := 1;
      Thread.Stack := CuBit.Kernel_Calls.Call (ABI.Allocate_Owned_Memory, Stack_Bytes);
      if Thread.Stack = 0 then
         return Thread;
      end if;
      --  A guard page at the bottom: an overflow faults, never reaches a
      --  neighbour.
      if CuBit.Kernel_Calls.Call
           (ABI.Protect_Owned_Memory, Thread.Stack, ABI.Page_Bytes, Inaccessible) /= 0
      then
         return Thread;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         return Thread;
      end if;
      if CuBit.Kernel_Calls.Call
           (ABI.Thread_Create, Unsigned_64 (To_Integer (Worker'Address)),
            ((Thread.Stack + Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
            Unsigned_64 (Index), Area,
            Unsigned_64 (To_Integer (Exit_Words (Index)'Address))) = ABI.Failed
      then
         CuBit.Thread_Areas.Discard (Area);
         return Thread;
      end if;
      Thread.Created := True;
      return Thread;
   end Start;

   procedure Finish (Index : Worker_Index; Thread : Started) is
      Seen : Word;
      Ignore : Unsigned_64;
   begin
      if Thread.Created then
         loop
            Seen := Exit_Words (Index);
            exit when Seen = 0;
            Ignore := CuBit.Kernel_Calls.Call
              (ABI.Futex_Wait, Unsigned_64 (To_Integer (Exit_Words (Index)'Address)),
               Unsigned_64 (Seen), ABI.Forever);
         end loop;
      end if;
      if Thread.Stack /= 0 then
         Ignore := CuBit.Kernel_Calls.Call (ABI.Release_Owned_Memory, Thread.Stack, Stack_Bytes);
      end if;
   end Finish;

end Stack_Threads;
