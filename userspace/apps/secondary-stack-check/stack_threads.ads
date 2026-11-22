------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  secondary-stack-check's extra threads: each its own primary stack (owned
--  memory, guard page at the bottom) and thread area (CuBit.Thread_Areas),
--  running Stack_Work.Full_Run at the same time as the main thread.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.System_Facts;
with Stack_Work;

package Stack_Threads is

   Workers : constant := 4;
   subtype Worker_Index is Positive range 1 .. Workers;

   --  Each worker's primary stack: room for Stack_Work's depth.
   Stack_Bytes : constant := 24 * Stack_Work.MiB;

   package SP renames CuBit.System_Facts.SP;

   type Started is record
      Created : Boolean := False;
      --  The primary stack's base (owned memory).
      Stack : Unsigned_64 := 0;
      --  Its thread-count slot, from THREAD_CREATE's result.
      Slot : SP.Thread_Slot := SP.Main_Thread_Slot;
   end record;

   --  Allocate Index's stack and thread area, and start it.
   function Start (Index : Worker_Index) return Started;

   --  Wait for Index to end, then give back its primary stack.
   procedure Finish (Index : Worker_Index; Thread : Started);

   --  What each worker's run found, written before it ends.
   Results : array (Worker_Index) of Stack_Work.Outcome;

   --  What each worker's thread count showed (Readiness_Publication):
   --  the slot its entry received (RSI), its count before its first system
   --  call (0: a slot's line starts from zero for each thread), and how
   --  many one YIELD added to its own count (1: the read is not a call).
   type Count_Seen is record
      Entry_Slot  : SP.Thread_Slot := SP.Main_Thread_Slot;
      First_Count : Unsigned_64 := Unsigned_64'Last;
      Yield_Added : Unsigned_64 := 0;
   end record;
   Counts : array (Worker_Index) of Count_Seen;

end Stack_Threads;
