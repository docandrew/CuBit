------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  secondary-stack-check's extra threads: each its own primary stack (owned
--  memory, guard page at the bottom) and thread area (CuBit.Thread_Areas),
--  running Stack_Work at the same time as the main thread.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Stack_Work;

package Stack_Threads is

   Workers : constant := 2;
   subtype Worker_Index is Positive range 1 .. Workers;

   --  Each worker's primary stack: room for Stack_Work's depth.
   Stack_Bytes : constant := 24 * Stack_Work.MiB;

   type Started is record
      Created : Boolean := False;
      --  The primary stack's base (owned memory).
      Stack : Unsigned_64 := 0;
   end record;

   --  Allocate Index's stack and thread area, and start it.
   function Start (Index : Worker_Index) return Started;

   --  Wait for Index to end, then give back its primary stack.
   procedure Finish (Index : Worker_Index; Thread : Started);

   --  What each worker's run found, written before it ends.
   Results : array (Worker_Index) of Stack_Work.Outcome;

end Stack_Threads;
