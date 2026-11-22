------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The children a program started and has not yet waited for
--  (docs/process-arguments.md): the libc's posix_spawn records each one,
--  exit events end them, and waitpid takes them, oldest first.
--
--  @description
--  A child is named by its identity (KERN-003, docs/process-objects.md):
--  one word for one life, never reused. POSIX callers see a pid_t, the
--  identity folded to 31 bits (POSIX_Of); the table maps it back, so the
--  libc acts only on children it knows. Exit events for anything else are
--  ignored. Proved (tests/libc-ada): every operation stays within the
--  tables, a child is ended only by its own identity's event, and a wait
--  takes only an ended child that matches.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Child_Exits;
with CuBit.Process_IDs; use CuBit.Process_IDs;

package CuBit.Child_Table with Pure, SPARK_Mode is

   use type Interfaces.C.int;

   --  Children tracked at once (a table size, not a process limit).
   Capacity : constant := 256;
   subtype Count is Natural range 0 .. Capacity;
   subtype Slot is Positive range 1 .. Capacity;

   --  A wait status: exit code << 8, or SIGKILL for a stopped process.
   subtype Wait_Status is Interfaces.C.int range 0 .. 16#FF00#;
   Status_Code_Shift : constant := 8;
   Killed_Status : constant Wait_Status := 9;   --  SIGKILL

   --  A child; always a process (Is_Process) in a live entry.
   subtype Child is Process_ID;
   type Ended_Child is record
      Process : Process_ID := No_Process;
      Status  : Wait_Status := 0;
   end record;
   type Children is array (Slot) of Child;
   type Ended_Children is array (Slot) of Ended_Child;

   type Table is record
      Live        : Children;
      Live_Count  : Count := 0;
      Ended       : Ended_Children;
      Ended_Count : Count := 0;
   end record;

   function Is_Live (T : Table; Process : Process_ID) return Boolean is
     (for some K in 1 .. T.Live_Count => T.Live (K) = Process);

   --  A child started. Dropped if the table is full.
   procedure Started (T : in out Table; Process : Process_ID)
   with Pre  => Is_Process (Process),
        Post => T.Ended_Count = T.Ended_Count'Old
                and then (if T.Live_Count'Old < Capacity
                          then Is_Live (T, Process));

   --  An exit report: a live child it names ends (its status queued);
   --  anything else changes nothing.
   procedure Exited (T : in out Table; Report : CuBit.Child_Exits.Report)
   with Post => T.Live_Count <= T.Live_Count'Old
                and then T.Ended_Count >= T.Ended_Count'Old
                and then (if not Is_Process (Report.Process)
                          then T = T'Old);

   --  Wanted: a pid_t (POSIX_Of), or zero or below for any child (CuBit
   --  has no process groups). Found is the oldest matching ended child's
   --  pid_t, with its status, removed; 0 when none.
   procedure Take (T : in out Table; Wanted : Interfaces.C.int;
                   Found : out Interfaces.C.int; Status : out Wait_Status)
   with Post => T.Live_Count = T.Live_Count'Old
                and then (if Found = 0 then T = T'Old
                          else Found in POSIX_PID
                               and then T.Ended_Count = T.Ended_Count'Old - 1
                               and then (Wanted <= 0 or else Found = Wanted));

   --  Whether a live child matches Wanted (any, for zero or below).
   function Has_Child (T : Table; Wanted : Interfaces.C.int) return Boolean is
     (if Wanted <= 0 then T.Live_Count > 0
      else (for some K in 1 .. T.Live_Count =>
              Is_Process (T.Live (K)) and then POSIX_Of (T.Live (K)) = Wanted));

end CuBit.Child_Table;
