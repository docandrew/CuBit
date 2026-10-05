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
--  A child is named by PID and generation: the kernel reuses a PID as soon
--  as its process retires, and a launch procmgr abandons still reports an
--  exit, so the PID alone is ambiguous. Exit events for anything else are
--  ignored. Proved (tests/libc-ada): every operation stays within the
--  tables, a child is ended only by its own (PID, generation) event, and a
--  wait takes only an ended child that matches.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Child_Exits;

package CuBit.Child_Table with Pure, SPARK_Mode is

   use type Interfaces.C.int;

   PID_Limit : constant := 256;           --  kernel PIDs are 1 .. 255
   subtype Process_Number is Unsigned_64 range 1 .. PID_Limit - 1;
   Capacity : constant := PID_Limit;
   subtype Count is Natural range 0 .. Capacity;
   subtype Slot is Positive range 1 .. Capacity;

   --  A wait status: exit code << 8, or SIGKILL for a stopped process.
   subtype Wait_Status is Interfaces.C.int range 0 .. 16#FF00#;
   Status_Code_Shift : constant := 8;
   Killed_Status : constant Wait_Status := 9;   --  SIGKILL

   type Child is record
      Process    : Process_Number := Process_Number'First;
      Generation : Unsigned_64 := 0;
   end record;
   type Ended_Child is record
      Process : Process_Number := Process_Number'First;
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

   function Is_Live (T : Table; Process : Process_Number;
                     Generation : Unsigned_64) return Boolean is
     (for some K in 1 .. T.Live_Count =>
        T.Live (K).Process = Process
        and then T.Live (K).Generation = Generation);

   --  A child started. Dropped if the table is full (it can hold every
   --  PID, so only a kernel fault could fill it).
   procedure Started (T : in out Table; Process : Process_Number;
                      Generation : Unsigned_64)
   with Post => T.Ended_Count = T.Ended_Count'Old
                and then (if T.Live_Count'Old < Capacity
                          then Is_Live (T, Process, Generation));

   --  An exit report: a live child it names ends (its status queued);
   --  anything else changes nothing.
   procedure Exited (T : in out Table; Report : CuBit.Child_Exits.Report)
   with Post => T.Live_Count <= T.Live_Count'Old
                and then T.Ended_Count >= T.Ended_Count'Old
                and then (if Report.Process not in Process_Number
                          then T = T'Old);

   --  Wanted: a PID, or zero or below for any child (CuBit has no process
   --  groups). Found is the oldest matching ended child's PID, with its
   --  status, removed; 0 when none.
   procedure Take (T : in out Table; Wanted : Interfaces.C.int;
                   Found : out Interfaces.C.int; Status : out Wait_Status)
   with Post => T.Live_Count = T.Live_Count'Old
                and then (if Found = 0 then T = T'Old
                          else Found in 1 .. PID_Limit - 1
                               and then T.Ended_Count = T.Ended_Count'Old - 1
                               and then (Wanted <= 0 or else Found = Wanted));

   --  Whether a live child matches Wanted (any, for zero or below).
   function Has_Child (T : Table; Wanted : Interfaces.C.int) return Boolean is
     (if Wanted <= 0 then T.Live_Count > 0
      else (for some K in 1 .. T.Live_Count =>
              T.Live (K).Process = Unsigned_64 (Wanted)));

end CuBit.Child_Table;
