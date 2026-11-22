-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's boot timeline (BOOT-001, docs/boot-timeline.md): when each
-- boot stage, process and long startup wait happened, in TSC ticks.
--
-- @description
-- Glue around the proved Boot_Timeline_Records: one leaf spinlock (no other
-- lock is ever taken while it is held), the TSC readings, and an atomic
-- mirror of "still recording" so each hook costs one load once the window
-- has closed. Always on; bounded and allocation-free.
--
-- Readers see the Image as a granted publication (System_Publications.
-- Boot_Timeline): a page-aligned kernel copy, rewritten whole after every
-- record while the window is open, mapped read-only into a program whose
-- manifest declares it (Map_Into, at launch).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Boot_Timeline_Layout;
with Boot_Timeline_Records;
with Virtmem;

package Boot_Timeline with
   SPARK_Mode => Off -- global state behind a spinlock; the logic is proved
is
   package Layout renames Boot_Timeline_Layout;
   subtype Process_Event is Boot_Timeline_Records.Process_Event;
   Resumed    : constant Process_Event := Boot_Timeline_Records.Resumed;
   First_Run  : constant Process_Event := Boot_Timeline_Records.First_Run;
   Registered : constant Process_Event := Boot_Timeline_Records.Registered;
   Received   : constant Process_Event := Boot_Timeline_Records.Received;

   --  BSP, before anything else records: Entry_Ticks is the TSC at kmain.
   procedure Initialize (Entry_Ticks : Unsigned_64);

   --  False once the window has closed; then no hook records anything.
   function Recording return Boolean with Inline;

   function Now return Unsigned_64 with Inline;

   procedure Mark_Stage (Stage : Layout.Kernel_Stage);

   --  The admitted TSC rate, when the scheduling clock starts.
   procedure Set_Frequency (Frequency : Unsigned_64);

   --  Slot is the kernel process slot (Process.ProcessID).
   procedure Note_Spawn (Slot         : Natural;
                         Identity     : Unsigned_64;
                         Spawner      : Unsigned_64;
                         Spawner_Slot : Natural;
                         Name         : String;
                         Started      : Unsigned_64);

   procedure Note_Event (Slot : Natural; Event : Process_Event);

   --  A blocking system call that began at Started and ends now.
   procedure Note_Wait (Slot    : Natural;
                        Waiter  : Unsigned_64;
                        Kind    : Layout.Wait_Kind;
                        Label   : Unsigned_32;
                        Target  : Unsigned_64;
                        Started : Unsigned_64);

   procedure Mark_Milestone (Item : Layout.Milestone; Marker : Unsigned_64);

   --  Map the published Image read-only into Root at
   --  System_Publications.Boot_Timeline_Address (already there: Success).
   procedure Map_Into (Root : in out Virtmem.P4; Success : out Boolean);
end Boot_Timeline;
