------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The boot timeline's record (BOOT-001, docs/boot-timeline.md): when each
--  boot stage, process and startup wait happened, in TSC ticks.
--
--  @description
--  Shared by the kernel (the only writer, kernel/src/boot_timeline*.ad?)
--  and its readers (procmgr copies it to process observers; the
--  boot-timeline app and hosted tests decode it). One fixed-size Image,
--  little-endian, laid out by the representation clauses below.
--
--  Every time is a raw TSC value. The TSC starts near zero at reset (and at
--  KVM vCPU creation), so Ticks / Frequency is time since power-on and the
--  kernel's entry time includes firmware and the boot loader. Zero means
--  "not recorded". Frequency is the kernel's admitted TSC rate (the clock
--  publication's, docs/fast-clock.md); zero means unknown.
--
--  The kernel records from its entry until Window_End (Recording_Window
--  after the scheduling clock starts), then stops: recording costs one
--  atomic load per system call after that.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Boot_Timeline_Layout with
   Pure, SPARK_Mode
is
   Layout_Version : constant := 1;

   ---------------------------------------------------------------------------
   --  The BOOT_TIMELINE system call
   ---------------------------------------------------------------------------
   System_Call_Number : constant := 133;

   --  arg0 selects the operation.
   --  Read: arg1 user buffer, arg2 its length, arg3 byte offset into the
   --        Image. Copies at most Read_Chunk_Bytes; returns the bytes
   --        copied (0 at or past the end), Unsigned_64'Last if denied.
   --        Needs CAP_PROCESS with READ (procmgr, devmgr).
   --  Mark: arg1 a Milestone code. Needs the CAP_NOTIFICATION (WRITE) that
   --        registers the milestone's owning driver role (the kernel's
   --        table in boot_timeline.adb). The first mark wins; returns 0,
   --        or Unsigned_64'Last if denied or not a milestone.
   --  arg4: the handle of the capability the operation relies on (CAP-002).
   type Operation is (Read, Mark);
   for Operation use (Read => 0, Mark => 1);

   --  Bytes one Read copies at most (the kernel bounces them on its stack).
   Read_Chunk_Bytes : constant := 256;

   ---------------------------------------------------------------------------
   --  Kernel stages, in boot order (kernel/src/kmain.adb, modules.adb)
   ---------------------------------------------------------------------------
   type Kernel_Stage is
     (Kernel_Entry,             --  first statement of kmain
      Memory_Ready,             --  page tables, allocators, process tables
      Framebuffer_Ready,        --  boot diagnostics panel set up
      Firmware_Tables_Ready,    --  ACPI tables read
      Firmware_Timers_Owned,    --  firmware timers quiesced
      Clock_Calibrated,         --  TSC rate from CPUID 15h or the PIT
      Scheduling_Clock_Started, --  LAPIC tick and the clock publication
      Interrupt_Routing_Ready,  --  I/O APIC programmed
      Initrd_Parsed,            --  CPIO archive indexed (Modules.setup)
      Device_Manager_Loaded,    --  devmgr.svc loaded and resumed
      Secondary_CPUs_Ready,
      Scheduler_Started);       --  BSP enters the scheduler

   Stage_Capacity : constant := 16;
   type Stage_Slot is range 0 .. Stage_Capacity - 1;
   function Slot_Of (Stage : Kernel_Stage) return Stage_Slot is
     (Stage_Slot (Kernel_Stage'Pos (Stage)));

   ---------------------------------------------------------------------------
   --  User-space milestones. Codes are the wire values of Mark; the
   --  userspace copy in userspace/lib/display/cubit-boot_milestones.ads
   --  must agree (tests/boot-timeline checks it).
   ---------------------------------------------------------------------------
   type Milestone is
     (Startup_Profile_Started,   --  procmgr begins init.ccl
      Startup_Profile_Launched,  --  procmgr launched every init.ccl entry
      Display_Leased,            --  display.svc grants output zero's lease
      First_Frame_Presented,     --  Desktop's first frame published
      First_Input_Accepted);     --  Desktop accepted its first input event
   for Milestone use
     (Startup_Profile_Started  => 0,
      Startup_Profile_Launched => 1,
      Display_Leased           => 2,
      First_Frame_Presented    => 3,
      First_Input_Accepted     => 4);

   Milestone_Capacity : constant := 8;
   type Milestone_Slot is range 0 .. Milestone_Capacity - 1;
   function Slot_Of (Item : Milestone) return Milestone_Slot is
     (Milestone_Slot (Milestone'Enum_Rep (Item)));

   ---------------------------------------------------------------------------
   --  Records
   ---------------------------------------------------------------------------
   subtype Ticks is Unsigned_64;
   Not_Recorded : constant Ticks := 0;

   --  A process identity word (KERN-003); zero is the kernel or nobody.
   subtype Identity_Word is Unsigned_64;
   No_Identity : constant Identity_Word := 0;

   Name_Bytes : constant := 16;
   subtype Name_Text is String (1 .. Name_Bytes);

   --  Saturating event counts.
   type Event_Count is range 0 .. 2**31 - 1 with Size => 32;

   Process_Capacity : constant := 48;
   type Process_Count is range 0 .. Process_Capacity with Size => 32;
   subtype Process_Number is Process_Count range 1 .. Process_Capacity;

   Wait_Capacity : constant := 96;
   type Wait_Count is range 0 .. Wait_Capacity with Size => 32;
   subtype Wait_Number is Wait_Count range 1 .. Wait_Capacity;

   --  Which blocking system call a wait was.
   type Wait_Kind is
     (No_Wait,
      Call,         --  synchronous capability call; Target is the callee
      Sleep,        --  SLEEP or SLEEP_UNTIL
      Receive,      --  receive before the process was Ready
      Completion,   --  WAIT_COMPLETION before Ready
      Futex)        --  FUTEX_WAIT before Ready
   with Size => 8;
   for Wait_Kind use
     (No_Wait => 0, Call => 1, Sleep => 2, Receive => 3, Completion => 4,
      Futex => 5);

   --  One process, from its spawn. Ready is the first receive at or after
   --  its driver registration; a process that never registers has only
   --  First_Receive. The tick totals cover waits during the window.
   type Process_Entry is record
      Identity      : Identity_Word := No_Identity;
      Spawner       : Identity_Word := No_Identity;
      Name          : Name_Text := [others => ' '];
      Spawn_Started : Ticks := Not_Recorded;  --  SPAWN entered
      Loaded        : Ticks := Not_Recorded;  --  ELF loaded, suspended
      Resumed       : Ticks := Not_Recorded;  --  spawner resumed it
      First_Run     : Ticks := Not_Recorded;  --  first dispatch
      Registered    : Ticks := Not_Recorded;  --  REGISTER_DRIVER
      First_Receive : Ticks := Not_Recorded;
      Ready         : Ticks := Not_Recorded;
      Calls         : Event_Count := 0;       --  synchronous calls made
      Waits         : Event_Count := 0;       --  other blocking calls
      Call_Ticks    : Ticks := 0;             --  blocked in calls
      Sleep_Ticks   : Ticks := 0;             --  asleep
      Wait_Ticks    : Ticks := 0;             --  other waits before Ready
      Reserved      : Unsigned_64 := 0;
   end record;
   for Process_Entry use record
      Identity      at   0 range 0 .. 63;
      Spawner       at   8 range 0 .. 63;
      Name          at  16 range 0 .. 127;
      Spawn_Started at  32 range 0 .. 63;
      Loaded        at  40 range 0 .. 63;
      Resumed       at  48 range 0 .. 63;
      First_Run     at  56 range 0 .. 63;
      Registered    at  64 range 0 .. 63;
      First_Receive at  72 range 0 .. 63;
      Ready         at  80 range 0 .. 63;
      Calls         at  88 range 0 .. 31;
      Waits         at  92 range 0 .. 31;
      Call_Ticks    at  96 range 0 .. 63;
      Sleep_Ticks   at 104 range 0 .. 63;
      Wait_Ticks    at 112 range 0 .. 63;
      Reserved      at 120 range 0 .. 63;
   end record;
   Process_Entry_Bytes : constant := 128;
   for Process_Entry'Size use Process_Entry_Bytes * 8;

   --  Saturating repeat counts of one kind of wait.
   type Wait_Repeats is range 0 .. 2**16 - 1 with Size => 16;

   --  Blocking system calls that lasted at least Minimum_Wait, by one
   --  waiter, of one kind, to one target with one label: the first one's
   --  start, how many, and their total duration.
   type Wait_Entry is record
      Kind     : Wait_Kind := No_Wait;
      Pad_8    : Unsigned_8 := 0;
      Repeats  : Wait_Repeats := 0;
      Label    : Unsigned_32 := 0;              --  a call's message label
      Waiter   : Identity_Word := No_Identity;
      Target   : Identity_Word := No_Identity;  --  a call's callee
      Started  : Ticks := Not_Recorded;         --  the first one
      Duration : Ticks := 0;                    --  all of them
   end record;
   for Wait_Entry use record
      Kind     at  0 range 0 .. 7;
      Pad_8    at  1 range 0 .. 7;
      Repeats  at  2 range 0 .. 15;
      Label    at  4 range 0 .. 31;
      Waiter   at  8 range 0 .. 63;
      Target   at 16 range 0 .. 63;
      Started  at 24 range 0 .. 63;
      Duration at 32 range 0 .. 63;
   end record;
   Wait_Entry_Bytes : constant := 40;
   for Wait_Entry'Size use Wait_Entry_Bytes * 8;

   type Milestone_Entry is record
      At_Ticks : Ticks := Not_Recorded;
      Marker   : Identity_Word := No_Identity;
   end record;
   for Milestone_Entry use record
      At_Ticks at 0 range 0 .. 63;
      Marker   at 8 range 0 .. 63;
   end record;
   Milestone_Entry_Bytes : constant := 16;
   for Milestone_Entry'Size use Milestone_Entry_Bytes * 8;

   type Recording_State is (Closed, Open) with Size => 32;
   for Recording_State use (Closed => 0, Open => 1);

   type Header is record
      Version           : Unsigned_32 := Layout_Version;
      Recording         : Recording_State := Open;
      Frequency         : Unsigned_64 := 0;     --  TSC Hz; 0 unknown
      Processes         : Process_Count := 0;
      Waits             : Wait_Count := 0;
      Processes_Dropped : Event_Count := 0;     --  spawns past capacity
      Waits_Dropped     : Event_Count := 0;     --  new long waits past capacity
      Window_End        : Ticks := Not_Recorded;
      Closed_At         : Ticks := Not_Recorded;
      Minimum_Wait      : Ticks := 0;           --  shortest wait recorded
      Reserved          : Unsigned_64 := 0;
   end record;
   for Header use record
      Version           at  0 range 0 .. 31;
      Recording         at  4 range 0 .. 31;
      Frequency         at  8 range 0 .. 63;
      Processes         at 16 range 0 .. 31;
      Waits             at 20 range 0 .. 31;
      Processes_Dropped at 24 range 0 .. 31;
      Waits_Dropped     at 28 range 0 .. 31;
      Window_End        at 32 range 0 .. 63;
      Closed_At         at 40 range 0 .. 63;
      Minimum_Wait      at 48 range 0 .. 63;
      Reserved          at 56 range 0 .. 63;
   end record;
   Header_Bytes : constant := 64;
   for Header'Size use Header_Bytes * 8;

   type Stage_Table is array (Stage_Slot) of Ticks
     with Default_Component_Value => Not_Recorded;
   type Milestone_Table is array (Milestone_Slot) of Milestone_Entry;
   type Process_Table is array (Process_Number) of Process_Entry;
   type Wait_Table is array (Wait_Number) of Wait_Entry;

   Stage_Table_Bytes     : constant := Stage_Capacity * 8;
   Milestone_Table_Bytes : constant := Milestone_Capacity * Milestone_Entry_Bytes;
   Process_Table_Bytes   : constant := Process_Capacity * Process_Entry_Bytes;
   Wait_Table_Bytes      : constant := Wait_Capacity * Wait_Entry_Bytes;

   Stages_Offset     : constant := Header_Bytes;
   Milestones_Offset : constant := Stages_Offset + Stage_Table_Bytes;
   Processes_Offset  : constant := Milestones_Offset + Milestone_Table_Bytes;
   Waits_Offset      : constant := Processes_Offset + Process_Table_Bytes;
   Image_Bytes       : constant := Waits_Offset + Wait_Table_Bytes;

   type Image is record
      Head       : Header;
      Stages     : Stage_Table;
      Milestones : Milestone_Table;
      Processes  : Process_Table;
      Waits      : Wait_Table;
   end record;
   for Image use record
      Head       at 0 range 0 .. Header_Bytes * 8 - 1;
      Stages     at Stages_Offset range 0 .. Stage_Table_Bytes * 8 - 1;
      Milestones at Milestones_Offset range 0 .. Milestone_Table_Bytes * 8 - 1;
      Processes  at Processes_Offset range 0 .. Process_Table_Bytes * 8 - 1;
      Waits      at Waits_Offset range 0 .. Wait_Table_Bytes * 8 - 1;
   end record;
   for Image'Size use Image_Bytes * 8;

   subtype Image_Offset is Unsigned_64 range 0 .. Image_Bytes;
end Boot_Timeline_Layout;
