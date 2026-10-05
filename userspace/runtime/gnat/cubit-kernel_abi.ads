------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel's system-call numbers and IPC message layout, as code without
--  an Ada run-time library (the libc's Ada, docs/c-removal.md) uses them.
--
--  @description
--  CuBit.Messages has these too but needs the user runtime. A hosted test
--  (tests/libc-ada) checks every system call here against the kernel's own
--  enumeration (kernel/src/syscall.ads). Grows as the libc's C is replaced.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Kernel_ABI with Pure, SPARK_Mode is

   subtype System_Call is Unsigned_64;

   Exit_Process                 : constant System_Call := 0;
   Get_Process_Id               : constant System_Call := 6;
   Grow_Heap                    : constant System_Call := 8;
   Write                        : constant System_Call := 12;
   Info                         : constant System_Call := 15;
   Receive                      : constant System_Call := 17;
   Reply                        : constant System_Call := 18;
   Poll_Any_IPC                 : constant System_Call := 22;
   Wait_Completion              : constant System_Call := 24;
   Receive_Event_Nonblocking    : constant System_Call := 26;
   Get_Time                     : constant System_Call := 27;
   Call_Via_Endpoint_Capability : constant System_Call := 41;
   Submit_Via_Endpoint_Capability : constant System_Call := 42;
   Create_Shared_Memory_Grant_For_Process_Id : constant System_Call := 102;
   Revoke_Shared_Memory_Grant   : constant System_Call := 103;
   Create_Shared_Memory_Grant_Via_Capability : constant System_Call := 106;
   Get_Owned_Shared_Memory_Grant_Generation  : constant System_Call := 108;
   Acquire_Shared_Memory_Grant  : constant System_Call := 109;
   Wait_For_IPC_Or_Completion_Until_Monotonic_Millisecond :
     constant System_Call := 113;
   Thread_Exit                  : constant System_Call := 91;
   Futex_Wait                   : constant System_Call := 92;
   Futex_Wake                   : constant System_Call := 93;
   Inspect_Capability           : constant System_Call := 84;
   Read_Monotonic_Microseconds  : constant System_Call := 114;
   Allocate_Owned_Memory        : constant System_Call := 115;
   Release_Owned_Memory         : constant System_Call := 116;
   Protect_Owned_Memory         : constant System_Call := 117;
   Yield                        : constant System_Call := 118;
   Sleep_Until_Monotonic_Microsecond : constant System_Call := 119;

   --  What a call returns when it fails or has nothing to give.
   Failed : constant Unsigned_64 := Unsigned_64'Last;
   --  SYSCALL_RECEIVE_EVENT_NB: an event was copied out.
   Event_Received : constant Unsigned_64 := 1;
   --  The deadline that never comes.
   Forever : constant Unsigned_64 := Unsigned_64'Last;

   --  FUTEX_WAIT results (kernel/src/process-futex.ads).
   Futex_Woken     : constant Unsigned_64 := 0;
   Futex_Retry     : constant Unsigned_64 := 1;
   Futex_Timed_Out : constant Unsigned_64 := 2;

   --  The largest owned-memory region (kernel Process.Owned_Memory).
   Maximum_Owned_Bytes : constant := 256 * 1024 * 1024;

   --  SYSINFO_WALL_CLOCK_OFFSET: UTC milliseconds at monotonic zero, set by
   --  the clock service while its time is current (0 until then).
   Wall_Clock_Offset : constant := 1403;
   --  SYSINFO_REGISTERED_DRIVER (key + driver): the PID registered as a
   --  driver; DRIVER_NETSTACK names netstack (CuBit.Messages).
   Registered_Driver : constant := 2000;
   Driver_Netstack   : constant := 3;
   --  SYSCALL_INSPECT_CAPABILITY: six words; an endpoint's kind is 1.
   Capability_Words    : constant := 6;
   Capability_Endpoint : constant := 1;
   Capability_Slots    : constant := 64;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

   --  A message (kernel Process.Message, 48 bytes).
   type Message_Words is array (0 .. 3) of Unsigned_64;
   type Message is record
      Label     : Unsigned_32 := 0;
      Length    : Unsigned_8 := 0;
      Flags     : Unsigned_8 := 0;
      Reserved  : Unsigned_16 := 0;
      Authority : Unsigned_64 := 0;   --  kernel-stamped
      Words     : Message_Words := [others => 0];
   end record;
   for Message use record
      Label     at 0 range 0 .. 31;
      Length    at 4 range 0 .. 7;
      Flags     at 5 range 0 .. 7;
      Reserved  at 6 range 0 .. 15;
      Authority at 8 range 0 .. 63;
      Words     at 16 range 0 .. 255;
   end record;
   Message_Bytes : constant := 48;
   pragma Compile_Time_Error (Message'Size /= Message_Bytes * 8,
                              "the IPC message ABI is 48 bytes");

   --  Shared-memory grants (SYSCALL_CREATE_SHARED_MEMORY_GRANT_*).
   Grant_Read_Only : constant := 0;
   Generation_Shift : constant := 32;   --  wire form: generation << 32 | slot

   Page_Bytes : constant := 4_096;

   --  SYSCALL_WRITE's console device.
   Console : constant := 1;

   --  Fixed capability slots (userspace/ccl/catalogs/native-runtime-services.ccl).
   Process_Manager_Slot : constant := 12;

end CuBit.Kernel_ABI;
