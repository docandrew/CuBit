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
with Wait_ABI;

package CuBit.Kernel_ABI with Pure, SPARK_Mode is

   subtype System_Call is Unsigned_64;

   Exit_Process                 : constant System_Call := 0;
   Get_Process_Id               : constant System_Call := 6;
   Grow_Heap                    : constant System_Call := 8;
   Write                        : constant System_Call := 12;
   Reply                        : constant System_Call := 18;
   --  SEND_EVENT: also the channel wake (Channel_Wake_ABI).
   Send_Event                   : constant System_Call := 19;
   Get_Time                     : constant System_Call := 27;
   --  With a deadline (arg 2; Forever, spelled out, for none).
   Call_Via_Endpoint_Capability : constant System_Call := 128;
   Submit_Via_Endpoint_Capability : constant System_Call := 42;
   Create_Shared_Memory_Grant_For_Process_Id : constant System_Call := 102;
   Revoke_Shared_Memory_Grant   : constant System_Call := 103;
   Revoke_Shared_Memory_Grant_Reference : constant System_Call := 110;
   Create_Shared_Memory_Grant_Via_Capability : constant System_Call := 106;
   Get_Owned_Shared_Memory_Grant_Generation  : constant System_Call := 108;
   Acquire_Shared_Memory_Grant  : constant System_Call := 109;
   --  The same, the owner named by an endpoint capability (slot, grant
   --  slot, generation, offset, length, access).
   Acquire_Shared_Memory_Grant_Via_Capability : constant System_Call := 112;
   --  Entry, stack pointer, argument (RDI), FS base, exit word.
   Thread_Create                : constant System_Call := 90;
   Thread_Exit                  : constant System_Call := 91;
   Futex_Wake                   : constant System_Call := 93;
   Read_Monotonic_Microseconds  : constant System_Call := 114;
   --  The one wait (ABI-002 slice 2): its sources, deadline and results
   --  are Wait_ABI's. It replaced RECEIVE, the polls, WAIT_COMPLETION,
   --  the activity wait, SLEEP, SLEEP_UNTIL, FUTEX_WAIT and
   --  ACCEPT_OFFERED_CAPABILITY.
   Wait                         : constant System_Call := Wait_ABI.System_Call_Number;

   Yield                        : constant System_Call := 118;
   --  Owned memory, one region API (kernel Process.Owned_Memory, ABI-002;
   --  CuBit.Owned_Reservations wraps it). Reserve_Owned_Region (bytes,
   --  alignment): page-aligned capacity, no RAM; the base is a multiple of
   --  the alignment (0: a page; else a power of two up to 1 GiB), or 0.
   --  Change_Owned_Range (address, bytes, Owned_Range_Operation): one
   --  operation on a page range of one region; 0, or Failed.
   --  Release_Owned_Region (base, bytes): the exact region; 0, or Failed.
   Reserve_Owned_Region         : constant System_Call := 146;
   Change_Owned_Range           : constant System_Call := 147;
   Release_Owned_Region         : constant System_Call := 148;
   --  A control message to a child, or to a process this one holds the
   --  process capability for (CuBit.Control_Events).
   Send_Control                 : constant System_Call := 127;
   --  This process's statistics and free memory (System_Publications).
   Read_Statistics              : constant System_Call := 134;
   --  A value published through the kernel (System_Publications).
   Publish                      : constant System_Call := 135;
   --  The most one commit may back (kernel Owned_Reservation_Policy).
   Maximum_Owned_Commit_Bytes : constant := 16 * 1024 * 1024;
   --  CHANGE_OWNED_RANGE's operation; the wire value is the position
   --  (kernel Owned_Reservation_Policy.Range_Operation). Commit: fresh zero
   --  pages, read/write, over a range with no committed page. Decommit:
   --  the frames go back, the range stays reserved and faults until it is
   --  committed again (then zero). The others set the access of committed
   --  pages; owned memory is never executable.
   type Owned_Range_Operation is
     (Commit, Decommit, Make_Inaccessible, Make_Read_Only, Make_Read_Write);

   --  What a call returns when it fails or has nothing to give.
   Failed : constant Unsigned_64 := Unsigned_64'Last;
   --  Not accepted now, but may be later; the caller keeps its request
   --  (SEND_CONTROL when the target's sender slots are full).
   Busy : constant Unsigned_64 := Unsigned_64'Last - 1;
   --  WAIT for an event: one was copied out (Wait_ABI.Taken).
   Event_Received : constant Unsigned_64 := Wait_ABI.Taken;
   --  The deadline that never comes.
   Forever : constant Unsigned_64 := Unsigned_64'Last;

   --  WAIT on a futex word: its results (Wait_ABI, kernel Process.Futex).
   Futex_Woken     : constant Unsigned_64 := Wait_ABI.Futex_Woken;
   Futex_Retry     : constant Unsigned_64 := Wait_ABI.Futex_Retry;
   Futex_Timed_Out : constant Unsigned_64 := Wait_ABI.Futex_Timed_Out;

   --  The largest owned-memory region (kernel Owned_Reservation_Policy).
   Maximum_Owned_Region_Bytes : constant := 2 * 1024 * 1024 * 1024;

   --  The service role netstack registers (CuBit.Messages.DRIVER_NETSTACK).
   Driver_Netstack   : constant := 3;

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
   Grant_Read_Write : constant := 1;
   --  Added to the above: the recipient (procmgr) may derive grants from it.
   Grant_Forwardable : constant := 2;
   Grant_Forwardable_Read_Write : constant := Grant_Forwardable + Grant_Read_Write;
   --  Added to the above: post grant lifecycle events for it
   --  (CuBit.Control_Events; docs/data-plane.md).
   Grant_Notify : constant := 4;
   Generation_Shift : constant := 32;   --  wire form: generation << 32 | slot

   Page_Bytes : constant := 4_096;

   --  SYSCALL_WRITE's console device.
   Console : constant := 1;

end CuBit.Kernel_ABI;
