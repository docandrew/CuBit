------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel's read-only publications besides the clock (KERN-002,
--  docs/syscall-retirement.md): system facts, the service registry and the
--  wall clock. They replace the SYSCALL_INFO key-value board.
--
--  @description
--  Each is one page the kernel maps read-only and non-executable into every
--  process, beside the clock publication (shared/time) in the same page
--  table, so they cost no extra page-table frames. The kernel is the only
--  writer. Every value is one aligned 64-bit word, written with one store,
--  so a reader never sees half of one; no word's meaning depends on another
--  word, so no seqlock is needed.
--
--  - System facts: written before the first process exists, except
--    Boot_Display_Native, which devmgr publishes once (SYSCALL_PUBLISH).
--  - Service registry: the process identity (KERN-003) registered for each
--    service role (SYSCALL_REGISTER_DRIVER), or 0 for none; cleared when the
--    process retires.
--  - Wall clock: UTC milliseconds since the Unix epoch at monotonic time
--    zero, or 0 while unknown. The registered clock service publishes it
--    (SYSCALL_PUBLISH); UTC now = this + monotonic milliseconds.
--
--  These were public through SYSCALL_INFO before; nothing here is new
--  knowledge for any process. Free memory is a live word here too (as it
--  was through SYSCALL_READ_STATISTICS); a process's own statistics are on
--  its readiness page (Readiness_Publication).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;

package System_Publications with Pure, SPARK_Mode is

   Page_Bytes : constant := Clock_Publication.Page_Bytes;

   Facts_Address      : constant := Clock_Publication.Page_Address + 1 * Page_Bytes;
   Registry_Address   : constant := Clock_Publication.Page_Address + 2 * Page_Bytes;
   Wall_Clock_Address : constant := Clock_Publication.Page_Address + 3 * Page_Bytes;
   Page_Count : constant := 3;

   --  Version word of every page: the layouts below, or not yet written.
   Layout_Version : constant := 2;
   Unpublished    : constant := 0;

   ---------------------------------------------------------------------------
   --  System facts
   ---------------------------------------------------------------------------

   --  The boot framebuffer (firmware mode); Width 0: none.
   type Framebuffer_Geometry is record
      Width          : Unsigned_64 := 0;
      Height         : Unsigned_64 := 0;
      Pitch          : Unsigned_64 := 0;   --  bytes per row
      Bits_Per_Pixel : Unsigned_64 := 0;
   end record;

   --  The platform monotonic clock's startup (kernel Platform_Monotonic):
   --  its status, the HPET's ID and period, and the timer probe's counter
   --  offset and samples. Diagnostic only.
   type Clock_Diagnostic is record
      Status          : Unsigned_64 := 0;
      HPET_ID         : Unsigned_64 := 0;
      Period_Fs       : Unsigned_64 := 0;
      Timer_Offset    : Unsigned_64 := 0;
      Timer_Before    : Unsigned_64 := 0;
      Timer_After     : Unsigned_64 := 0;
   end record;

   type Facts_Page is record
      Version             : Unsigned_64 := Unpublished;
      Processor_Count     : Unsigned_64 := 0;
      Memory_Bytes        : Unsigned_64 := 0;   --  RAM the kernel manages
      Framebuffer         : Framebuffer_Geometry;
      --  1 once devmgr gave the boot display adapter to its native driver
      --  (the kernel's firmware renderer is retired then); else 0.
      Boot_Display_Native : Unsigned_64 := 0;
      Clock               : Clock_Diagnostic;
      --  Device-memory requests the kernel refused so far (CAP-001). A
      --  count only: what was refused and why is in the log service's
      --  Device_Memory_Publication page. The Desktop bootstrap screen shows
      --  it and waits for that many records to reach the log.
      Device_Memory_Denials : Unsigned_64 := 0;
      --  User processes the kernel killed for a fault so far (count only;
      --  the records are in Device_Memory_Publication, for logstore).
      Process_Kills : Unsigned_64 := 0;
      --  Times the kernel's screen diagnostics that outlive the boot
      --  framebuffer's handover (Boot_Diagnostics.Stuck_Calls) erased a
      --  region of it. Its owner (display.svc) copies a whole frame on the
      --  next present after this changes; the kernel cannot repaint it.
      Scanout_Overwrites : Unsigned_64 := 0;
      --  The futex pause (IPC-010): the most TSC cycles a thread may UMWAIT
      --  on its futex word before the kernel sleep (the kernel's
      --  IA32_UMWAIT_CONTROL maximum on every CPU); 0: no pause.
      User_Wait_Cycles : Unsigned_64 := 0;
      --  Free physical memory in the whole system, in bytes: a snapshot a
      --  CPU crossing a scheduling boundary refreshes at most every
      --  CPU_Times_Publication.Memory_Refresh_Microseconds of its own,
      --  with the CPU-times page's copy.
      Free_Memory_Bytes : Unsigned_64 := 0;
   end record;
   for Facts_Page use record
      Version             at 0 range 0 .. 63;
      Processor_Count     at 8 range 0 .. 63;
      Memory_Bytes        at 16 range 0 .. 63;
      Framebuffer         at 24 range 0 .. 255;
      Boot_Display_Native at 56 range 0 .. 63;
      Clock               at 64 range 0 .. 383;
      Device_Memory_Denials at 112 range 0 .. 63;
      Process_Kills         at 120 range 0 .. 63;
      Scanout_Overwrites    at 128 range 0 .. 63;
      User_Wait_Cycles      at 136 range 0 .. 63;
      Free_Memory_Bytes     at 144 range 0 .. 63;
   end record;
   for Framebuffer_Geometry use record
      Width          at 0 range 0 .. 63;
      Height         at 8 range 0 .. 63;
      Pitch          at 16 range 0 .. 63;
      Bits_Per_Pixel at 24 range 0 .. 63;
   end record;
   for Clock_Diagnostic use record
      Status       at 0 range 0 .. 63;
      HPET_ID      at 8 range 0 .. 63;
      Period_Fs    at 16 range 0 .. 63;
      Timer_Offset at 24 range 0 .. 63;
      Timer_Before at 32 range 0 .. 63;
      Timer_After  at 40 range 0 .. 63;
   end record;
   pragma Compile_Time_Error (Facts_Page'Size / 8 > Page_Bytes,
                              "system facts exceed one page");

   ---------------------------------------------------------------------------
   --  Service registry
   ---------------------------------------------------------------------------

   --  Service roles (the kernel's driver registry; userspace names them
   --  DRIVER_* in CuBit.Messages and by name in CuBit.Service_Names).
   Maximum_Role : constant := 127;
   subtype Service_Role is Unsigned_64 range 0 .. Maximum_Role;
   --  Named registry ID for devmgr's read-only PCI-window broker.
   ACPI_PCI_Windows_Role : constant Service_Role := 40;
   type Identity_Table is array (Service_Role) of Unsigned_64;

   --  A role with no registered process.
   No_Identity : constant Unsigned_64 := 0;

   type Registry_Page is record
      Version    : Unsigned_64 := Unpublished;
      Identities : Identity_Table := [others => No_Identity];
   end record;
   for Registry_Page use record
      Version    at 0 range 0 .. 63;
      Identities at 8 range 0 .. 64 * (Maximum_Role + 1) - 1;
   end record;
   pragma Compile_Time_Error (Registry_Page'Size / 8 > Page_Bytes,
                              "the service registry exceeds one page");

   ---------------------------------------------------------------------------
   --  Wall clock
   ---------------------------------------------------------------------------

   type Wall_Clock_Page is record
      Version     : Unsigned_64 := Unpublished;
      --  UTC milliseconds since the Unix epoch at monotonic time zero; 0:
      --  unknown (no clock service, or its time is not current).
      Offset_Ms   : Unsigned_64 := 0;
   end record;
   for Wall_Clock_Page use record
      Version   at 0 range 0 .. 63;
      Offset_Ms at 8 range 0 .. 63;
   end record;

   ---------------------------------------------------------------------------
   --  SYSCALL_PUBLISH: the values a service publishes through the kernel.
   --  arg0 the value, arg1 the word. Each has one authorized publisher.
   ---------------------------------------------------------------------------
   type Published_Value is (Wall_Clock_Offset, Boot_Display_Native);
   for Published_Value use (Wall_Clock_Offset => 1, Boot_Display_Native => 2);

   ---------------------------------------------------------------------------
   --  Publications a launcher grants (KERN-002): SYSCALL_INSTALL_LAUNCH_
   --  ARGUMENTS arg4 is a set of them, one bit each (Bit), and the kernel
   --  maps each page read-only into the child before it first runs. Only
   --  what the child's manifest requests, (request-publication NAME), is
   --  ever granted (procmgr). A set with other bits is refused.
   ---------------------------------------------------------------------------
   --  CPU_Times: CPU_Times_Publication (one page at +5).
   --  Boot_Timeline: the kernel's boot timeline Image (Boot_Timeline_Layout,
   --  docs/boot-timeline.md), Boot_Timeline_Pages pages from
   --  Boot_Timeline_Address (+9; the thread-count pages are +7, +8).
   type Granted_Publication is (CPU_Times, Boot_Timeline);
   Boot_Timeline_Address : constant := Clock_Publication.Page_Address + 9 * Page_Bytes;
   Boot_Timeline_Pages   : constant := 3;

   function Bit (P : Granted_Publication) return Unsigned_64 is
     (Shift_Left (1, Granted_Publication'Pos (P)));
   No_Publications  : constant := 0;
   All_Publications : constant :=
     2 ** (Granted_Publication'Pos (Granted_Publication'Last) + 1) - 1;

end System_Publications;
