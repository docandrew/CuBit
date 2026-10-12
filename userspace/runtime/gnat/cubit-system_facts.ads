------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel's system publications (KERN-002, docs/syscall-retirement.md):
--  system facts, the service registry and the wall clock, read from pages
--  the kernel maps read-only into every process; and the two system calls
--  around them (READ_STATISTICS, PUBLISH). Shared by the Ada runtime and the
--  libc's Ada, so it needs no run-time library and calls the kernel through
--  CuBit.Kernel_Calls.
--
--  @description
--  The page layouts are shared/publications/system_publications.ads; this
--  spec repeats only the reader's types, so units that use it need nothing
--  outside the runtime directory (the body checks they match). Every value
--  is one word the kernel writes with one store; a page not yet written
--  reads as zeros.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.System_Facts is

   package SP is
      --  The boot (firmware) framebuffer; Width 0: none.
      type Framebuffer_Geometry is record
         Width          : Unsigned_64 := 0;
         Height         : Unsigned_64 := 0;
         Pitch          : Unsigned_64 := 0;   --  bytes per row
         Bits_Per_Pixel : Unsigned_64 := 0;
      end record;

      --  The platform monotonic clock's startup diagnostic (kernel
      --  Platform_Monotonic), a snapshot at boot.
      type Clock_Diagnostic is record
         Status, HPET_ID, Period_Fs : Unsigned_64 := 0;
         Timer_Offset, Timer_Before, Timer_After : Unsigned_64 := 0;
      end record;

      --  READ_STATISTICS: this process's frames in bytes, its mailbox
      --  drops, and the system's free memory in bytes.
      type Statistics is record
         Owned_Bytes       : Unsigned_64 := 0;
         Event_Drops       : Unsigned_64 := 0;
         Free_Memory_Bytes : Unsigned_64 := 0;
      end record;
      for Statistics use record
         Owned_Bytes       at 0 range 0 .. 63;
         Event_Drops       at 8 range 0 .. 63;
         Free_Memory_Bytes at 16 range 0 .. 63;
      end record;

      --  What a service publishes through the kernel (PUBLISH).
      type Published_Value is (Wall_Clock_Offset, Boot_Display_Native);
      for Published_Value use (Wall_Clock_Offset => 1, Boot_Display_Native => 2);
   end SP;

   --  Where the user runtime's secondary stack starts (Process_Layout).
   Secondary_Stack_Address : constant := 16#0000_7FFF_FF00_0000#;

   --  Logical processors the kernel found (ACPI MADT).
   function Processor_Count return Unsigned_64;

   --  RAM the kernel manages, in bytes.
   function Memory_Bytes return Unsigned_64;

   --  The boot (firmware) framebuffer's geometry; Width 0: none.
   function Framebuffer return SP.Framebuffer_Geometry;

   --  devmgr gave the boot display adapter to its native driver; the
   --  firmware framebuffer is not a fallback then.
   function Boot_Display_Native return Boolean;

   --  Device-memory requests the kernel has refused so far (CAP-001); what
   --  and why reach logstore as "kernel: device-memory denied #N" records.
   function Device_Memory_Denials return Unsigned_64;

   --  User processes the kernel has killed for a fault so far; who, why and
   --  where reach logstore as "kernel: process killed #N" records.
   function Process_Kills return Unsigned_64;

   --  The platform clock's startup diagnostic (snapshot at boot).
   function Clock return SP.Clock_Diagnostic;

   --  The identity word registered for Role (CuBit.Process_IDs wire form),
   --  or 0 for none.
   function Registered (Role : Unsigned_64) return Unsigned_64;

   --  UTC milliseconds since the Unix epoch at monotonic time zero, or 0
   --  while unknown (no clock service, or its time not current).
   function Wall_Clock_Offset return Unsigned_64;

   --  This process's statistics and the system's free memory; Success
   --  False if the kernel refused (never for a correct caller).
   procedure Read_Statistics
     (Value : out SP.Statistics; Success : out Boolean);

   --  Publish Word as Value through the kernel. Only the value's publisher
   --  may (the registered clock service, or the registered device manager).
   function Publish
     (Value : SP.Published_Value; Word : Unsigned_64) return Boolean;

end CuBit.System_Facts;
