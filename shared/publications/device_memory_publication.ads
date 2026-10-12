------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Device-memory diagnostics (CAP-001): the kernel's record of every refused
--  device-memory request and its boot classification of physical memory.
--
--  @description
--  One page the kernel writes and maps read-only into the registered log
--  service only (it names physical addresses, which no other process
--  learns). Logstore turns each record into a Warning record with the
--  stable prefix "kernel: device-memory denied", and the classification
--  into "kernel: physical map" records, so a photo of the Desktop bootstrap
--  screen or the boot-logs app shows why a mapping failed on a real
--  machine whose serial console nobody can see.
--
--  Denials: a ring of Denial_Capacity records. The kernel numbers records
--  1, 2, ... (Total is the latest). Record N lives in slot
--  (N - 1) mod Denial_Capacity. While the kernel writes a record its
--  Sequence word is Writing; it is N once the record is complete. A reader
--  that last took record Seen reads Total, takes N in Seen + 1 .. Total
--  whose slot still says Sequence = N (reading Sequence again after the
--  fields), and counts the rest as lost (Denial_Reading).
--
--  Classification: written once at boot, before the first process exists.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;

package Device_Memory_Publication with Pure, SPARK_Mode is

   Page_Bytes   : constant := Clock_Publication.Page_Bytes;
   --  After the clock page and the three system publications.
   Page_Address : constant := Clock_Publication.Page_Address + 4 * Page_Bytes;

   --  2: process-kill records (Kill_Total, Kills) after the roots.
   Layout_Version : constant := 2;
   Unpublished    : constant := 0;

   --  What a physical page is (kernel Physical_Ranges.Range_Class derives
   --  from this list; most restrictive first).
   type Physical_Class is
     (Kernel_Image, Low_Memory, Boot_Module, Usable_RAM, Defective_RAM,
      ACPI_NVS, ACPI_Tables, Kernel_MMIO, Firmware_Reserved,
      Firmware_Framebuffer, Unreported);

   --  The request that was refused.
   type Operation is (Map_Device, Map_Into, Map_Framebuffer, Policy_Mint);

   --  Why: the range itself is malformed (empty, wrapping, beyond the
   --  physical limit), the classification is not usable (not sealed or
   --  incomplete), a page of the range is not device memory (Page, Class),
   --  no capability of the caller covers it with the access asked for, or
   --  (MAP_INTO read-only) the pages are not the boot image.
   type Reason is
     (Invalid_Range, Classification_Unavailable, Blocked_Page,
      No_Covering_Capability, Not_Boot_Module);

   type Access_Mode is (Read_Write, Read_Only);

   Name_Bytes : constant := 16;
   subtype Process_Name is String (1 .. Name_Bytes);

   --  A record's Sequence while the kernel is writing it.
   Writing : constant := 0;

   type Denial_Record is record
      Sequence : Unsigned_64 := Writing;
      Caller   : Unsigned_64 := 0;     --  process identity (KERN-003)
      Name     : Process_Name := [others => ' '];
      Request  : Unsigned_64 := 0;     --  Operation'Pos
      Why      : Unsigned_64 := 0;     --  Reason'Pos
      Base     : Unsigned_64 := 0;     --  physical
      Bytes    : Unsigned_64 := 0;
      Page     : Unsigned_64 := 0;     --  first offending page (Blocked_Page)
      Class    : Unsigned_64 := 0;     --  Physical_Class'Pos (Blocked_Page)
      Mode     : Unsigned_64 := 0;     --  Access_Mode'Pos
   end record;
   for Denial_Record use record
      Sequence at 0 range 0 .. 63;
      Caller   at 8 range 0 .. 63;
      Name     at 16 range 0 .. 127;
      Request  at 32 range 0 .. 63;
      Why      at 40 range 0 .. 63;
      Base     at 48 range 0 .. 63;
      Bytes    at 56 range 0 .. 63;
      Page     at 64 range 0 .. 63;
      Class    at 72 range 0 .. 63;
      Mode     at 80 range 0 .. 63;
   end record;
   Denial_Record_Bytes : constant := 88;

   Denial_Capacity : constant := 16;
   subtype Denial_Slot is Unsigned_64 range 0 .. Denial_Capacity - 1;
   type Denial_Ring is array (Denial_Slot) of Denial_Record;

   type Span_Record is record
      First, Last : Unsigned_64 := 0;  --  inclusive physical bytes
      Class       : Unsigned_64 := 0;  --  Physical_Class'Pos
   end record;
   for Span_Record use record
      First at 0 range 0 .. 63;
      Last  at 8 range 0 .. 63;
      Class at 16 range 0 .. 63;
   end record;

   type Run_Record is record
      First, Last : Unsigned_64 := 0;
   end record;
   for Run_Record use record
      First at 0 range 0 .. 63;
      Last  at 8 range 0 .. 63;
   end record;

   --  Spans beyond Span_Capacity are counted (Span_Count) but not shown.
   Span_Capacity : constant := 72;
   Root_Capacity : constant := 8;
   type Span_Table is array (1 .. Span_Capacity) of Span_Record;
   type Run_Table is array (1 .. Root_Capacity) of Run_Record;

   --  Why the kernel killed a user process. The page faults: a write or read
   --  to a page its rights forbid, an instruction fetch from a no-execute
   --  page, an address outside every reservation (not present), or no frame
   --  for a demand page (out of memory). Other CPU exceptions: Exception,
   --  with the vector in Vector.
   type Kill_Kind is
     (Write_Protection, Read_Protection, Execute_Protection, Not_Present,
      Out_Of_Memory, CPU_Exception);

   type Kill_Record is record
      Sequence : Unsigned_64 := Writing;
      Victim   : Unsigned_64 := 0;     --  process identity (KERN-003)
      Name     : Process_Name := [others => ' '];
      Kind     : Unsigned_64 := 0;     --  Kill_Kind'Pos
      Vector   : Unsigned_64 := 0;     --  CPU exception vector (14: page fault)
      Address  : Unsigned_64 := 0;     --  faulting address (page faults)
      RIP      : Unsigned_64 := 0;     --  faulting instruction, 0: unknown
      Time_Ms  : Unsigned_64 := 0;     --  kernel milliseconds since boot
   end record;
   for Kill_Record use record
      Sequence at 0 range 0 .. 63;
      Victim   at 8 range 0 .. 63;
      Name     at 16 range 0 .. 127;
      Kind     at 32 range 0 .. 63;
      Vector   at 40 range 0 .. 63;
      Address  at 48 range 0 .. 63;
      RIP      at 56 range 0 .. 63;
      Time_Ms  at 64 range 0 .. 63;
   end record;
   Kill_Record_Bytes : constant := 72;

   --  Kills use the same numbering, slot rule and Writing protocol as
   --  denials, with their own Kill_Total.
   Kill_Capacity : constant := 8;
   subtype Kill_Slot is Unsigned_64 range 0 .. Kill_Capacity - 1;
   type Kill_Ring is array (Kill_Slot) of Kill_Record;

   function Kill_Slot_Of (Number : Unsigned_64) return Kill_Slot is
     ((Number - 1) mod Kill_Capacity)
   with Pre => Number >= 1;

   --  First_Available for the kill ring.
   function First_Kill_Available (Seen, Total : Unsigned_64) return Unsigned_64 is
     (if Total - Seen > Kill_Capacity then Total - (Kill_Capacity - 1)
      else Seen + 1)
   with Pre  => Seen < Total,
        Post => First_Kill_Available'Result in Seen + 1 .. Total
                and then Total - First_Kill_Available'Result < Kill_Capacity;

   type Diagnostics_Page is record
      Version    : Unsigned_64 := Unpublished;
      Total      : Unsigned_64 := 0;   --  latest record number; 0: none
      Denials    : Denial_Ring;
      Span_Count : Unsigned_64 := 0;   --  spans in the kernel's map
      Complete   : Unsigned_64 := 0;   --  1: every description recorded
      Spans      : Span_Table;
      Root_Count : Unsigned_64 := 0;   --  devmgr's device-memory roots
      Roots      : Run_Table;
      Kill_Total : Unsigned_64 := 0;   --  latest kill record number; 0: none
      Kills      : Kill_Ring;
   end record;
   for Diagnostics_Page use record
      Version    at 0 range 0 .. 63;
      Total      at 8 range 0 .. 63;
      Denials    at 16 range 0 .. Denial_Capacity * Denial_Record_Bytes * 8 - 1;
      Span_Count at 1424 range 0 .. 63;
      Complete   at 1432 range 0 .. 63;
      Spans      at 1440 range 0 .. Span_Capacity * 24 * 8 - 1;
      Root_Count at 3168 range 0 .. 63;
      Roots      at 3176 range 0 .. Root_Capacity * 16 * 8 - 1;
      Kill_Total at 3304 range 0 .. 63;
      Kills      at 3312 range 0 .. Kill_Capacity * Kill_Record_Bytes * 8 - 1;
   end record;
   pragma Compile_Time_Error (Kill_Record'Size /= Kill_Record_Bytes * 8,
                              "a kill record is 72 bytes");
   pragma Compile_Time_Error (Denial_Record'Size /= Denial_Record_Bytes * 8,
                              "a denial record is 88 bytes");
   pragma Compile_Time_Error (Diagnostics_Page'Size / 8 > Page_Bytes,
                              "device-memory diagnostics exceed one page");

   --  The slot record Number lives in.
   function Slot_Of (Number : Unsigned_64) return Denial_Slot is
     ((Number - 1) mod Denial_Capacity)
   with Pre => Number >= 1;

   --  A reader that took every record up to Seen and now sees a larger
   --  Total takes First_Available .. Total; the records between Seen and
   --  First_Available are overwritten (lost: First_Available - Seen - 1).
   --  (A Total below Seen cannot happen while the page lives; a reader
   --  treats it as Seen = 0.)
   function First_Available (Seen, Total : Unsigned_64) return Unsigned_64 is
     (if Total - Seen > Denial_Capacity then Total - (Denial_Capacity - 1)
      else Seen + 1)
   with Pre  => Seen < Total,
        Post => First_Available'Result in Seen + 1 .. Total
                and then Total - First_Available'Result < Denial_Capacity;

   --  Short names for records and the bootstrap screen.
   function Class_Name (Class : Unsigned_64) return String is
     (if Class > Physical_Class'Pos (Physical_Class'Last) then "class?"
      else
        (case Physical_Class'Val (Class) is
           when Kernel_Image         => "kernel-image",
           when Low_Memory           => "low-memory",
           when Boot_Module          => "boot-module",
           when Usable_RAM           => "RAM",
           when Defective_RAM        => "bad-RAM",
           when ACPI_NVS             => "ACPI-NVS",
           when ACPI_Tables          => "ACPI-tables",
           when Kernel_MMIO          => "kernel-MMIO",
           when Firmware_Reserved    => "reserved",
           when Firmware_Framebuffer => "framebuffer",
           when Unreported           => "bus"));

   function Operation_Name (Request : Unsigned_64) return String is
     (if Request > Operation'Pos (Operation'Last) then "request?"
      else
        (case Operation'Val (Request) is
           when Map_Device      => "MAP_DEVICE",
           when Map_Into        => "MAP_INTO",
           when Map_Framebuffer => "MAPFB",
           when Policy_Mint     => "POLICY_MINT"));

   function Reason_Name (Why : Unsigned_64) return String is
     (if Why > Reason'Pos (Reason'Last) then "reason?"
      else
        (case Reason'Val (Why) is
           when Invalid_Range              => "invalid range",
           when Classification_Unavailable => "classification unavailable",
           when Blocked_Page               => "not device memory",
           when No_Covering_Capability     => "no covering capability",
           when Not_Boot_Module            => "not the boot image"));

   function Kill_Kind_Name (Kind : Unsigned_64) return String is
     (if Kind > Kill_Kind'Pos (Kill_Kind'Last) then "kind?"
      else
        (case Kill_Kind'Val (Kind) is
           when Write_Protection   => "write-protection",
           when Read_Protection    => "read-protection",
           when Execute_Protection => "execute-protection",
           when Not_Present        => "not-present",
           when Out_Of_Memory      => "out-of-memory",
           when CPU_Exception      => "exception"));

   function Mode_Name (Mode : Unsigned_64) return String is
     (if Mode = Access_Mode'Pos (Read_Only) then "ro" else "rw");
end Device_Memory_Publication;
