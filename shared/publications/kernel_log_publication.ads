------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel log ring (KERN-002 publication, LOG-004): the kernel's own
--  console lines and its typed reports of processes it stopped (a CPU
--  exception, a page it could not back, another process stopping it:
--  Fault_Reports), kept from the first boot line on, for the log service
--  to drain whenever it runs. No program's text is ever in it: programs
--  log through their own publisher rings (LOG-001).
--
--  @description
--  Like Linux's printk buffer read through /dev/kmsg by journald: the kernel
--  writes records into a bounded ring whether or not anything reads it, and
--  never waits for a reader. Only the registered log service reads it: the
--  kernel maps the ring read-only and non-executable into the process that
--  registers the logstore role (SYSCALL_REGISTER_DRIVER, which needs the
--  logstore notification capability), never into any other. No system call
--  reads it; logstore polls it every pass of its loop.
--
--  Records are numbered 1, 2, ... (Total is the latest). Record N lives in
--  slot Slot_Of (N, Capacity). The kernel writes one record at a time
--  (Kernel_Log's lock): it stores Writing into the slot's Sequence, then
--  the other fields, then N into Sequence, then N into Total, each store in
--  that order (volatile; x86 keeps stores in order). So every record up to
--  Total was complete when Total named it.
--
--  A full ring overwrites its oldest record. That is the declared loss of
--  this channel: the reader (Drain) counts exactly which numbers it did not
--  get, those overwritten before it read them, or while it copied them,
--  and reports each run as a dropped range, which logstore logs as
--  "kernel log: N records dropped (sequence a..b)".
--
--  Capacity is the kernel's build setting (Kernel_Log_Records, default
--  Maximum_Records); the reader takes it from the header, so a test build
--  with a small ring needs no other change. Maximum_Records is the named
--  ceiling (LIM-001): the mapped area is sized for it.
--
--  Why 4096 records (1 MiB): a QEMU desktop boot prints about 600-1,100
--  kernel lines before logstore starts (about 15 per program loaded, plus
--  the memory map, ACPI and CPU bring-up), a laptop more (a longer memory
--  map, more ACPI tables and drivers). 4096 holds such a boot several times
--  over, and a whole minute of one STUCK-CALL report per second for 60
--  threads, while costing 1 MiB of a machine's RAM.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;
with Fault_Reports;

package Kernel_Log_Publication with Pure, SPARK_Mode is

   Page_Bytes : constant := Clock_Publication.Page_Bytes;
   --  After the six one-page publications (clock, the three system pages,
   --  device-memory diagnostics, CPU times), leaving room for ten more.
   --  The whole area shares their page table (it ends below 2 MiB).
   Page_Address : constant := Clock_Publication.Page_Address + 16 * Page_Bytes;

   Layout_Version : constant := 1;
   Unpublished    : constant := 0;

   ---------------------------------------------------------------------------
   --  Sizes
   ---------------------------------------------------------------------------
   Record_Bytes : constant := 256;
   Header_Bytes : constant := 64;
   Name_Bytes   : constant := 16;
   Fault_Bytes  : constant := Fault_Reports.Report_Bytes;
   Text_Bytes   : constant := Record_Bytes - 48 - Fault_Bytes;   --  144
   Records_Per_Page : constant := Page_Bytes / Record_Bytes;

   --  The named ceiling (LIM-001): the records the mapped area holds.
   Maximum_Records : constant := 4_096;
   Record_Pages    : constant := Maximum_Records / Records_Per_Page;
   --  The header has a page of its own, then the records.
   Area_Pages      : constant := 1 + Record_Pages;
   Area_Bytes      : constant := Area_Pages * Page_Bytes;

   pragma Compile_Time_Error
     (Maximum_Records mod Records_Per_Page /= 0, "the ring fills whole pages");
   pragma Compile_Time_Error
     ((Page_Address - Clock_Publication.Page_Address) / Page_Bytes + Area_Pages > 512,
      "the kernel log must share the publications' page table");

   subtype Record_Capacity is Unsigned_64 range 1 .. Maximum_Records;
   subtype Slot is Unsigned_64 range 0 .. Maximum_Records - 1;

   --  A record's number; Writing in Sequence while the kernel writes it.
   Writing : constant := 0;
   subtype Record_Number is Unsigned_64 range 1 .. Unsigned_64'Last;

   ---------------------------------------------------------------------------
   --  Who a record is about
   ---------------------------------------------------------------------------

   --  The source word of the kernel's own records. A process word is a
   --  process identity (KERN-003): a slot in the low 24 bits and its
   --  generation above. The kernel has far fewer slots than 2**24 - 1
   --  (Kernel_Log checks Config.MAX_PROCESSES against it), so this word,
   --  the last slot with generation zero, never names a process.
   Kernel_Source : constant Unsigned_64 := 2 ** 24 - 1;

   --  Kernel_Line: one line the kernel printed on its console (Text).
   --  Process_Stopped: the kernel stopped the process Source, named Name,
   --  for the reason in Fault; no Text. Each stop is two records: the
   --  summary (Everyone) and the detail (Diagnostics), unless it has no
   --  detail (stopped by another process).
   type Origin is (Kernel_Line, Process_Stopped);
   for Origin use (Kernel_Line => 1, Process_Stopped => 2);

   --  The log levels, in CuBit.Log_Records.Severity's order.
   type Level is (Trace, Debug, Information, Warning, Error, Critical);
   for Level use
     (Trace => 0, Debug => 1, Information => 2, Warning => 3, Error => 4,
      Critical => 5);

   --  Who may read a record. Everyone: every reader of the logs it is
   --  filed under. Diagnostics: only a reader holding a diagnostics
   --  authority; a fault's detail can point at memory corruption an
   --  attacker could use. Logstore's readers have no such authority today
   --  (the log-observer badge is all or nothing), so logstore keeps
   --  Diagnostics records out of the store: they stay in this ring, which
   --  only the logstore-role capability holder can map.
   type Audience is (Everyone, Diagnostics);
   for Audience use (Everyone => 0, Diagnostics => 1);

   --  Flags. Truncated: the line was longer than Text_Bytes (the rest is
   --  only on the serial console).
   Truncated_Flag  : constant := 1;

   subtype Name_Text is String (1 .. Name_Bytes);
   subtype Record_Text is String (1 .. Text_Bytes);
   subtype Text_Length is Unsigned_16 range 0 .. Text_Bytes;

   ---------------------------------------------------------------------------
   --  The area
   ---------------------------------------------------------------------------
   type Log_Record is record
      Sequence : Unsigned_64 := Writing;
      Time_Ms  : Unsigned_64 := 0;       --  kernel monotonic ms (msTicks)
      Source   : Unsigned_64 := 0;       --  Kernel_Source or a process
      Name     : Name_Text := [others => ASCII.NUL];  --  process name
      Kind     : Unsigned_8 := 0;        --  Origin'Enum_Rep
      Severity : Unsigned_8 := 0;        --  Level'Enum_Rep
      Flags    : Unsigned_8 := 0;
      Readers  : Unsigned_8 := 0;        --  Audience'Enum_Rep
      Length   : Unsigned_16 := 0;       --  bytes of Text used
      Spare    : Unsigned_16 := 0;
      Fault    : Fault_Reports.Report;   --  Process_Stopped records
      Text     : Record_Text := [others => ASCII.NUL];
   end record;
   for Log_Record use record
      Sequence at 0 range 0 .. 63;
      Time_Ms  at 8 range 0 .. 63;
      Source   at 16 range 0 .. 63;
      Name     at 24 range 0 .. Name_Bytes * 8 - 1;
      Kind     at 40 range 0 .. 7;
      Severity at 41 range 0 .. 7;
      Flags    at 42 range 0 .. 7;
      Readers  at 43 range 0 .. 7;
      Length   at 44 range 0 .. 15;
      Spare    at 46 range 0 .. 15;
      Fault    at 48 range 0 .. Fault_Bytes * 8 - 1;
      Text     at 48 + Fault_Bytes range 0 .. Text_Bytes * 8 - 1;
   end record;
   for Log_Record'Size use Record_Bytes * 8;

   type Header is record
      Version  : Unsigned_64 := Unpublished;
      --  The records the ring holds (the kernel's Kernel_Log_Records).
      Capacity : Unsigned_64 := 0;
      --  The latest record number; 0: none yet.
      Total    : Unsigned_64 := 0;
      --  Records the kernel gave up without numbering them: a CPU that was
      --  already writing a record was interrupted by a path that logs (an
      --  NMI or a fault in the writer). Diagnostic count only.
      Reentered : Unsigned_64 := 0;
      Spare     : Unsigned_64 := 0;
      Spare_2   : Unsigned_64 := 0;
      Spare_3   : Unsigned_64 := 0;
      Spare_4   : Unsigned_64 := 0;
   end record;
   for Header use record
      Version   at 0 range 0 .. 63;
      Capacity  at 8 range 0 .. 63;
      Total     at 16 range 0 .. 63;
      Reentered at 24 range 0 .. 63;
      Spare     at 32 range 0 .. 63;
      Spare_2   at 40 range 0 .. 63;
      Spare_3   at 48 range 0 .. 63;
      Spare_4   at 56 range 0 .. 63;
   end record;
   for Header'Size use Header_Bytes * 8;

   type Record_Table is array (Slot) of Log_Record;

   type Ring_Area is record
      Head    : Header;
      Records : Record_Table;
   end record;
   for Ring_Area use record
      Head    at 0 range 0 .. Header_Bytes * 8 - 1;
      Records at Page_Bytes range 0 .. Maximum_Records * Record_Bytes * 8 - 1;
   end record;
   for Ring_Area'Size use Area_Bytes * 8;

   ---------------------------------------------------------------------------
   --  Numbering
   ---------------------------------------------------------------------------

   --  A header a reader may use: this layout and a capacity the area holds.
   function Valid_Header (H : Header) return Boolean is
     (H.Version = Layout_Version and then H.Capacity in Record_Capacity);

   --  The slot record Number lives in (one of the first Capacity: the
   --  definition says so; the Slot subtype keeps every index in the area
   --  whatever the capacity).
   function Slot_Of (Number : Record_Number; Capacity : Record_Capacity) return Slot is
     ((Number - 1) mod Capacity);

   --  The writer's next number after Total. The kernel stops numbering at
   --  Unsigned_64'Last (2**64 records: never, at any real rate).
   function Can_Number (Total : Unsigned_64) return Boolean is
     (Total < Unsigned_64'Last);
   function Next_Number (Total : Unsigned_64) return Record_Number is
     (Total + 1)
   with Pre  => Can_Number (Total),
        Post => Next_Number'Result > Total;

   --  A reader that has accounted for every record up to Seen and sees a
   --  larger Total: the oldest record still in the ring. The records
   --  between Seen and it were overwritten (First_Available - Seen - 1).
   function First_Available
     (Seen : Unsigned_64; Total : Record_Number; Capacity : Record_Capacity)
      return Record_Number
   is (if Total - Seen > Capacity then Total - (Capacity - 1) else Seen + 1)
   with Pre  => Seen < Total,
        Post => First_Available'Result in Seen + 1 .. Total
                and then Total - First_Available'Result < Capacity;

   ---------------------------------------------------------------------------
   --  Text
   ---------------------------------------------------------------------------

   --  What a record may hold: printable ASCII. Others become Replacement
   --  (tabs a space), so logstore's record text is always valid.
   Replacement : constant Character := '?';
   function Printable (C : Character) return Boolean is (C in ' ' .. '~');
   function Cleaned (C : Character) return Character is
     (if Printable (C) then C elsif C = ASCII.HT then ' ' else Replacement)
   with Post => Printable (Cleaned'Result);

   --  A kernel line, numbered later: Sequence is Writing (the writer
   --  stores the number last). Text beyond Text_Bytes is cut and flagged
   --  Truncated, as is any text the caller already cut. Its level comes
   --  from its prefix (Line_Level).
   function Make_Line
     (Time_Ms   : Unsigned_64;
      Text      : String;
      Truncated : Boolean) return Log_Record
   with Pre  => Text'Last < Positive'Last,
        Post => Make_Line'Result.Sequence = Writing
                and then Make_Line'Result.Source = Kernel_Source
                and then Make_Line'Result.Length <= Text_Bytes
                and then Make_Line'Result.Kind = Origin'Enum_Rep (Kernel_Line)
                and then (for all C of Make_Line'Result.Text
                            (1 .. Natural (Make_Line'Result.Length)) => Printable (C));

   --  The kernel stopped Source (named Name) for Fault: an Error, for
   --  Readers. An Everyone record holds only Fault's summary.
   function Make_Stop
     (Time_Ms : Unsigned_64;
      Source  : Unsigned_64;
      Name    : Name_Text;
      Fault   : Fault_Reports.Report;
      Readers : Audience) return Log_Record
   with Post => Make_Stop'Result.Sequence = Writing
                and then Make_Stop'Result.Length = 0
                and then Make_Stop'Result.Kind = Origin'Enum_Rep (Process_Stopped)
                and then Make_Stop'Result.Severity = Level'Enum_Rep (Error)
                and then Make_Stop'Result.Readers = Audience'Enum_Rep (Readers)
                and then (if Readers = Everyone
                          then Fault_Reports."=" (Make_Stop'Result.Fault, Fault_Reports.Summary_Of (Fault)));

   --  Who may read a record as copied (anything but Everyone is
   --  Diagnostics: a reader trusts nothing it copied).
   function Audience_Of (Item : Log_Record) return Audience is
     (if Item.Readers = Audience'Enum_Rep (Everyone) then Everyone else Diagnostics);

   --  Whether a reader with Reader's authority may see Item.
   function Visible (Item : Log_Record; Reader : Audience) return Boolean is
     (Audience_Of (Item) = Everyone or else Reader = Diagnostics);

   --  The text of a record as read: at most Text_Bytes, printable only
   --  (a record is the kernel's, but a reader trusts nothing it copied).
   function Used_Length (Item : Log_Record) return Natural is
     (Natural (Unsigned_16'Min (Item.Length, Text_Bytes)))
   with Post => Used_Length'Result <= Text_Bytes;
   function Text_Of (Item : Log_Record) return String
   with Post => Text_Of'Result'First = 1
                and then Text_Of'Result'Length = Used_Length (Item)
                and then (for all C of Text_Of'Result => Printable (C));

   --  The process name's length: up to its first NUL, without trailing
   --  spaces.
   function Name_Length (Item : Log_Record) return Natural
   with Post => Name_Length'Result <= Name_Bytes;

   --  The process name, up to its first NUL or trailing spaces.
   function Name_Of (Item : Log_Record) return String
   with Post => Name_Of'Result'First = 1
                and then Name_Of'Result'Length <= Name_Bytes
                and then (for all C of Name_Of'Result => Printable (C));

   function Level_Of (Item : Log_Record) return Level is
     (case Item.Severity is
         when Level'Enum_Rep (Trace)    => Trace,
         when Level'Enum_Rep (Debug)    => Debug,
         when Level'Enum_Rep (Warning)  => Warning,
         when Level'Enum_Rep (Error)    => Error,
         when Level'Enum_Rep (Critical) => Critical,
         when others                    => Information);
   function Origin_Of (Item : Log_Record) return Origin is
     (if Item.Kind = Origin'Enum_Rep (Process_Stopped) then Process_Stopped
      else Kernel_Line);
   function Truncated (Item : Log_Record) return Boolean is
     ((Item.Flags and Truncated_Flag) /= 0);

   --  A kernel console line's level, from its prefix (the kernel's
   --  printing conventions): fatal and panic lines Critical, process kills
   --  and failed allocations Error, stuck calls, refused device memory and
   --  warnings Warning, everything else Information.
   function Starts_With (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length
      and then Text (Text'First .. Text'First + (Prefix'Length - 1)) = Prefix)
   with Pre => Text'Last < Positive'Last;
   function Line_Level (Text : String) return Level is
     (if Starts_With (Text, "FATAL:") or else Starts_With (Text, "PANIC")
         or else Starts_With (Text, "Kernel panic")
      then Critical
      elsif Starts_With (Text, "process killed") or else Starts_With (Text, "USER-MEMORY-FAULT:")
         or else Starts_With (Text, "PAGE-ALLOCATION-FAILED:")
      then Error
      elsif Starts_With (Text, "STUCK-CALL:") or else Starts_With (Text, "STUCK-WATCH:")
         or else Starts_With (Text, "WARNING:") or else Starts_With (Text, "SCHED-ALARM:")
         or else Starts_With (Text, "device-memory denied")
      then Warning
      else Information)
   with Pre => Text'Last < Positive'Last;
end Kernel_Log_Publication;
