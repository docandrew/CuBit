------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI I/O windows between the two launchers
--  (coordination/acpi-region-boundary.md, slice 1).
--
--  @description
--  devmgr holds the I/O port space and the record of who holds which ports;
--  procmgr computes an approved program's ACPI windows from the firmware
--  tables (CuBit.ACPI_Fixed_Windows) and asks devmgr for each, one call
--  per window, from procmgr to devmgr (synchronous calls only go down).
--
--  Window_Request: a call accepting a capability, from procmgr only and
--  through its broker endpoint. words (First_Word) = the first port,
--  words (Count_Word) = the number of ports (1 .. Maximum_Window_Ports).
--  devmgr claims exactly that range for ACPI for the rest of the boot,
--  unless a driver or devmgr itself holds any port of it, or it partly
--  overlaps an earlier ACPI window: the same window may be asked again (a
--  restarted backend), never a different overlapping one. Later driver
--  claims overlapping an ACPI window are refused. REPLY_OK carries an
--  I/O-port capability covering exactly the range, with read, write and
--  grant (procmgr installs a copy without grant); Reply_Error carries
--  nothing.
--
--  Memory_Window_Request (slice 2): the same kind of call, words
--  (Window_Word) = K (1 .. Maximum_Memory_Windows). devmgr answers with its
--  K-th ACPI firmware-memory window, ascending by address: the kernel's
--  firmware-memory root itself (an ACPI NVS run, the FACS page, or PCI
--  segment 0 bus 0's ECAM slice), claimed for ACPI since devmgr started,
--  never narrowed or widened. REPLY_OK carries it with read and grant, and
--  write unless it is PCI configuration (CuBit.ACPI_Memory_Classes.
--  Writable_Of), and words (First_Word) = its first physical byte, words
--  (Bytes_Word) = its length (procmgr installs a copy without grant, and the
--  PCI slice only for a program the startup plan approves with
--  acpi-pci-config; the program reads the extent and kind back from the
--  kernel, never these words). Reply_Error: there is no K-th window (the
--  windows end).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.ACPI_Window_Exchange with Pure, SPARK_Mode is

   Window_Request : constant Unsigned_32 := 16#024B#;

   Request_Words : constant := 2;
   First_Word    : constant := 0;
   Count_Word    : constant := 1;

   --  An FADT block's length is one byte; an EC register is one port.
   Maximum_Window_Ports : constant := 255;

   Memory_Window_Request : constant Unsigned_32 := 16#024C#;
   Window_Word : constant := 0;
   Bytes_Word  : constant := 1;
   --  CuBit.ACPI_Windows.Maximum_Memory_Windows.
   Maximum_Memory_Windows : constant := 16;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

end CuBit.ACPI_Window_Exchange;
