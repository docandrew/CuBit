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
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.ACPI_Window_Exchange with Pure, SPARK_Mode is

   Window_Request : constant Unsigned_32 := 16#024B#;

   Request_Words : constant := 2;
   First_Word    : constant := 0;
   Count_Word    : constant := 1;

   --  An FADT block's length is one byte; an EC register is one port.
   Maximum_Window_Ports : constant := 255;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

end CuBit.ACPI_Window_Exchange;
