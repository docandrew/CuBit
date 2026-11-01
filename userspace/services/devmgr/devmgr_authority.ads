------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's authority (CAP-002): the roots the kernel placed in its table
--  before it ran (CuBit.Handles.Boot_Roots), and which device-memory root
--  covers a physical range.
--
--  @description
--  Every capability devmgr hands a child is a narrowed copy of one of these
--  roots or a kernel object SPAWN gave it; devmgr itself maps device pages
--  and programs hardware through them.
--
--  The firmware-memory roots (the kernel's ACPI NVS runs and FACS page) are
--  claimed for ACPI at Initialize (Devmgr_Memory_Claims) and handed, whole,
--  only to the program procmgr's startup plan approves
--  (CuBit.ACPI_Window_Exchange). No device-memory range that touches one of
--  their pages is ever covered (Device_Region), so neither a driver nor
--  devmgr itself can map or be given them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;

package Devmgr_Authority is

   package H renames CuBit.Handles;

   --  Read the roots from devmgr's launch configuration and the extent of
   --  each device-memory root. Valid False when devmgr was not given them.
   procedure Initialize;
   function Valid return Boolean;
   function Roots return CuBit.Handles.Boot_Roots.Roots;

   --  The device-memory root covering [Physical, Physical + Bytes), or the
   --  null handle (also when any page of it is an ACPI window's).
   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory;

   --  That range of the covering root, with Kept rights (a copy for a
   --  child). Covered tells whether one covers it.
   function Covered (Physical, Bytes : Unsigned_64) return Boolean is
     (H.Valid (Device_Region (Physical, Bytes)));
   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation
   with Pre => Covered (Physical, Bytes);

   --  The ACPI firmware-memory windows claimed at Initialize, in ascending
   --  address order: window K's handle (read, write, grant) and the whole
   --  pages it covers.
   type ACPI_Window is record
      Memory : H.Device_Memory := H.No_Device_Memory;
      First, Bytes : Unsigned_64 := 0;
   end record;
   function ACPI_Window_Count return Natural;
   function ACPI_Window_At (K : Positive) return ACPI_Window
   with Pre => K <= ACPI_Window_Count;
   --  Firmware-memory roots the kernel gave that could not be claimed
   --  (not whole pages, overlapping, or not firmware memory).
   function ACPI_Windows_Refused return Natural;

end Devmgr_Authority;
