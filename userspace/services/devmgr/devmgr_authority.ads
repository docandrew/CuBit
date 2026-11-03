------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's authority (CAP-002): the roots the kernel placed in its table
--  before it ran (CuBit.Handles.Boot_Roots), which device-memory root
--  covers a physical range, and the claim ledger that keeps ACPI's pages
--  and devices' pages apart.
--
--  @description
--  Every capability devmgr hands a child is a narrowed copy of one of these
--  roots or a kernel object SPAWN gave it; devmgr itself maps device pages
--  and programs hardware through them. The roots are as many as the kernel
--  found, held on the heap (LIM-001b).
--
--  The firmware-memory roots of the ACPI hardware grant (the kernel's ACPI
--  NVS runs, FACS page and PCI segment 0 bus 0 ECAM slice) are claimed for
--  ACPI at Initialize (Devmgr_Memory_Claims) and handed, whole, only to the
--  program procmgr's startup plan approves (CuBit.ACPI_Window_Exchange; the
--  PCI slice only with acpi-pci-config approved too). The firmware-data
--  roots (table-named firmware data) are not handed out: devmgr narrows one
--  envelope at a time for a program approved with acpi-firmware-data
--  (CuBit.Firmware_Data_Exchange) and claims each envelope it grants. No
--  device-memory range that touches an ACPI claim is ever covered
--  (Device_Region), so neither a driver nor devmgr itself can map or be
--  given those pages; every device range devmgr hands out or maps is
--  claimed in turn, and so is every PCI BAR's and bridge window's whole
--  extent from enumeration on, so no data envelope overlaps one.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.ACPI_Memory_Classes;
with Devmgr_Memory_Claims;
with Devmgr_Heap;
with Devmgr_Claim_Ledgers;

package Devmgr_Authority is

   package H renames CuBit.Handles;
   package Ledgers is new Devmgr_Claim_Ledgers (Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   subtype Capacity_Report is Ledgers.Capacity_Report;

   --  Read the roots from devmgr's launch configuration and the extent of
   --  each root. Valid False when devmgr was not given them.
   procedure Initialize;
   function Valid return Boolean;
   function Roots return CuBit.Handles.Boot_Roots.Roots;

   --  The device-memory root covering [Physical, Physical + Bytes), or the
   --  null handle (also when any page of it is ACPI's: a window or a
   --  granted data envelope). A covered range is claimed for devices from
   --  then on (the device's, never ACPI's).
   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory;

   --  Whether one covers it (nothing is claimed).
   function Covered (Physical, Bytes : Unsigned_64) return Boolean;

   --  That range of the covering root, with Kept rights (a copy for a
   --  child), claimed for devices.
   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation
   with Pre => Covered (Physical, Bytes);

   --  The ACPI firmware-memory windows claimed at Initialize, in ascending
   --  address order: window K's handle (read, write, grant) and the whole
   --  pages it covers.
   type ACPI_Window is record
      Memory : H.Device_Memory := H.No_Device_Memory;
      First, Bytes : Unsigned_64 := 0;
      --  What its kernel tag says it is (CuBit.ACPI_Memory_Classes): PCI
      --  configuration is held, and handed on, read-only.
      Kind : CuBit.ACPI_Memory_Classes.Memory_Kind := CuBit.ACPI_Memory_Classes.ACPI_NVS;
   end record;
   function ACPI_Window_Count return Natural;
   function ACPI_Window_At (K : Positive) return ACPI_Window
   with Pre => K <= ACPI_Window_Count;
   --  Firmware-memory roots the kernel gave that could not be claimed
   --  (not whole pages, overlapping, not firmware memory, or no room).
   function ACPI_Windows_Refused return Natural;

   --  The firmware-data roots (Firmware_Data_ABI.Firmware_Data_Tag), ascending:
   --  root K's handle (read, write, grant) and its whole pages.
   type Data_Root is record
      Memory : H.Device_Memory := H.No_Device_Memory;
      First, Bytes : Unsigned_64 := 0;
   end record;
   function Data_Root_Count return Natural;
   function Data_Root_At (K : Positive) return Data_Root
   with Pre => K <= Data_Root_Count;

   function Device_Root_Count return Natural;
   --  Device-memory root K's extent (Bytes zero when it could not be read).
   procedure Device_Root_Extent (K : Positive; First, Bytes : out Unsigned_64)
   with Pre => K <= Device_Root_Count;

   --  Claim whole pages [Physical, Physical + Bytes) as a granted firmware-
   --  data envelope (ACPI's for the rest of the boot). Report says why when
   --  the ledger could not grow.
   procedure Claim_Data
     (Physical, Bytes : Unsigned_64; Result : out Devmgr_Memory_Claims.Claim_Result;
      Report : out Capacity_Report);

   --  No page of the range is a device's.
   function Clear_Of_Devices (Physical, Bytes : Unsigned_64) return Boolean;

   --  Claim pages [First_Page, End_Page) for a device whether or not a
   --  device-memory root covers them: a PCI BAR's whole extent or a
   --  bridge's window, found at enumeration before any driver runs
   --  (PCI_BAR_Extents). Device claims never conflict with each other, so
   --  the driver later bound to the device is granted its windows inside
   --  it (Device_Region); no data envelope ever overlaps it. Overlaps_ACPI
   --  when a page of it is an ACPI window; Needs_Room (Report says why)
   --  when the ledger could not grow.
   procedure Claim_Device_Pages
     (First_Page, End_Page : Unsigned_64; Result : out Devmgr_Memory_Claims.Claim_Result;
      Report : out Capacity_Report)
   with Pre => First_Page < End_Page and then End_Page <= Devmgr_Memory_Claims.Page_Limit;

   --  The first page of the lowest claim (ACPI's or a device's) starting
   --  above Page, or the physical limit.
   function Next_Claim_Start (Page : Unsigned_64) return Unsigned_64;

   --  The last capacity failure of a device claim (Device_Region), for the
   --  warning; Status Grown when none since the last call.
   procedure Take_Capacity_Failure (Report : out Capacity_Report; Failed : out Boolean);

end Devmgr_Authority;
