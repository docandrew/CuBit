------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ACPI hardware windows a launcher grants acpi-backend.svc
--  (coordination/acpi-region-boundary.md, "Concrete forms").
--
--  @description
--  A program declares (Request.ACPI_Hardware "<binding>"); the trusted
--  startup plan approves it, (acpi-hardware approve-declared); the
--  generated Startup.<Binding> returns this Grant
--  (CuBit.ACPI_Windows.Launch.Named). Each window's kernel handle is the
--  authority and enforces exactly its range; the numbers only say what the
--  handle covers. Unused slots hold null handles and the bounds' lowest
--  values.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.ACPI_Memory_Classes;

package CuBit.ACPI_Windows with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   --  Bounds of one launch's grant (the launcher refuses more).
   Maximum_IO_Windows     : constant := 16;
   Maximum_Memory_Windows : constant := 16;
   subtype IO_Window_Count     is Natural range 0 .. Maximum_IO_Windows;
   subtype Memory_Window_Count is Natural range 0 .. Maximum_Memory_Windows;
   subtype Port_Number is Interfaces.Unsigned_16;
   subtype Port_Count  is Interfaces.Unsigned_32 range 1 .. 65_536;
   subtype Page_Number is Interfaces.Unsigned_64;     --  physical page frame
   subtype Page_Count  is Interfaces.Unsigned_64 range 1 .. 2 ** 40;

   type IO_Window is record
      Ports : CuBit.Handles.IO_Ports := CuBit.Handles.No_IO_Ports;  --  enforces exactly this range
      First : Port_Number := Port_Number'First;
      Count : Port_Count := Port_Count'First;
   end record;

   --  What a memory window is and how the kernel maps it, from its own
   --  capability's kernel tag (CuBit.ACPI_Memory_Classes), never from the
   --  launcher.
   --
   --  Mapping contract (slice 2):
   --  * Map with CuBit.Handles.Devices.Map_Device (Memory, Physical,
   --    Virtual, Pages, Read_Only), i.e. MAP_DEVICE through this window's
   --    own handle. Physical is a page address inside the window; the
   --    caller chooses Virtual: page aligned, in the user range, and NOT
   --    inside the owned-memory aperture (Owned_Memory_Layout, which the
   --    kernel refuses) or a received grant; drivers use a fixed device
   --    area such as 16#6000_0000_0000#. At most 1024 pages per call,
   --    mapped eagerly by that call: map the pages a region needs, on
   --    demand (a whole 12,208-page NVS run is 12 calls and 24 page-table
   --    pages). The kernel maps NVS and the FACS page write-back, and PCI
   --    configuration (ACPI-PCI-001) uncached and only with Read_Only
   --    (Cache; CuBit.ACPI_Memory_Classes.Writable_Of).
   --  * A PCI configuration window is granted only when the startup plan
   --    also approves (acpi-pci-config approve-declared); without it the
   --    grant holds no PCI_Config window at all.
   --  * Kind and Cache are what INSPECT_CAPABILITY (CuBit.Handles.IPC.
   --    Describe (Memory).Authority_Tag) says; Launch.Named fills them.
   --  * No unmap call exists: a mapping lasts until the process exits.
   --    devmgr's ACPI claim on the window is never released for the rest
   --    of the boot (frozen whether or not mapped), and a restarted backend
   --    is granted the same window again.
   subtype Memory_Kind is CuBit.ACPI_Memory_Classes.Memory_Kind;
   subtype Cache_Mode  is CuBit.ACPI_Memory_Classes.Cache_Mode;

   type Memory_Window is record
      Memory     : CuBit.Handles.Device_Memory := CuBit.Handles.No_Device_Memory;  --  whole pages only
      First_Page : Page_Number := Page_Number'First;
      Pages      : Page_Count := Page_Count'First;
      Kind       : Memory_Kind := CuBit.ACPI_Memory_Classes.ACPI_NVS;
      Cache      : Cache_Mode := CuBit.ACPI_Memory_Classes.Uncached;
   end record;

   type IO_Windows     is array (1 .. Maximum_IO_Windows) of IO_Window;
   type Memory_Windows is array (1 .. Maximum_Memory_Windows) of Memory_Window;

   type Grant is record
      IO_Used     : IO_Window_Count := 0;
      IO          : IO_Windows;
      Memory_Used : Memory_Window_Count := 0;
      Memory      : Memory_Windows;
   end record;
   --  Windows are sorted by first port / first page and never overlap;
   --  an empty Grant (both counts 0) means nothing was admitted.
end CuBit.ACPI_Windows;
