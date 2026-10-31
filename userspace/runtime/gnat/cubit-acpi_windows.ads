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

   type Memory_Window is record
      Memory     : CuBit.Handles.Device_Memory := CuBit.Handles.No_Device_Memory;  --  whole pages only
      First_Page : Page_Number := Page_Number'First;
      Pages      : Page_Count := Page_Count'First;
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
