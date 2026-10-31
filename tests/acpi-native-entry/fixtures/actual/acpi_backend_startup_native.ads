pragma Ada_2022;
with CuBit.Handles;
with CuBit.ACPI_Windows;
with ACPI_Backend_Catalog;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Startup;
with ACPI_FADT;
generic
   with package Catalog is new ACPI_Backend_Catalog
     (Memory_Authority => CuBit.Handles.Device_Memory,
      No_Memory => CuBit.Handles.No_Device_Memory,
      Port_Authority => CuBit.Handles.IO_Ports,
      No_Ports => CuBit.Handles.No_IO_Ports, others => <>);
package ACPI_Backend_Startup_Native is
   package Owner is new ACPI_Backend_Startup
     (Catalog, ACPI_Backend_Kernel_Handles.Read_Clock,
      CuBit.ACPI_Windows.Maximum_IO_Windows, CuBit.ACPI_Windows.Maximum_Memory_Windows);
   -- Called only by backend main with the typed Startup.Hardware grant.
   -- Nothing in the scalar or inventory IPC may supply this input.
   -- Only used entries are read; the launcher owns grant initialization and
   -- must independently exclude RAM, other claims and partial shared pages.
   procedure Initialize
     (Run : in out Owner.Context; Hardware : in out Catalog.Context;
      Input : aliased in CuBit.ACPI_Windows.Grant;
      FADT : ACPI_FADT.Description; Deadline : Owner.B.Tick; Result : out Owner.Report);
end ACPI_Backend_Startup_Native;
