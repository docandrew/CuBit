pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Bindings;
with ACPI_Backend_Kernel_Handles;
generic
   Maximum_Windows : Positive;
   Virtual_First, Virtual_Limit : Interfaces.Unsigned_64;
   Mapping_Page_Limit : Positive;
package ACPI_Backend_Native_Bindings is
   package Owner is new ACPI_Backend_Bindings
     (CuBit.Handles.Device_Memory, CuBit.Handles.No_Device_Memory,
      CuBit.Handles.IO_Ports, CuBit.Handles.No_IO_Ports,
      ACPI_Backend_Kernel_Handles.Inspect_Memory, ACPI_Backend_Kernel_Handles.Inspect_Ports,
      ACPI_Backend_Kernel_Handles.Map_Memory, ACPI_Backend_Kernel_Handles.Read_Clock,
      Maximum_Windows, Virtual_First, Virtual_Limit, Mapping_Page_Limit);
end ACPI_Backend_Native_Bindings;
