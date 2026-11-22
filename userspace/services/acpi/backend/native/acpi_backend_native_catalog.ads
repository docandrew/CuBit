pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Catalog;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Port_Access;
generic
   Maximum_Windows, Maximum_Registers, Maximum_Scopes : Positive;
   Virtual_First, Virtual_Limit : Interfaces.Unsigned_64;
   Mapping_Page_Limit : Positive;
   with procedure Map_Memory
     (Authority : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean)
     is ACPI_Backend_Kernel_Handles.Map_Memory;
package ACPI_Backend_Native_Catalog is
   package Owner is new ACPI_Backend_Catalog
     (CuBit.Handles.Device_Memory, CuBit.Handles.No_Device_Memory,
      CuBit.Handles.IO_Ports, CuBit.Handles.No_IO_Ports,
      ACPI_Backend_Kernel_Handles.Inspect_Memory, ACPI_Backend_Kernel_Handles.Inspect_Ports,
      Map_Memory, ACPI_Backend_Kernel_Handles.Read_Clock,
      ACPI_Backend_Memory_Access.Transfer, ACPI_Backend_Port_Access.Transfer,
      Maximum_Windows, Maximum_Registers, Maximum_Scopes, Virtual_First, Virtual_Limit, Mapping_Page_Limit);
end ACPI_Backend_Native_Catalog;
