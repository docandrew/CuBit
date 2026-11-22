pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Binding_Types;
package ACPI_Backend_Kernel_Handles is
   procedure Inspect_Memory
     (Authority : CuBit.Handles.Device_Memory; Item : out ACPI_Backend_Binding_Types.Description);
   procedure Inspect_Ports
     (Authority : CuBit.Handles.IO_Ports; Item : out ACPI_Backend_Binding_Types.Description);
   procedure Map_Memory
     (Authority : CuBit.Handles.Device_Memory; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean);
   procedure Read_Clock
     (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
end ACPI_Backend_Kernel_Handles;
