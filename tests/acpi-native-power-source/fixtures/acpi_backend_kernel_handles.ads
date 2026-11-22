with ACPI_Backend_Binding_Types;
package ACPI_Backend_Kernel_Handles is
   Clock:ACPI_Backend_Binding_Types.Tick:=20;
   Clock_Available:Boolean:=True;
   procedure Read_Clock(Value:out ACPI_Backend_Binding_Types.Tick;Available:out Boolean);
end ACPI_Backend_Kernel_Handles;
