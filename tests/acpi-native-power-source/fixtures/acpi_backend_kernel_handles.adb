package body ACPI_Backend_Kernel_Handles is
   procedure Read_Clock(Value:out ACPI_Backend_Binding_Types.Tick;Available:out Boolean) is
   begin Value:=Clock;Available:=Clock_Available;end Read_Clock;
end ACPI_Backend_Kernel_Handles;
