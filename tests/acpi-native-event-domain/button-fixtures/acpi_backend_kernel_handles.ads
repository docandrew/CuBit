with ACPI_Backend_Binding_Types;
-- Clock-only hosted native boundary. No IPC or hardware behavior is modeled.
package ACPI_Backend_Kernel_Handles is
 procedure Read_Clock(Now:out ACPI_Backend_Binding_Types.Tick;Available:out Boolean);
end ACPI_Backend_Kernel_Handles;
