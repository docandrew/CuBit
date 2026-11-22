with Interfaces;with ACPI_PCI_Protocol;with ACPI_Local_PCI_Results;package ACPI_Native_PCI_Windows is
 subtype Word is Interfaces.Unsigned_64;subtype Local_Window is ACPI_Local_PCI_Results.Window_Description;
 procedure Prepare_Local(Scope:Word;Key:ACPI_PCI_Protocol.P.Bus_Key;Setup_Deadline_Milliseconds:ACPI_PCI_Protocol.Wire.Time_Instant;Allow_Setup:Boolean;Result:out Local_Window);
 procedure Read_Local(Scope,Epoch,ID:Word;Region:ACPI_PCI_Protocol.P.Region;Offset:Word;Width:ACPI_PCI_Protocol.P.T.Access_Width;Deadline_Microseconds:Word;Guard:not null access function return Boolean;Result:out ACPI_Local_PCI_Results.Result);
end ACPI_Native_PCI_Windows;