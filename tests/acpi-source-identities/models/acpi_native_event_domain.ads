with ACPI_Requests;with ACPI_Cached_Power_Values;with ACPI_Power_Identity_Protocol;with ACPI_Source_Paths;
package ACPI_Native_Event_Domain is
 procedure Read_Identity(Server:ACPI_Requests.State;Index:Natural;Expected:ACPI_Cached_Power_Values.Generation;
  Current:out ACPI_Cached_Power_Values.Generation;Code:out ACPI_Power_Identity_Protocol.Result_Code;Value:out ACPI_Source_Paths.Path);
end ACPI_Native_Event_Domain;
