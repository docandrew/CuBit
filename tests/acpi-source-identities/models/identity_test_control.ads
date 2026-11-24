with ACPI_Source_Paths;with ACPI_Power_Identity_Protocol;
package Identity_Test_Control is
 Calls:Natural:=0;
 Value:ACPI_Source_Paths.Path:=ACPI_Source_Paths.Empty;
 Code:ACPI_Power_Identity_Protocol.Result_Code:=ACPI_Power_Identity_Protocol.OK;
end Identity_Test_Control;
