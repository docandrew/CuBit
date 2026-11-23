with Identity_Test_Control;
package body ACPI_Native_Event_Domain is
 use type ACPI_Cached_Power_Values.Generation;
 procedure Read_Identity(Server:ACPI_Requests.State;Index:Natural;Expected:ACPI_Cached_Power_Values.Generation;
  Current:out ACPI_Cached_Power_Values.Generation;Code:out ACPI_Power_Identity_Protocol.Result_Code;Value:out ACPI_Source_Paths.Path) is
  pragma Unreferenced(Server,Index);
 begin Identity_Test_Control.Calls:=Identity_Test_Control.Calls+1;Current:=6;Value:=ACPI_Source_Paths.Empty;
  if Expected/=6 then Code:=ACPI_Power_Identity_Protocol.Stale;return;end if;
  Code:=Identity_Test_Control.Code;Value:=Identity_Test_Control.Value;
 end;
end ACPI_Native_Event_Domain;
