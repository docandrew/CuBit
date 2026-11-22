with Admission_Probe;
package body ACPI_Native_Cached_Power is
 procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
  Read_Allowed : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean) is
  pragma Unreferenced (Server, Request);
 begin
  Admission_Probe.Cached_Power_Allowed := Read_Allowed;
  Reply := CuBit.Messages.NULL_MESSAGE; Handled := False;
 end Dispatch;
end ACPI_Native_Cached_Power;
