with ACPI_Requests;with CuBit.Messages;
package ACPI_Native_Battery_Reads is
 procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;Read_Allowed:Boolean;
  Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean);
 procedure Begin_Stop;
 function Pending return Boolean;function Quiet return Boolean;
 procedure Advance(Server:ACPI_Requests.State;Owner_Quiet,Read_Allowed:Boolean;Did_Work:out Boolean);
end ACPI_Native_Battery_Reads;
