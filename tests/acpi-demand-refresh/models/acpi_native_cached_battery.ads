with ACPI_Requests;with CuBit.Messages;
package ACPI_Native_Cached_Battery is
 procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;Read_Allowed:Boolean;Reply:out CuBit.Messages.Message;Handled:out Boolean);
end ACPI_Native_Cached_Battery;
