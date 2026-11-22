package body ACPI_Native_Power_Identities is
 procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;Read_Allowed:Boolean;
  Reply:out CuBit.Messages.Message;Handled:out Boolean) is
  pragma Unreferenced(Server,Request,Read_Allowed);
 begin Reply:=CuBit.Messages.NULL_MESSAGE;Handled:=False;end;
end ACPI_Native_Power_Identities;
