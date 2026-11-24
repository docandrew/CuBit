with ACPI_Native_Cached_Battery;
package body ACPI_Native_Battery_Reads is
 procedure Begin_Stop is null;
 function Pending return Boolean is(False);function Quiet return Boolean is(True);
 procedure Advance(Server:ACPI_Requests.State;Owner_Quiet,Read_Allowed:Boolean;Did_Work:out Boolean) is
  pragma Unreferenced(Server,Owner_Quiet,Read_Allowed);
 begin Did_Work:=False;end;
 procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;Read_Allowed:Boolean;
  Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean) is
 begin
  ACPI_Native_Cached_Battery.Dispatch(Server,Request,Read_Allowed,Reply,Handled);Reply_Now:=True;
 end;
end ACPI_Native_Battery_Reads;
