with Endpoint_Test_Boundary;
package body ACPI_Native_Passive_Queries is
   procedure Dispatch(Server:in out ACPI_Requests.State;Sender:CuBit.Messages.Process_ID;
      Packet:CuBit.Messages.Message;Admission_Open:Boolean;
      Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean) is
      pragma Unreferenced(Server,Sender,Packet,Admission_Open);
   begin Endpoint_Test_Boundary.Pass_Through_Calls:=Endpoint_Test_Boundary.Pass_Through_Calls+1;
      Reply:=CuBit.Messages.NULL_MESSAGE;Handled:=False;Reply_Now:=True;end Dispatch;
end ACPI_Native_Passive_Queries;
