with Endpoint_Test_Boundary;
package body ACPI_Native_Power_Entry is
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean;
      Cached_Shutdown_Selected : Boolean := False) is
      pragma Unreferenced(Server,Request,Runtime_Selected,Cached_Shutdown_Selected);
   begin Endpoint_Test_Boundary.Pass_Through_Calls:=Endpoint_Test_Boundary.Pass_Through_Calls+1;
      Reply:=CuBit.Messages.NULL_MESSAGE;Handled:=False;Reply_Now:=True;end Dispatch;
   procedure Begin_Stop is begin null;end Begin_Stop;
end ACPI_Native_Power_Entry;
