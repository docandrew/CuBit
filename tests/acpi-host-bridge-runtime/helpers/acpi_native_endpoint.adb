with CuBit.Messages;
package body ACPI_Native_Endpoint is
   function Reply_Now return Boolean is (True);
   procedure Dispatch (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
      pragma Unreferenced (Server, From, Request);
   begin Reply := CuBit.Messages.NULL_MESSAGE; end Dispatch;
end ACPI_Native_Endpoint;
