with ACPI_Requests;with CuBit.Messages;
package ACPI_Native_Power_Entry is
   Blocked_State : Boolean := False;
   function Blocks_Runtime return Boolean is (Blocked_State);
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean;
      Cached_Shutdown_Selected : Boolean := False);
   procedure Begin_Stop;
end ACPI_Native_Power_Entry;
