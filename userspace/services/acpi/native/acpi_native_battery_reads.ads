pragma Ada_2022;
with CuBit.Messages;
with ACPI_Requests;
package ACPI_Native_Battery_Reads is
   Maximum_Pending : constant Positive := 8;
   -- Actual kernel-received message/stamp only. A moved reply suppresses the
   -- normal immediate reply. No Core pointer/handle or AML invocation is saved.
   procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;
      Read_Allowed:Boolean;Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Advance(Server:ACPI_Requests.State;Owner_Quiet,Read_Allowed:Boolean;Did_Work:out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   procedure Begin_Stop;
   function Pending return Boolean;
   function Quiet return Boolean;
end ACPI_Native_Battery_Reads;
