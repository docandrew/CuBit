pragma Ada_2022;
with CuBit.Messages;with ACPI_Requests;
package ACPI_Native_Power_Identities is
   -- Kernel-stamped observer, copied names only; never saves a reply or runs AML.
   procedure Dispatch(Server:ACPI_Requests.State;Request:CuBit.Messages.Message;Read_Allowed:Boolean;
      Reply:out CuBit.Messages.Message;Handled:out Boolean) with Pre=>ACPI_Requests.Valid(Server);
end ACPI_Native_Power_Identities;
