with Interfaces;with ACPI_Requests;
package ACPI_Endpoint is
 Reply_OK:constant Interfaces.Unsigned_32:=16#F000#;
 Reply_Error:constant Interfaces.Unsigned_32:=16#F001#;
 function Encode(R:ACPI_Requests.Response) return ACPI_Requests.Packet;
end ACPI_Endpoint;
