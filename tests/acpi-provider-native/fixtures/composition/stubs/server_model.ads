pragma Ada_2022;
with ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Native_Blocks;
package Server_Model is
 Server : ACPI_Requests.State(2, 12_288, 8_192, 2, 0);
 Config : constant ACPI_Endpoint.Configuration := (10,11);
 Adapter : ACPI_Native_Blocks.State;
 procedure Route(Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet);
 procedure Receiver_Turn;
end Server_Model;
