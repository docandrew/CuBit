with ACPI_Endpoint;
with ACPI_Requests;
with ACPI_Native_Blocks;
with CuBit.Messages;
-- Serialized native IPC runner. The launcher owns Server and Adapter for the
-- process lifetime, including after a fatal return with pending grant cleanup.
-- Config and Provider must be bound by trusted startup before Run.
with CuBit.Handles;
package ACPI_Native_Server is
   type Stop_Reason is
     (Invalid_Configuration, Wait_Unavailable, Clock_Unavailable,
      Unexpected_Completion);
   -- Called once before Run. Only kernel-stamped launcher authority may set
   -- configuration. On runtime failure Accepted=False and outputs are unusable.
   procedure Await_Configuration
     (Config : out ACPI_Endpoint.Configuration;
      Provider : out CuBit.Handles.Endpoint;
      Accepted : out Boolean);
   procedure Run
     (Server : in out ACPI_Requests.State;
      Adapter : in out ACPI_Native_Blocks.State;
      Config : ACPI_Endpoint.Configuration;
      Provider : CuBit.Handles.Endpoint;
      Reason : out Stop_Reason);
end ACPI_Native_Server;
