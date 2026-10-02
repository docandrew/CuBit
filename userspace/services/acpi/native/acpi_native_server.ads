with ACPI_Endpoint;
with ACPI_Requests;
with ACPI_Native_Blocks;
with CuBit.Messages;
-- Serialized native IPC runner. The launcher owns Server and Adapter for the
-- process lifetime, including after a fatal return with pending grant cleanup.
-- Config and Provider_Slot must be bound by trusted startup before Run.
package ACPI_Native_Server is
   type Stop_Reason is
     (Invalid_Configuration, Wait_Unavailable, Clock_Unavailable,
      Unexpected_Completion);
   -- Called once before Run. Only kernel-stamped launcher authority may set
   -- configuration. On runtime failure Accepted=False and outputs are unusable.
   procedure Await_Configuration
     (Config : out ACPI_Endpoint.Configuration;
      Provider_Slot : out CuBit.Messages.CapabilitySlot;
      Accepted : out Boolean);
   procedure Run
     (Server : in out ACPI_Requests.State;
      Adapter : in out ACPI_Native_Blocks.State;
      Config : ACPI_Endpoint.Configuration;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Reason : out Stop_Reason);
end ACPI_Native_Server;
