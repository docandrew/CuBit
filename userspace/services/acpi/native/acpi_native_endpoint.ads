with ACPI_Native_Blocks;
with ACPI_Endpoint;
with ACPI_Requests;
with CuBit.Messages;
-- Mechanical boundary to the real runtime ABI. Request must be received from
-- the kernel; constructing a Message locally does not authenticate its stamp.
package ACPI_Native_Endpoint is
   procedure Dispatch
     (Adapter : in out ACPI_Native_Blocks.State;
      Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Request : CuBit.Messages.Message;
      Reply : out CuBit.Messages.Message);
end ACPI_Native_Endpoint;
