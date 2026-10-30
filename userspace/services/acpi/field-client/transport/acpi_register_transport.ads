pragma Ada_2022;
with CuBit.Handles;
with ACPI_Region_Protocol;
with ACPI_Region_Client;
-- Userspace IPC glue. Startup supplies a scoped typed endpoint; this package
-- does not create authority, choose hardware addresses or release backing.
package ACPI_Register_Transport is
   type State is null record;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Packet;
      Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
   package Client is new ACPI_Region_Client
     (State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint, Exchange);
   -- Reads the native clock. Exchange checks it again immediately before IPC.
   -- Callers must obey downward-only synchronous service layering.
   procedure Dispatch
     (IO : in out Client.Context;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Register_Request; Reply : out Client.Result);
end ACPI_Register_Transport;
