pragma Ada_2022;
with CuBit.Handles;
with ACPI_Inventory_Client;
with ACPI_Inventory_Protocol;
generic
   Maximum_Registers : Positive;
package ACPI_Inventory_Transport is
   type State is null record;
   procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   package Client is new ACPI_Inventory_Client
     (State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      Read_Clock, Exchange, Maximum_Registers);
   -- Destination is the startup-provisioned, scope-badged backend endpoint.
   -- Calls go down the service layering only. This endpoint is not queried
   -- for new authority or address registration.
   procedure Load
     (Destination : CuBit.Handles.Endpoint;
      Deadline : Client.Wire.Time_Instant; Result : out Client.Snapshot);
end ACPI_Inventory_Transport;
