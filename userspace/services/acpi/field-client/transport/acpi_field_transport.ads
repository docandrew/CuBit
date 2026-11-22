pragma Ada_2022;
with CuBit.Handles;
with ACPI_Inventory_Protocol;
-- Stateless native transport for inventory metadata and named scalar access.
-- All authority comes from the already-held typed endpoint. No lookup/mint.
package ACPI_Field_Transport is
   type State is null record;
   procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
end ACPI_Field_Transport;
