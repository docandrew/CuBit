pragma Ada_2022;
with CuBit.Handles;
with ACPI_Region_Protocol;
-- Native frontend transport for explicitly recognized NVS/lock/data frames.
-- Retained typed endpoint and original absolute IPC deadline only; no lookup,
-- mint, peer replacement or automatic retry. Nothing here resolves addresses.
package ACPI_Field_Protocol_Transport is
   procedure Exchange
     (Destination : CuBit.Handles.Endpoint; Deadline : ACPI_Region_Protocol.Time_Instant;
      Item : ACPI_Region_Protocol.Packet; Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
end ACPI_Field_Protocol_Transport;
