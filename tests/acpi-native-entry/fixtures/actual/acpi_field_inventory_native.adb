pragma Ada_2022;
package body ACPI_Field_Inventory_Native is
   package Proto renames ACPI_Inventory_Protocol;
   procedure Read_Clock
     (Now : out Proto.Wire.Time_Instant; Available : out Boolean) is
   begin ACPI_Field_Transport.Read_Clock (Now, Available); end Read_Clock;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : Proto.Wire.Time_Instant;
      Item : Proto.Wire.Packet; Reply : out Proto.Wire.Packet; Delivered : out Proto.Wire.Delivery) is
   begin
      ACPI_Field_Transport.Exchange (Transport, Destination, Deadline, Item, Reply, Delivered);
   end Exchange;
end ACPI_Field_Inventory_Native;
