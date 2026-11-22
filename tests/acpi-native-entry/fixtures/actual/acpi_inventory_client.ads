pragma Ada_2022;
with Interfaces;
with ACPI_Inventory_Protocol;
generic
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   Maximum_Registers : Positive;
package ACPI_Inventory_Client is
   package Proto renames ACPI_Inventory_Protocol;
   package Wire renames Proto.Wire;
   subtype Register_Count is Natural range 0 .. Maximum_Registers;
   subtype Register_Index is Register_Count range 1 .. Maximum_Registers;
   type Descriptions is array (Register_Index) of Proto.Description;
   type Snapshot is record
      Complete : Boolean := False;
      Revision : Interfaces.Unsigned_64 := 0;
      Count : Register_Count := 0;
      Items : Descriptions;
   end record;
   -- A bounded, one-attempt metadata copy under one finite overall deadline.
   -- The transport must authenticate and correlate replies with the exact
   -- synchronous call; Destination must come from trusted startup. Re-read
   -- the header after all parts; publish nothing on revision/clock/refusal/
   -- delivery/format failure. Never retry automatically or cache partial data.
   -- Even a complete snapshot is descriptive, not authority or current epoch
   -- liveness. Later I/O must pass the backend's independent scalar checks.
   procedure Load
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : Wire.Time_Instant; Result : out Snapshot);
end ACPI_Inventory_Client;
