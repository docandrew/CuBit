pragma Ada_2022;
with CuBit.Handles;
with ACPI_Inventory_Protocol;
with ACPI_Field_Inventory_Owner;
with AML_Field_Access;
with ACPI_Field_Transport;
generic
   with procedure Read_Window
     (Now, Deadline : out ACPI_Inventory_Protocol.Wire.Time_Instant);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
package ACPI_Field_Inventory_Native is
   subtype State is ACPI_Field_Transport.State;
   procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   package Owner is new ACPI_Field_Inventory_Owner
     (State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint, Read_Clock,
      Exchange, Read_Window, Maximum_Registers, Maximum_Confirmations);
end ACPI_Field_Inventory_Native;
