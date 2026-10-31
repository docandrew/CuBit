with Owner_Test_Control;
package body ACPI_Field_Transport is
   procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean) is
      pragma Unreferenced (Now, Available);
   begin
      Owner_Test_Control.Native_Reads := Owner_Test_Control.Native_Reads + 1;
      raise Program_Error with "absent register endpoint must not sample transport clock";
   end Read_Clock;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery) is
      pragma Unreferenced (Transport, Destination, Deadline, Item, Reply, Delivered);
   begin
      Owner_Test_Control.Native_Reads := Owner_Test_Control.Native_Reads + 1;
      raise Program_Error with "absent register endpoint must not exchange messages";
   end Exchange;
end ACPI_Field_Transport;
