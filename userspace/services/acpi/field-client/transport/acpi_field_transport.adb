pragma Ada_2022;
with Interfaces; with ACPI_Native_Startup_Timing;
with ACPI_Inventory_Transport;
with ACPI_Register_Transport;
with AML_Field_Access;
with ACPI_Field_Protocol_Frames;
with ACPI_Field_Protocol_Transport;
package body ACPI_Field_Transport is
   package Proto renames ACPI_Inventory_Protocol;
   package Metadata is new ACPI_Inventory_Transport (AML_Field_Access.Default_Maximum_Registers);
   use type Interfaces.Unsigned_32;
   procedure Read_Clock (Now : out Proto.Wire.Time_Instant; Available : out Boolean) is
   begin Metadata.Read_Clock (Now, Available); end Read_Clock;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : Proto.Wire.Time_Instant; Item : Proto.Wire.Packet;
      Reply : out Proto.Wire.Packet; Delivered : out Proto.Wire.Delivery) is
      pragma Unreferenced (Transport);
      Meta : Metadata.State;
      Scalar : ACPI_Register_Transport.State;
      Timing : ACPI_Native_Startup_Timing.Token;
   begin
      ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Classify (Item.Label),Timing);
      if Item.Label = Proto.Header_Operation or else Item.Label = Proto.Part_Operation then
         Metadata.Exchange (Meta, Destination, Deadline, Item, Reply, Delivered);
      elsif ACPI_Field_Protocol_Frames.Owned_Label (Item.Label) then
         ACPI_Field_Protocol_Transport.Exchange (Destination, Deadline, Item, Reply, Delivered);
      else
         ACPI_Register_Transport.Exchange (Scalar, Destination, Deadline, Item, Reply, Delivered);
      end if;
      ACPI_Native_Startup_Timing.Leave (Timing);
   end Exchange;
end ACPI_Field_Transport;
