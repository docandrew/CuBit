pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
package ACPI_Native_Events is
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean);
end ACPI_Native_Events;
