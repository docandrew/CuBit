package body ACPI_Native_Events is
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean) is
      pragma Unreferenced (Publisher, Item);
   begin Stop := False; Handled := False; end Handle;
end ACPI_Native_Events;
