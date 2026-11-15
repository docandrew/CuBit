pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
package ACPI_Native_Events is
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean);
   -- Parked owner only. Observe authenticated Stop without dispatch/AML or
   -- retirement effects. At most one established-ring append attempt per
   -- process lifetime, including when the copied logger sheds the record.
   procedure Note_Retained_Stop (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Owners : String);
end ACPI_Native_Events;
